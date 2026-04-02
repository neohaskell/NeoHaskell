module Schema.OpenApiSpec where

import Array qualified
import Basics
import Control.Lens qualified as Lens
import Core
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.HashSet.InsOrd qualified as InsOrdHashSet
import Data.OpenApi qualified as OpenApi
import LinkedList qualified
import Map qualified
import Schema.OpenApi qualified as OpenApiGen
import Service.Application (ApiInfo (..))
import Service.Transport (EndpointSchema (..))
import Test


-- -----------------------------------------------------------------------------
-- Test Helpers
-- -----------------------------------------------------------------------------

-- | Helper to create a simple EndpointSchema for testing
makeEndpointSchema :: Maybe Schema -> Schema -> EndpointSchema
makeEndpointSchema reqSchema respSchema = do
  EndpointSchema
    { requestSchema = reqSchema
    , responseSchema = respSchema
    , description = "Test endpoint"
    , deprecated = False
    , entityName = Nothing
    }


-- | Extract the inline response schema from an OpenAPI spec at a given path and HTTP method.
-- Fails with descriptive messages if any step in the traversal is missing.
extractResponseSchema ::
  [Char] ->
  Lens.Getting (Maybe OpenApi.Operation) OpenApi.PathItem (Maybe OpenApi.Operation) ->
  OpenApi.OpenApi ->
  (OpenApi.Schema -> Task Text Unit) ->
  Task Text Unit
extractResponseSchema path operationLens openApiSpec assertion = do
  let paths = openApiSpec |> Lens.view OpenApi.paths
  let maybePathItem = InsOrdHashMap.lookup path paths
  case maybePathItem of
    Nothing -> Test.fail "Expected path not found in OpenAPI spec"
    Just pathItem -> do
      let maybeOp = pathItem |> Lens.view operationLens
      case maybeOp of
        Nothing -> Test.fail "Expected HTTP operation not found"
        Just op -> do
          let responses = op |> Lens.view OpenApi.responses
          let responsesMap = responses |> Lens.view OpenApi.responses
          let maybeResponse = InsOrdHashMap.lookup 200 responsesMap
          case maybeResponse of
            Nothing -> Test.fail "200 response not found"
            Just (OpenApi.Inline response) -> do
              let content = response |> Lens.view OpenApi.content
              let maybeMediaType = InsOrdHashMap.lookup "application/json" content
              case maybeMediaType of
                Nothing -> Test.fail "application/json media type not found"
                Just mediaType -> do
                  let maybeSchema = mediaType |> Lens.view OpenApi.schema
                  case maybeSchema of
                    Nothing -> Test.fail "Schema not found"
                    Just (OpenApi.Inline schema) -> assertion schema
                    Just (OpenApi.Ref _) -> Test.fail "Schema is a reference, expected inline"
            Just (OpenApi.Ref _) -> Test.fail "Response is a reference, expected inline"


-- -----------------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "toOpenApiSchema - Primitive Types" do
    it "converts SNull to OpenApiNull" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema SNull
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiNull)

    it "converts SBool to OpenApiBoolean" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema SBool
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiBoolean)

    it "converts SInt to OpenApiInteger" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema SInt
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiInteger)

    it "converts SNumber to OpenApiNumber" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema SNumber
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiNumber)

    it "converts SText to OpenApiString" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema SText
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiString)

  describe "toOpenApiSchema - Container Types" do
    it "converts SArray to OpenApiArray with items" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema (SArray SText)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiArray)
      -- Verify items are set
      let items = schema |> Lens.view OpenApi.items
      items |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

    it "converts SOptional by unwrapping inner schema" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema (SOptional SInt)
      let typeValue = schema |> Lens.view OpenApi.type_
      -- SOptional unwraps to the inner type
      typeValue |> shouldBe (Just OpenApi.OpenApiInteger)

  describe "toOpenApiSchema - Object Types" do
    it "converts SObject with required fields" \_ -> do
      let fields = Array.fromLinkedList
            [ FieldSchema "name" SText True "User name"
            , FieldSchema "age" SInt True "User age"
            ]
      let schema = OpenApiGen.toOpenApiSchema (SObject fields)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiObject)
      -- Verify properties are set
      let properties = schema |> Lens.view OpenApi.properties
      InsOrdHashMap.size properties |> shouldBe 2
      -- Verify required fields
      let required = schema |> Lens.view OpenApi.required
      LinkedList.length required |> shouldBe 2

    it "converts SObject with optional fields correctly" \_ -> do
      let fields = Array.fromLinkedList
            [ FieldSchema "name" SText True "User name"
            , FieldSchema "email" (SOptional SText) False "Optional email"
            ]
      let schema = OpenApiGen.toOpenApiSchema (SObject fields)
      -- Verify only required field is in required list
      let required = schema |> Lens.view OpenApi.required
      LinkedList.length required |> shouldBe 1
      LinkedList.head required |> shouldBe (Just "name")

  describe "toOpenApiSchema - Enum Types" do
    it "converts SEnum to string with enum values" \_ -> do
      let variants = Array.fromLinkedList ["Active", "Inactive", "Pending"]
      let schema = OpenApiGen.toOpenApiSchema (SEnum variants)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiString)
      -- Verify enum values are set
      let enumValues = schema |> Lens.view OpenApi.enum_
      enumValues |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

  describe "toOpenApiSchema - Union Types" do
    it "converts SUnion to oneOf" \_ -> do
      let variants = Array.fromLinkedList
            [ ("Circle", SObject (Array.fromLinkedList [FieldSchema "radius" SInt True "Circle radius"]))
            , ("Rectangle", SObject (Array.fromLinkedList [FieldSchema "width" SInt True "Width", FieldSchema "height" SInt True "Height"]))
            ]
      let schema = OpenApiGen.toOpenApiSchema (SUnion variants)
      -- Verify oneOf is set
      let oneOf = schema |> Lens.view OpenApi.oneOf
      oneOf |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

  describe "toOpenApiSchema - Reference Types" do
    it "converts SRef to schema with title as reference" \_ -> do
      let schema = OpenApiGen.toOpenApiSchema (SRef "UserSchema")
      -- SRef sets the title to the reference name
      let title = schema |> Lens.view OpenApi.title
      title |> shouldBe (Just "UserSchema")

  describe "toOpenApiSpec - Full Spec Generation" do
    it "generates valid OpenAPI spec with info section" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" "A test API"
      let commandSchemas = Map.empty
      let querySchemas = Map.empty
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      -- Verify info section
      let info = openApiSpec |> Lens.view OpenApi.info
      let title = info |> Lens.view OpenApi.title
      title |> shouldBe "Test API"
      let apiVersion = info |> Lens.view OpenApi.version
      apiVersion |> shouldBe "1.0.0"

    it "generates paths for commands" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", makeEndpointSchema (Just (SObject (Array.fromLinkedList [FieldSchema "name" SText True ""]))) SText)
            ])
      let querySchemas = Map.empty
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      -- Verify paths are generated
      let paths = openApiSpec |> Lens.view OpenApi.paths
      InsOrdHashMap.size paths |> shouldSatisfy (\n -> n > 0)

    it "generates paths for queries" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.empty
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      -- Verify paths are generated
      let paths = openApiSpec |> Lens.view OpenApi.paths
      InsOrdHashMap.size paths |> shouldSatisfy (\n -> n > 0)

    it "query endpoint response schema is paginated object" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.empty
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let typeValue = schema |> Lens.view OpenApi.type_
        typeValue |> shouldBe (Just OpenApi.OpenApiObject)
        )

    it "query endpoint response has all four required properties" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let properties = schema |> Lens.view OpenApi.properties
        InsOrdHashMap.size properties |> shouldBe 4
        InsOrdHashMap.member "items" properties |> shouldBe True
        InsOrdHashMap.member "total" properties |> shouldBe True
        InsOrdHashMap.member "hasMore" properties |> shouldBe True
        InsOrdHashMap.member "effectiveLimit" properties |> shouldBe True
        )

    it "query endpoint response requires all four properties" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let required = schema |> Lens.view OpenApi.required
        LinkedList.length required |> shouldBe 4
        )

    it "query items property is array of item schema" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let properties = schema |> Lens.view OpenApi.properties
        case InsOrdHashMap.lookup "items" properties of
          Nothing -> Test.fail "items property not found"
          Just (OpenApi.Inline itemsSchema) -> do
            let typeValue = itemsSchema |> Lens.view OpenApi.type_
            typeValue |> shouldBe (Just OpenApi.OpenApiArray)
            let items = itemsSchema |> Lens.view OpenApi.items
            items |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })
          Just (OpenApi.Ref _) -> Test.fail "items is a reference, expected inline"
        )

    it "query total property is integer" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let properties = schema |> Lens.view OpenApi.properties
        case InsOrdHashMap.lookup "total" properties of
          Nothing -> Test.fail "total property not found"
          Just (OpenApi.Inline totalSchema) -> do
            let typeValue = totalSchema |> Lens.view OpenApi.type_
            typeValue |> shouldBe (Just OpenApi.OpenApiInteger)
          Just (OpenApi.Ref _) -> Test.fail "total is a reference, expected inline"
        )

    it "query hasMore property is boolean" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let properties = schema |> Lens.view OpenApi.properties
        case InsOrdHashMap.lookup "hasMore" properties of
          Nothing -> Test.fail "hasMore property not found"
          Just (OpenApi.Inline hasMoreSchema) -> do
            let typeValue = hasMoreSchema |> Lens.view OpenApi.type_
            typeValue |> shouldBe (Just OpenApi.OpenApiBoolean)
          Just (OpenApi.Ref _) -> Test.fail "hasMore is a reference, expected inline"
        )

    it "query effectiveLimit property is integer" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let properties = schema |> Lens.view OpenApi.properties
        case InsOrdHashMap.lookup "effectiveLimit" properties of
          Nothing -> Test.fail "effectiveLimit property not found"
          Just (OpenApi.Inline elSchema) -> do
            let typeValue = elSchema |> Lens.view OpenApi.type_
            typeValue |> shouldBe (Just OpenApi.OpenApiInteger)
          Just (OpenApi.Ref _) -> Test.fail "effectiveLimit is a reference, expected inline"
        )

    it "command endpoint response schema is singular (not array)" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", makeEndpointSchema (Just (SObject (Array.fromLinkedList [FieldSchema "name" SText True ""]))) SText)
            ])
      let querySchemas = Map.empty
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      extractResponseSchema "/commands/create-user" OpenApi.post openApiSpec (\schema -> do
        let typeValue = schema |> Lens.view OpenApi.type_
        typeValue |> shouldBe (Just OpenApi.OpenApiString)
        )

    it "combines command and query paths" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", makeEndpointSchema (Just SText) SText)
            ])
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas Map.empty Map.empty
      -- Verify both paths are present
      let paths = openApiSpec |> Lens.view OpenApi.paths
      InsOrdHashMap.size paths |> shouldBe 2

  describe "toOpenApiSpec - Tag Generation" do
    it "generates tags for unique entity names from commands" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Create user"
                , deprecated = False
                , entityName = Just "User"
                })
            , ("UpdateUser", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Update user"
                , deprecated = False
                , entityName = Just "User"  -- Same entity, should not duplicate tag
                })
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas Map.empty Map.empty Map.empty
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have exactly 1 tag for User (deduplicated)
      InsOrdHashSet.size tags |> shouldBe 1

    it "generates tags for multiple different entities" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Create user"
                , deprecated = False
                , entityName = Just "User"
                })
            , ("CreateOrder", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Create order"
                , deprecated = False
                , entityName = Just "Order"
                })
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas Map.empty Map.empty Map.empty
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have 2 tags: User and Order
      InsOrdHashSet.size tags |> shouldBe 2

    it "generates Queries tag when query schemas present" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have exactly 1 tag: Queries
      InsOrdHashSet.size tags |> shouldBe 1

    it "generates Authentication tag when oauth2 schemas present" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let oauth2Schemas = Map.fromArray (Array.fromLinkedList
            [ ("connect", EndpointSchema
                { requestSchema = Nothing
                , responseSchema = SText
                , description = "Connect OAuth2"
                , deprecated = False
                , entityName = Just "Authentication"
                })
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty Map.empty oauth2Schemas Map.empty
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have exactly 1 tag: Authentication
      InsOrdHashSet.size tags |> shouldBe 1

    it "generates Files tag when file schemas present" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let fileSchemas = Map.fromArray (Array.fromLinkedList
            [ ("upload", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Upload file"
                , deprecated = False
                , entityName = Just "Files"
                })
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty Map.empty Map.empty fileSchemas
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have exactly 1 tag: Files
      InsOrdHashSet.size tags |> shouldBe 1

    it "generates all tags when all schema types present" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromArray (Array.fromLinkedList
            [ ("CreateUser", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Create user"
                , deprecated = False
                , entityName = Just "User"
                })
            ])
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let oauth2Schemas = Map.fromArray (Array.fromLinkedList
            [ ("connect", EndpointSchema
                { requestSchema = Nothing
                , responseSchema = SText
                , description = "Connect OAuth2"
                , deprecated = False
                , entityName = Just "Authentication"
                })
            ])
      let fileSchemas = Map.fromArray (Array.fromLinkedList
            [ ("upload", EndpointSchema
                { requestSchema = Just SText
                , responseSchema = SText
                , description = "Upload file"
                , deprecated = False
                , entityName = Just "Files"
                })
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo commandSchemas querySchemas oauth2Schemas fileSchemas
      let tags = openApiSpec |> Lens.view OpenApi.tags
      -- Should have 4 tags: User, Queries, Authentication, Files
      InsOrdHashSet.size tags |> shouldBe 4

  describe "toOpenApiSpec - Query Parameters" do
    it "query endpoint has three query parameters" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      let paths = openApiSpec |> Lens.view OpenApi.paths
      case InsOrdHashMap.lookup "/queries/get-user" paths of
        Nothing -> Test.fail "Expected path not found"
        Just pathItem -> do
          case pathItem |> Lens.view OpenApi.get of
            Nothing -> Test.fail "GET operation not found"
            Just op -> do
              let params = op |> Lens.view OpenApi.parameters
              LinkedList.length params |> shouldBe 3

    it "query endpoint has limit parameter" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      let paths = openApiSpec |> Lens.view OpenApi.paths
      case InsOrdHashMap.lookup "/queries/get-user" paths of
        Nothing -> Test.fail "Expected path not found"
        Just pathItem -> do
          case pathItem |> Lens.view OpenApi.get of
            Nothing -> Test.fail "GET operation not found"
            Just op -> do
              let params = op |> Lens.view OpenApi.parameters
              let hasLimit = params |> LinkedList.any (\ref -> case ref of
                    OpenApi.Inline param -> (param |> Lens.view OpenApi.name) == "limit"
                    _ -> False)
              hasLimit |> shouldBe True

    it "query endpoint has offset parameter" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      let paths = openApiSpec |> Lens.view OpenApi.paths
      case InsOrdHashMap.lookup "/queries/get-user" paths of
        Nothing -> Test.fail "Expected path not found"
        Just pathItem -> do
          case pathItem |> Lens.view OpenApi.get of
            Nothing -> Test.fail "GET operation not found"
            Just op -> do
              let params = op |> Lens.view OpenApi.parameters
              let hasOffset = params |> LinkedList.any (\ref -> case ref of
                    OpenApi.Inline param -> (param |> Lens.view OpenApi.name) == "offset"
                    _ -> False)
              hasOffset |> shouldBe True

    it "query endpoint has q parameter" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      let paths = openApiSpec |> Lens.view OpenApi.paths
      case InsOrdHashMap.lookup "/queries/get-user" paths of
        Nothing -> Test.fail "Expected path not found"
        Just pathItem -> do
          case pathItem |> Lens.view OpenApi.get of
            Nothing -> Test.fail "GET operation not found"
            Just op -> do
              let params = op |> Lens.view OpenApi.parameters
              let hasQ = params |> LinkedList.any (\ref -> case ref of
                    OpenApi.Inline param -> (param |> Lens.view OpenApi.name) == "q"
                    _ -> False)
              hasQ |> shouldBe True

  describe "toOpenApiSpec - Query Edge Cases" do
    it "query with complex object item schema wraps correctly in envelope" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let fields = Array.fromLinkedList
            [ FieldSchema "name" SText True "User name"
            , FieldSchema "age" SInt True "User age"
            ]
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing (SObject fields))
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let typeValue = schema |> Lens.view OpenApi.type_
        typeValue |> shouldBe (Just OpenApi.OpenApiObject)
        let properties = schema |> Lens.view OpenApi.properties
        case InsOrdHashMap.lookup "items" properties of
          Nothing -> Test.fail "items property not found"
          Just (OpenApi.Inline itemsSchema) -> do
            let itemsType = itemsSchema |> Lens.view OpenApi.type_
            itemsType |> shouldBe (Just OpenApi.OpenApiArray)
          Just (OpenApi.Ref _) -> Test.fail "items is a reference, expected inline"
        )

    it "multiple query endpoints each get paginated schema" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let querySchemas = Map.fromArray (Array.fromLinkedList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            , ("GetOrder", makeEndpointSchema Nothing SInt)
            ])
      let openApiSpec = OpenApiGen.toOpenApiSpec apiInfo Map.empty querySchemas Map.empty Map.empty
      extractResponseSchema "/queries/get-user" OpenApi.get openApiSpec (\schema -> do
        let typeValue = schema |> Lens.view OpenApi.type_
        typeValue |> shouldBe (Just OpenApi.OpenApiObject)
        )
      extractResponseSchema "/queries/get-order" OpenApi.get openApiSpec (\schema -> do
        let typeValue = schema |> Lens.view OpenApi.type_
        typeValue |> shouldBe (Just OpenApi.OpenApiObject)
        )
