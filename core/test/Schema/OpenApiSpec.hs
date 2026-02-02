module Schema.OpenApiSpec where

import Array qualified
import Basics
import Control.Lens qualified as Lens
import Core
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
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
