module Schema.OpenApiSpec where

import Array qualified
import Basics
import Core
import Data.OpenApi qualified as OpenApi
import Lens.Micro qualified as Lens
import Map qualified
import Schema (FieldSchema (..), Schema (..))
import Schema.OpenApi qualified as OpenApi
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
    }


-- -----------------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "toOpenApiSchema - Primitive Types" do
    it "converts SNull to OpenApiNull" \_ -> do
      let schema = OpenApi.toOpenApiSchema SNull
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiNull)

    it "converts SBool to OpenApiBoolean" \_ -> do
      let schema = OpenApi.toOpenApiSchema SBool
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiBoolean)

    it "converts SInt to OpenApiInteger" \_ -> do
      let schema = OpenApi.toOpenApiSchema SInt
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiInteger)

    it "converts SNumber to OpenApiNumber" \_ -> do
      let schema = OpenApi.toOpenApiSchema SNumber
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiNumber)

    it "converts SText to OpenApiString" \_ -> do
      let schema = OpenApi.toOpenApiSchema SText
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiString)

  describe "toOpenApiSchema - Container Types" do
    it "converts SArray to OpenApiArray with items" \_ -> do
      let schema = OpenApi.toOpenApiSchema (SArray SText)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiArray)
      -- Verify items are set
      let items = schema |> Lens.view OpenApi.items
      items |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

    it "converts SOptional by unwrapping inner schema" \_ -> do
      let schema = OpenApi.toOpenApiSchema (SOptional SInt)
      let typeValue = schema |> Lens.view OpenApi.type_
      -- SOptional unwraps to the inner type
      typeValue |> shouldBe (Just OpenApi.OpenApiInteger)

  describe "toOpenApiSchema - Object Types" do
    it "converts SObject with required fields" \_ -> do
      let fields = Array.fromList
            [ FieldSchema "name" SText True "User name"
            , FieldSchema "age" SInt True "User age"
            ]
      let schema = OpenApi.toOpenApiSchema (SObject fields)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiObject)
      -- Verify properties are set
      let properties = schema |> Lens.view OpenApi.properties
      Map.length properties |> shouldBe 2
      -- Verify required fields
      let required = schema |> Lens.view OpenApi.required
      Array.length required |> shouldBe 2

    it "converts SObject with optional fields correctly" \_ -> do
      let fields = Array.fromList
            [ FieldSchema "name" SText True "User name"
            , FieldSchema "email" (SOptional SText) False "Optional email"
            ]
      let schema = OpenApi.toOpenApiSchema (SObject fields)
      -- Verify only required field is in required list
      let required = schema |> Lens.view OpenApi.required
      Array.length required |> shouldBe 1
      Array.head required |> shouldBe (Just "name")

  describe "toOpenApiSchema - Enum Types" do
    it "converts SEnum to string with enum values" \_ -> do
      let variants = Array.fromList ["Active", "Inactive", "Pending"]
      let schema = OpenApi.toOpenApiSchema (SEnum variants)
      let typeValue = schema |> Lens.view OpenApi.type_
      typeValue |> shouldBe (Just OpenApi.OpenApiString)
      -- Verify enum values are set
      let enumValues = schema |> Lens.view OpenApi.enum_
      enumValues |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

  describe "toOpenApiSchema - Union Types" do
    it "converts SUnion to oneOf" \_ -> do
      let variants = Array.fromList
            [ ("Circle", SObject (Array.fromList [FieldSchema "radius" SInt True "Circle radius"]))
            , ("Rectangle", SObject (Array.fromList [FieldSchema "width" SInt True "Width", FieldSchema "height" SInt True "Height"]))
            ]
      let schema = OpenApi.toOpenApiSchema (SUnion variants)
      -- Verify oneOf is set
      let oneOf = schema |> Lens.view OpenApi.oneOf
      oneOf |> shouldSatisfy (\x -> case x of { Just _ -> True; Nothing -> False })

  describe "toOpenApiSchema - Reference Types" do
    it "converts SRef to reference" \_ -> do
      let schema = OpenApi.toOpenApiSchema (SRef "UserSchema")
      let ref = schema |> Lens.view OpenApi.ref
      ref |> shouldBe (Just (OpenApi.Reference "UserSchema"))

  describe "toOpenApiSpec - Full Spec Generation" do
    it "generates valid OpenAPI spec with info section" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" "A test API"
      let commandSchemas = Map.empty
      let querySchemas = Map.empty
      let spec = OpenApi.toOpenApiSpec apiInfo commandSchemas querySchemas
      -- Verify OpenAPI version
      let version = spec |> Lens.view OpenApi.openapi
      version |> shouldBe "3.0.0"
      -- Verify info section
      let info = spec |> Lens.view OpenApi.info
      let title = info |> Lens.view OpenApi.title
      title |> shouldBe "Test API"
      let apiVersion = info |> Lens.view OpenApi.version
      apiVersion |> shouldBe "1.0.0"

    it "generates paths for commands" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromList
            [ ("CreateUser", makeEndpointSchema (Just (SObject (Array.fromList [FieldSchema "name" SText True ""]))) SText)
            ]
      let querySchemas = Map.empty
      let spec = OpenApi.toOpenApiSpec apiInfo commandSchemas querySchemas
      -- Verify paths are generated
      let OpenApi.Paths paths = spec |> Lens.view OpenApi.paths
      Map.length paths |> shouldSatisfy (\n -> n > 0)

    it "generates paths for queries" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.empty
      let querySchemas = Map.fromList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ]
      let spec = OpenApi.toOpenApiSpec apiInfo commandSchemas querySchemas
      -- Verify paths are generated
      let OpenApi.Paths paths = spec |> Lens.view OpenApi.paths
      Map.length paths |> shouldSatisfy (\n -> n > 0)

    it "combines command and query paths" \_ -> do
      let apiInfo = ApiInfo "Test API" "1.0.0" ""
      let commandSchemas = Map.fromList
            [ ("CreateUser", makeEndpointSchema (Just SText) SText)
            ]
      let querySchemas = Map.fromList
            [ ("GetUser", makeEndpointSchema Nothing SText)
            ]
      let spec = OpenApi.toOpenApiSpec apiInfo commandSchemas querySchemas
      -- Verify both paths are present
      let OpenApi.Paths paths = spec |> Lens.view OpenApi.paths
      Map.length paths |> shouldBe 2
