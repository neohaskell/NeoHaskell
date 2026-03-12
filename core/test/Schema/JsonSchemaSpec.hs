module Schema.JsonSchemaSpec (spec) where

import Array qualified
import Basics
import Json qualified
import Schema (FieldSchema (..), Schema (..))
import Schema.JsonSchema qualified
import Test (Spec)
import Test qualified
import Text (Text)


spec :: Spec Unit
spec = do
  Test.describe "Schema.JsonSchema" do
    Test.describe "toJsonSchema" do
      Test.it "converts SNull to null type" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema SNull
        result
          |> Test.shouldBe
            (Json.object [("type", Json.toJSON ("null" :: Text))])

      Test.it "converts SBool to boolean type" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema SBool
        result
          |> Test.shouldBe
            (Json.object [("type", Json.toJSON ("boolean" :: Text))])

      Test.it "converts SInt to integer type" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema SInt
        result
          |> Test.shouldBe
            (Json.object [("type", Json.toJSON ("integer" :: Text))])

      Test.it "converts SNumber to number type" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema SNumber
        result
          |> Test.shouldBe
            (Json.object [("type", Json.toJSON ("number" :: Text))])

      Test.it "converts SText to string type" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema SText
        result
          |> Test.shouldBe
            (Json.object [("type", Json.toJSON ("string" :: Text))])

      Test.it "converts SArray SText to array with string items" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SArray SText)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("array" :: Text))
                , ("items", Json.object [("type", Json.toJSON ("string" :: Text))])
                ]
        result |> Test.shouldBe expected

      Test.it "SOptional unwrap is transparent - matches inner schema" \_ -> do
        let resultOptional = Schema.JsonSchema.toJsonSchema (SOptional SInt)
        let resultInt = Schema.JsonSchema.toJsonSchema SInt
        resultOptional |> Test.shouldBe resultInt

      Test.it "empty object has empty properties and required" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SObject Array.empty)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [])
                , ("required", Json.toJSON ([] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "single required field appears in both properties and required" \_ -> do
        let fields = Array.fromLinkedList [FieldSchema "x" SText True ""]
        let result = Schema.JsonSchema.toJsonSchema (SObject fields)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("x", Json.object [("type", Json.toJSON ("string" :: Text))])])
                , ("required", Json.toJSON (["x"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "single optional field appears in properties but not required" \_ -> do
        let fields = Array.fromLinkedList [FieldSchema "x" (SOptional SText) False ""]
        let result = Schema.JsonSchema.toJsonSchema (SObject fields)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("x", Json.object [("type", Json.toJSON ("string" :: Text))])])
                , ("required", Json.toJSON ([] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "mixed required and optional: required has only required fields" \_ -> do
        let fields = Array.fromLinkedList
              [ FieldSchema "a" SInt True ""
              , FieldSchema "b" (SOptional SText) False ""
              ]
        let result = Schema.JsonSchema.toJsonSchema (SObject fields)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object
                    [ ("a", Json.object [("type", Json.toJSON ("integer" :: Text))])
                    , ("b", Json.object [("type", Json.toJSON ("string" :: Text))])
                    ])
                , ("required", Json.toJSON (["a"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "fieldRequired=False without SOptional omits from required" \_ -> do
        let fields = Array.fromLinkedList [FieldSchema "a" SInt False ""]
        let result = Schema.JsonSchema.toJsonSchema (SObject fields)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("a", Json.object [("type", Json.toJSON ("integer" :: Text))])])
                , ("required", Json.toJSON ([] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "nested array-of-object has items with object schema" \_ -> do
        let innerFields = Array.fromLinkedList [FieldSchema "id" SText True ""]
        let result = Schema.JsonSchema.toJsonSchema (SArray (SObject innerFields))
        let expectedItems =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("id", Json.object [("type", Json.toJSON ("string" :: Text))])])
                , ("required", Json.toJSON (["id"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        let expected =
              Json.object
                [ ("type", Json.toJSON ("array" :: Text))
                , ("items", expectedItems)
                ]
        result |> Test.shouldBe expected

      Test.it "nested object with optional field: child required is empty" \_ -> do
        let childFields = Array.fromLinkedList [FieldSchema "nick" (SOptional SText) False ""]
        let parentFields = Array.fromLinkedList [FieldSchema "child" (SObject childFields) True ""]
        let result = Schema.JsonSchema.toJsonSchema (SObject parentFields)
        let childSchema =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("nick", Json.object [("type", Json.toJSON ("string" :: Text))])])
                , ("required", Json.toJSON ([] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        let expected =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("child", childSchema)])
                , ("required", Json.toJSON (["child"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        result |> Test.shouldBe expected

      Test.it "SEnum preserves original casing" \_ -> do
        let variants = Array.fromLinkedList ["Active", "INACTIVE", "pending"]
        let result = Schema.JsonSchema.toJsonSchema (SEnum variants)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("string" :: Text))
                , ("enum", Json.toJSON (["Active", "INACTIVE", "pending"] :: [Text]))
                ]
        result |> Test.shouldBe expected

      Test.it "empty SEnum preserves empty array" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SEnum Array.empty)
        let expected =
              Json.object
                [ ("type", Json.toJSON ("string" :: Text))
                , ("enum", Json.toJSON ([] :: [Text]))
                ]
        result |> Test.shouldBe expected

      Test.it "SUnion emits oneOf with each variant schema in input order" \_ -> do
        let variants = Array.fromLinkedList
              [ ("Circle", SObject (Array.fromLinkedList [FieldSchema "radius" SInt True ""]))
              , ("Square", SObject (Array.fromLinkedList [FieldSchema "side" SInt True ""]))
              ]
        let result = Schema.JsonSchema.toJsonSchema (SUnion variants)
        let circleSchema =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("radius", Json.object [("type", Json.toJSON ("integer" :: Text))])])
                , ("required", Json.toJSON (["radius"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        let squareSchema =
              Json.object
                [ ("type", Json.toJSON ("object" :: Text))
                , ("properties", Json.object [("side", Json.object [("type", Json.toJSON ("integer" :: Text))])])
                , ("required", Json.toJSON (["side"] :: [Text]))
                , ("additionalProperties", Json.toJSON False)
                ]
        let expected =
              Json.object
                [("oneOf", Json.toJSON ([circleSchema, squareSchema] :: [Json.Value]))]
        result |> Test.shouldBe expected

      Test.it "empty SUnion emits oneOf with empty array" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SUnion Array.empty)
        let expected = Json.object [("oneOf", Json.toJSON ([] :: [Json.Value]))]
        result |> Test.shouldBe expected

      Test.it "SRef CartEntity emits correct ref path" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SRef "CartEntity")
        let expected = Json.object [("$ref", Json.toJSON ("#/definitions/CartEntity" :: Text))]
        result |> Test.shouldBe expected

      Test.it "empty SRef name produces ref with empty definition" \_ -> do
        let result = Schema.JsonSchema.toJsonSchema (SRef "")
        let expected = Json.object [("$ref", Json.toJSON ("#/definitions/" :: Text))]
        result |> Test.shouldBe expected