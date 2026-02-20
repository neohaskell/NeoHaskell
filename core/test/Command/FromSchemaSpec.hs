module Command.FromSchemaSpec where

import Array qualified
import Command qualified
import Core
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Command.fromSchema" do
    it "constructs parser for SText" \_ -> do
      let parser = Command.fromSchema SText
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for SInt" \_ -> do
      let parser = Command.fromSchema SInt
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for SNumber" \_ -> do
      let parser = Command.fromSchema SNumber
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for SBool" \_ -> do
      let parser = Command.fromSchema SBool
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for empty object" \_ -> do
      let parser = Command.fromSchema (SObject Array.empty)
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for object with multiple fields" \_ -> do
      let fields = Array.fromLinkedList
            [ FieldSchema
                { fieldName = "name"
                , fieldSchema = SText
                , fieldRequired = True
                , fieldDescription = "Name"
                }
            , FieldSchema
                { fieldName = "age"
                , fieldSchema = SInt
                , fieldRequired = True
                , fieldDescription = "Age"
                }
            ]
      let parser = Command.fromSchema (SObject fields)
      let parserStr = show parser |> Text.fromLinkedList
      parserStr |> shouldSatisfy (Text.contains "OptionsParser")

    it "constructs parser for catch-all schema variants" \_ -> do
      -- SOptional, SArray, SEnum, SUnion, SRef all hit the catch-all case
      let parserOptional = Command.fromSchema (SOptional SText)
      let parserArray = Command.fromSchema (SArray SInt)
      let parserEnum = Command.fromSchema (SEnum (Array.fromLinkedList ["a", "b"]))
      let parserRef = Command.fromSchema (SRef "SomeType")
      let parserNull = Command.fromSchema SNull
      Text.length (show parserOptional |> Text.fromLinkedList) |> shouldSatisfy (\n -> n > 0)
      Text.length (show parserArray |> Text.fromLinkedList) |> shouldSatisfy (\n -> n > 0)
      Text.length (show parserEnum |> Text.fromLinkedList) |> shouldSatisfy (\n -> n > 0)
      Text.length (show parserRef |> Text.fromLinkedList) |> shouldSatisfy (\n -> n > 0)
      Text.length (show parserNull |> Text.fromLinkedList) |> shouldSatisfy (\n -> n > 0)

  describe "Command.fromFieldSchema" do
    it "constructs parser for text field" \_ -> do
      let field = FieldSchema
            { fieldName = "name"
            , fieldSchema = SText
            , fieldRequired = True
            , fieldDescription = "A name"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for int field" \_ -> do
      let field = FieldSchema
            { fieldName = "count"
            , fieldSchema = SInt
            , fieldRequired = True
            , fieldDescription = "A count"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for number field" \_ -> do
      let field = FieldSchema
            { fieldName = "price"
            , fieldSchema = SNumber
            , fieldRequired = True
            , fieldDescription = "A price"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for bool field" \_ -> do
      let field = FieldSchema
            { fieldName = "active"
            , fieldSchema = SBool
            , fieldRequired = True
            , fieldDescription = "Is active"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for optional field" \_ -> do
      let field = FieldSchema
            { fieldName = "nickname"
            , fieldSchema = SOptional SText
            , fieldRequired = False
            , fieldDescription = "Optional name"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for enum field" \_ -> do
      let field = FieldSchema
            { fieldName = "color"
            , fieldSchema = SEnum (Array.fromLinkedList ["red", "green", "blue"])
            , fieldRequired = True
            , fieldDescription = "Color"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for array field" \_ -> do
      let field = FieldSchema
            { fieldName = "tags"
            , fieldSchema = SArray SText
            , fieldRequired = True
            , fieldDescription = "Tags"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)

    it "constructs parser for fallback field schema" \_ -> do
      let field = FieldSchema
            { fieldName = "ref"
            , fieldSchema = SRef "OtherType"
            , fieldRequired = True
            , fieldDescription = "A reference"
            }
      let parser = Command.fromFieldSchema field
      let parserStr = show parser |> Text.fromLinkedList
      Text.length parserStr |> shouldSatisfy (\n -> n > 0)
