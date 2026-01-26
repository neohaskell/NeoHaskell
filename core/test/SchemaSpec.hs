module SchemaSpec where

import Array qualified
import Basics
import Core
import Test


-- -----------------------------------------------------------------------------
-- Test Types
-- -----------------------------------------------------------------------------

-- | Simple record for testing
data Person = Person
  { name :: Text
  , age :: Int
  }
  deriving (Generic)


instance ToSchema Person


-- | Record with optional field
data PersonWithEmail = PersonWithEmail
  { personName :: Text
  , email :: Maybe Text
  }
  deriving (Generic)


instance ToSchema PersonWithEmail


-- | Sum type with nullary constructors (enum)
data Status
  = Active
  | Inactive
  | Pending
  deriving (Generic)


instance ToSchema Status


-- | Sum type with fields (union)
data Shape
  = Circle {radius :: Int}
  | Rectangle {width :: Int, height :: Int}
  deriving (Generic)


instance ToSchema Shape


-- | Nested type for testing
data ShoppingOrder = ShoppingOrder
  { orderId :: Text
  , items :: Array Text
  , status :: Status
  }
  deriving (Generic)


instance ToSchema ShoppingOrder


-- -----------------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "ToSchema primitive instances" do
    it "generates SBool for Bool" \_ -> do
      toSchema @Bool |> shouldBe SBool

    it "generates SInt for Int" \_ -> do
      toSchema @Int |> shouldBe SInt

    it "generates SNumber for Float" \_ -> do
      toSchema @Float |> shouldBe SNumber

    it "generates SText for Text" \_ -> do
      toSchema @Text |> shouldBe SText

  describe "ToSchema container instances" do
    it "generates SArray for Array" \_ -> do
      toSchema @(Array Int) |> shouldBe (SArray SInt)

    it "generates SArray for list" \_ -> do
      toSchema @[Text] |> shouldBe (SArray SText)

    it "generates SOptional for Maybe" \_ -> do
      toSchema @(Maybe Int) |> shouldBe (SOptional SInt)

    it "generates nested SArray SOptional" \_ -> do
      toSchema @(Array (Maybe Text)) |> shouldBe (SArray (SOptional SText))

  describe "ToSchema for records" do
    it "generates SObject for simple record" \_ -> do
      let schema = toSchema @Person
      case schema of
        SObject fields -> do
          Array.length fields |> shouldBe 2
          -- Check that both expected fields exist (order may vary)
          let fieldNames = fields |> Array.map (\f -> f.fieldName)
          Array.contains "name" fieldNames |> shouldBe True
          Array.contains "age" fieldNames |> shouldBe True
          -- Find and check each field
          let nameField = fields |> Array.find (\f -> f.fieldName == "name")
          let ageField = fields |> Array.find (\f -> f.fieldName == "age")
          case (nameField, ageField) of
            (Just nf, Just af) -> do
              nf.fieldSchema |> shouldBe SText
              nf.fieldRequired |> shouldBe True
              af.fieldSchema |> shouldBe SInt
              af.fieldRequired |> shouldBe True
            _ -> do
              True |> shouldBe False
        _ -> do
          True |> shouldBe False

    it "marks Maybe fields as non-required" \_ -> do
      let schema = toSchema @PersonWithEmail
      case schema of
        SObject fields -> do
          Array.length fields |> shouldBe 2
          -- Find the email field
          let emailField = fields |> Array.find (\f -> f.fieldName == "email")
          case emailField of
            Just f -> do
              f.fieldRequired |> shouldBe False
              case f.fieldSchema of
                SOptional SText -> do
                  True |> shouldBe True
                _ -> do
                  True |> shouldBe False
            _ -> do
              True |> shouldBe False
        _ -> do
          True |> shouldBe False

  describe "ToSchema for sum types" do
    it "generates SEnum for nullary sum type" \_ -> do
      let schema = toSchema @Status
      case schema of
        SEnum variants -> do
          Array.length variants |> shouldBe 3
          Array.contains "Active" variants |> shouldBe True
          Array.contains "Inactive" variants |> shouldBe True
          Array.contains "Pending" variants |> shouldBe True
        _ -> do
          True |> shouldBe False

    it "generates SUnion for sum type with fields" \_ -> do
      let schema = toSchema @Shape
      case schema of
        SUnion variants -> do
          Array.length variants |> shouldBe 2
          let names = variants |> Array.map (\(n, _) -> n)
          Array.contains "Circle" names |> shouldBe True
          Array.contains "Rectangle" names |> shouldBe True
        _ -> do
          True |> shouldBe False

  describe "ToSchema for nested types" do
    it "generates schema for nested record with array and sum type" \_ -> do
      let schema = toSchema @ShoppingOrder
      case schema of
        SObject fields -> do
          Array.length fields |> shouldBe 3
          -- Find the status field
          let statusField = fields |> Array.find (\f -> f.fieldName == "status")
          case statusField of
            Just f -> do
              -- Status should be embedded as SEnum
              case f.fieldSchema of
                SEnum _ -> do
                  True |> shouldBe True
                _ -> do
                  True |> shouldBe False
            _ -> do
              True |> shouldBe False
        _ -> do
          True |> shouldBe False
