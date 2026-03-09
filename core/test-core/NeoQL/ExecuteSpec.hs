module NeoQL.ExecuteSpec where

import Array (Array)
import Array qualified
import Basics
import NeoQL (Expr (..), FieldName (..), Value (..))
import NeoQL qualified
import Json qualified
import Result (Result (..))
import Result qualified
import Test.Hspec qualified as Hspec
import Text (Text)


decodeFixtureResult :: Text -> Result Text Json.Value
decodeFixtureResult source =
  Json.decodeText source


decodeFixture :: Text -> Json.Value
decodeFixture source = do
  let decoded = decodeFixtureResult source
  case decoded of
    Result.Ok value -> value
    Result.Err _ -> Json.null


spec :: Hspec.Spec
spec = do
  Hspec.describe "NeoQL.Execute" do
    Hspec.describe "string equality" do
      Hspec.it "matches when field equals string value" do
        let decoded = decodeFixtureResult "{\"status\":\"pending\"}"
        decoded `Hspec.shouldSatisfy` Result.isOk

        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"status\":\"pending\"}"
        NeoQL.execute expr value `Hspec.shouldBe` True

      Hspec.it "does not match when field differs" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"status\":\"paid\"}"
        NeoQL.execute expr value `Hspec.shouldBe` False

    Hspec.describe "number equality" do
      Hspec.it "matches when field equals number value" do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":42}"
        NeoQL.execute expr value `Hspec.shouldBe` True

      Hspec.it "does not match when field differs" do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":41}"
        NeoQL.execute expr value `Hspec.shouldBe` False

    Hspec.describe "unknown field" do
      Hspec.it "returns False for unknown field (silent non-match)" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"other\":\"pending\"}"
        NeoQL.execute expr value `Hspec.shouldBe` False

    Hspec.describe "type mismatch" do
      Hspec.it "returns False when field is string but expr expects number" do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":\"42\"}"
        NeoQL.execute expr value `Hspec.shouldBe` False

    Hspec.describe "field access (no filter)" do
      Hspec.it "returns True for FieldAccess (projection, no filtering)" do
        let expr = FieldAccess (FieldName "status")
        let value = decodeFixture "{\"status\":\"pending\"}"
        NeoQL.execute expr value `Hspec.shouldBe` True

    Hspec.describe "non-object values" do
      Hspec.it "returns False for non-object Aeson.Value" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let stringValue = decodeFixture "\"pending\""
        let numberValue = decodeFixture "42"
        let boolValue = decodeFixture "true"
        let arrayValue = decodeFixture "[{\"status\":\"pending\"}]"
        let nullValue = decodeFixture "null"

        NeoQL.execute expr stringValue `Hspec.shouldBe` False
        NeoQL.execute expr numberValue `Hspec.shouldBe` False
        NeoQL.execute expr boolValue `Hspec.shouldBe` False
        NeoQL.execute expr arrayValue `Hspec.shouldBe` False
        NeoQL.execute expr nullValue `Hspec.shouldBe` False

    Hspec.describe "filterValues" do
      Hspec.it "filters array keeping only matching objects" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let pendingA = decodeFixture "{\"status\":\"pending\",\"id\":1}"
        let paid = decodeFixture "{\"status\":\"paid\",\"id\":2}"
        let pendingB = decodeFixture "{\"status\":\"pending\",\"id\":3}"
        let nonObject = decodeFixture "\"pending\""
        let values = Array.fromLinkedList [pendingA, paid, pendingB, nonObject]
        let expected = Array.fromLinkedList [pendingA, pendingB]
        NeoQL.filterValues expr values `Hspec.shouldBe` expected

      Hspec.it "returns empty array for empty input" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let values = Array.empty :: Array Json.Value
        NeoQL.filterValues expr values `Hspec.shouldBe` Array.empty

      Hspec.it "returns empty array when nothing matches" do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let paidA = decodeFixture "{\"status\":\"paid\"}"
        let paidB = decodeFixture "{\"status\":\"cancelled\"}"
        let values = Array.fromLinkedList [paidA, paidB]
        NeoQL.filterValues expr values `Hspec.shouldBe` Array.empty
