module NeoQL.ExecuteSpec where

import Array qualified
import Core
import Json qualified
import NeoQL (Expr (..), FieldName (..), Value (..))
import NeoQL qualified
import Result (Result (..))
import Result qualified
import Test


decodeFixtureResult :: Text -> Result Text Json.Value
decodeFixtureResult source =
  Json.decodeText source


decodeFixture :: Text -> Json.Value
decodeFixture source = do
  let decoded = decodeFixtureResult source
  case decoded of
    Result.Ok value -> value
    Result.Err err -> panic [fmt|Invalid JSON fixture: #{err}|]


spec :: Spec Unit
spec = do
  describe "NeoQL.Execute" do
    describe "string equality" do
      it "matches when field equals string value" \_ -> do
        let decoded = decodeFixtureResult "{\"status\":\"pending\"}"
        decoded |> shouldSatisfy Result.isOk

        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"status\":\"pending\"}"
        NeoQL.execute expr value |> shouldBe True

      it "does not match when field differs" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"status\":\"paid\"}"
        NeoQL.execute expr value |> shouldBe False

    describe "number equality" do
      it "matches when field equals number value" \_ -> do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":42}"
        NeoQL.execute expr value |> shouldBe True

      it "does not match when field differs" \_ -> do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":41}"
        NeoQL.execute expr value |> shouldBe False

    describe "unknown field" do
      it "returns False for unknown field (silent non-match)" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let value = decodeFixture "{\"other\":\"pending\"}"
        NeoQL.execute expr value |> shouldBe False

    describe "type mismatch" do
      it "returns False when field is string but expr expects number" \_ -> do
        let expr = FieldEquals (FieldName "count") (NumberValue 42)
        let value = decodeFixture "{\"count\":\"42\"}"
        NeoQL.execute expr value |> shouldBe False

    describe "field access (no filter)" do
      it "returns True for FieldAccess (projection, no filtering)" \_ -> do
        let expr = FieldAccess (FieldName "status")
        let value = decodeFixture "{\"status\":\"pending\"}"
        NeoQL.execute expr value |> shouldBe True

    describe "non-object values" do
      it "returns False for non-object Aeson.Value" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let stringValue = decodeFixture "\"pending\""
        let numberValue = decodeFixture "42"
        let boolValue = decodeFixture "true"
        let arrayValue = decodeFixture "[{\"status\":\"pending\"}]"
        let nullValue = decodeFixture "null"

        NeoQL.execute expr stringValue |> shouldBe False
        NeoQL.execute expr numberValue |> shouldBe False
        NeoQL.execute expr boolValue |> shouldBe False
        NeoQL.execute expr arrayValue |> shouldBe False
        NeoQL.execute expr nullValue |> shouldBe False

    describe "filterValues" do
      it "filters array keeping only matching objects" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let pendingA = decodeFixture "{\"status\":\"pending\",\"id\":1}"
        let paid = decodeFixture "{\"status\":\"paid\",\"id\":2}"
        let pendingB = decodeFixture "{\"status\":\"pending\",\"id\":3}"
        let nonObject = decodeFixture "\"pending\""
        let values = Array.fromLinkedList [pendingA, paid, pendingB, nonObject]
        let expected = Array.fromLinkedList [pendingA, pendingB]
        NeoQL.filterValues expr values |> shouldBe expected

      it "returns empty array for empty input" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let values = Array.empty :: Array Json.Value
        NeoQL.filterValues expr values |> shouldBe Array.empty

      it "returns empty array when nothing matches" \_ -> do
        let expr = FieldEquals (FieldName "status") (StringValue "pending")
        let paidA = decodeFixture "{\"status\":\"paid\"}"
        let paidB = decodeFixture "{\"status\":\"cancelled\"}"
        let values = Array.fromLinkedList [paidA, paidB]
        NeoQL.filterValues expr values |> shouldBe Array.empty
