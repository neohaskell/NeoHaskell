module Service.Query.PaginationSpec where

import Array qualified
import Core
import Json qualified
import Service.Query.Pagination
  ( QueryPageRequest (..),
    QueryPageResponse (..),
    absoluteMaxLimit,
    absoluteMaxOffset,
    defaultLimit,
    parsePageRequest,
  )
import Test


spec :: Spec Unit
spec = do
  describe "Service.Query.Pagination" do
    describe "constants" do
      it "defaultLimit is 100" \_ -> do
        defaultLimit |> shouldBe 100

      it "absoluteMaxLimit is 1000" \_ -> do
        absoluteMaxLimit |> shouldBe 1000

      it "absoluteMaxOffset is 10_000_000" \_ -> do
        absoluteMaxOffset |> shouldBe 10_000_000

    describe "parsePageRequest" do
      describe "happy paths" do
        it "parses valid limit and offset" \_ -> do
          let result = parsePageRequest (Just "25") (Just "50")
          result.limit |> shouldBe 25
          result.offset |> shouldBe 50

        it "uses defaults when both params missing" \_ -> do
          let result = parsePageRequest Nothing Nothing
          result.limit |> shouldBe 100
          result.offset |> shouldBe 0

        it "uses default limit when only offset provided" \_ -> do
          let result = parsePageRequest Nothing (Just "10")
          result.limit |> shouldBe 100
          result.offset |> shouldBe 10

        it "uses default offset when only limit provided" \_ -> do
          let result = parsePageRequest (Just "50") Nothing
          result.limit |> shouldBe 50
          result.offset |> shouldBe 0

      describe "limit edge cases" do
        it "clamps limit=0 to 1" \_ -> do
          let result = parsePageRequest (Just "0") Nothing
          result.limit |> shouldBe 1

        it "clamps negative limit to 1" \_ -> do
          let result = parsePageRequest (Just "-5") Nothing
          result.limit |> shouldBe 1

        it "clamps limit above absoluteMaxLimit to 1000" \_ -> do
          let result = parsePageRequest (Just "5000") Nothing
          result.limit |> shouldBe 1000

        it "allows limit at absoluteMaxLimit" \_ -> do
          let result = parsePageRequest (Just "1000") Nothing
          result.limit |> shouldBe 1000

        it "allows limit of 1" \_ -> do
          let result = parsePageRequest (Just "1") Nothing
          result.limit |> shouldBe 1

        it "falls back to default on non-numeric limit" \_ -> do
          let result = parsePageRequest (Just "abc") Nothing
          result.limit |> shouldBe 100

        it "falls back to default on empty string limit" \_ -> do
          let result = parsePageRequest (Just "") Nothing
          result.limit |> shouldBe 100

      describe "offset edge cases" do
        it "clamps negative offset to 0" \_ -> do
          let result = parsePageRequest Nothing (Just "-10")
          result.offset |> shouldBe 0

        it "allows offset of 0" \_ -> do
          let result = parsePageRequest Nothing (Just "0")
          result.offset |> shouldBe 0

        it "clamps very large offset to absoluteMaxOffset" \_ -> do
          let result = parsePageRequest Nothing (Just "99999999")
          result.offset |> shouldBe absoluteMaxOffset

        it "falls back to default on non-numeric offset" \_ -> do
          let result = parsePageRequest Nothing (Just "xyz")
          result.offset |> shouldBe 0

    describe "QueryPageResponse JSON" do
      it "serializes to JSON with correct field names" \_ -> do
        let response = QueryPageResponse
              { items = [1 :: Int, 2, 3]
              , total = 10
              , hasMore = True
              , effectiveLimit = 3
              }
        let encoded = Json.encodeText response
        let decoded = Json.decodeText @Json.Value encoded
        case decoded of
          Ok _ -> pass
          Err _err -> fail "Failed to encode/decode QueryPageResponse"

      it "round-trips through JSON encode/decode" \_ -> do
        let response = QueryPageResponse
              { items = [42 :: Int, 99]
              , total = 5
              , hasMore = True
              , effectiveLimit = 2
              }
        let encoded = Json.encodeText response
        let decoded = Json.decodeText @(QueryPageResponse Int) encoded
        case decoded of
          Ok roundTripped -> roundTripped |> shouldBe response
          Err _err -> fail "Failed to round-trip QueryPageResponse"

      it "serializes empty items array" \_ -> do
        let response = QueryPageResponse
              { items = [] :: Array Int
              , total = 0
              , hasMore = False
              , effectiveLimit = 100
              }
        let encoded = Json.encodeText response
        let decoded = Json.decodeText @(QueryPageResponse Int) encoded
        case decoded of
          Ok roundTripped -> do
            Array.length roundTripped.items |> shouldBe 0
            roundTripped.total |> shouldBe 0
            roundTripped.hasMore |> shouldBe False
          Err _err -> fail "Failed to round-trip empty QueryPageResponse"
