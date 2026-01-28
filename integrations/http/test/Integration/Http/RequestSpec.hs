{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Http.RequestSpec where

import Array qualified
import Core
import Integration.Http.Request qualified as Request
import Json qualified
import Task qualified
import Test


-- ============================================================================
-- Test Types
-- ============================================================================

data TestPayload = TestPayload
  { name :: Text
  , value :: Int
  }
  deriving (Generic, Eq, Show)

instance Json.ToJSON TestPayload
instance Json.FromJSON TestPayload


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Http.Request" do
    describe "Method" do
      it "GET equals GET" \_ -> do
        Request.GET |> shouldBe Request.GET

      it "POST equals POST" \_ -> do
        Request.POST |> shouldBe Request.POST

      it "methods are distinct" \_ -> do
        Request.GET |> shouldNotBe Request.POST
        Request.POST |> shouldNotBe Request.PUT
        Request.PUT |> shouldNotBe Request.PATCH
        Request.PATCH |> shouldNotBe Request.DELETE

      it "methods are showable" \_ -> do
        show Request.GET |> shouldBe "GET"
        show Request.POST |> shouldBe "POST"
        show Request.PUT |> shouldBe "PUT"
        show Request.PATCH |> shouldBe "PATCH"
        show Request.DELETE |> shouldBe "DELETE"

    describe "Body helpers" do
      describe "json" do
        it "creates JsonBody from ToJSON value" \_ -> do
          let body = Request.json TestPayload { name = "test", value = 42 }
          case body of
            Request.JsonBody _ -> Task.yield unit
            _ -> fail "Expected JsonBody"

        it "encodes simple values" \_ -> do
          let body = Request.json (42 :: Int)
          case body of
            Request.JsonBody jsonVal ->
              Json.encode jsonVal |> shouldBe (Json.encode (42 :: Int))
            _ -> fail "Expected JsonBody"

        it "encodes nested structures" \_ -> do
          let payload = TestPayload { name = "nested", value = 100 }
          let body = Request.json payload
          case body of
            Request.JsonBody jsonVal -> do
              let decoded = Json.decode @TestPayload jsonVal
              case decoded of
                Ok p -> p |> shouldBe payload
                Err _ -> fail "Failed to decode JSON"
            _ -> fail "Expected JsonBody"

      describe "form" do
        it "creates FormBody from key-value pairs" \_ -> do
          let body = Request.form [("key", "value")]
          case body of
            Request.FormBody params -> params |> shouldBe [("key", "value")]
            _ -> fail "Expected FormBody"

        it "preserves multiple parameters" \_ -> do
          let params = [("a", "1"), ("b", "2"), ("c", "3")]
          let body = Request.form params
          case body of
            Request.FormBody p -> Array.length p |> shouldBe 3
            _ -> fail "Expected FormBody"

        it "handles empty parameters" \_ -> do
          let body = Request.form []
          case body of
            Request.FormBody params -> Array.length params |> shouldBe 0
            _ -> fail "Expected FormBody"

      describe "raw" do
        it "creates RawBody with content type and content" \_ -> do
          let body = Request.raw "application/xml" "<data/>"
          case body of
            Request.RawBody { contentType, content } -> do
              contentType |> shouldBe "application/xml"
              content |> shouldBe "<data/>"
            _ -> fail "Expected RawBody"

        it "preserves custom content types" \_ -> do
          let body = Request.raw "text/csv" "a,b,c"
          case body of
            Request.RawBody { contentType } ->
              contentType |> shouldBe "text/csv"
            _ -> fail "Expected RawBody"

      describe "noBody" do
        it "creates NoBody" \_ -> do
          let body = Request.noBody
          case body of
            Request.NoBody -> Task.yield unit
            _ -> fail "Expected NoBody"

        it "is equal to itself" \_ -> do
          Request.noBody |> shouldBe Request.noBody

    describe "Body equality" do
      it "JsonBody values are comparable" \_ -> do
        let body1 = Request.json (42 :: Int)
        let body2 = Request.json (42 :: Int)
        body1 |> shouldBe body2

      it "different JsonBody values are not equal" \_ -> do
        let body1 = Request.json (42 :: Int)
        let body2 = Request.json (43 :: Int)
        body1 |> shouldNotBe body2

      it "different body types are not equal" \_ -> do
        let jsonBody = Request.json (42 :: Int)
        let formBody = Request.form [("key", "42")]
        jsonBody |> shouldNotBe formBody
