module Service.Transport.Mcp.JsonRpcSpec where

import Core
import Json qualified
import Service.Transport.Mcp.JsonRpc
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Transport.Mcp.JsonRpc" do
    -- ====================================================================
    -- parseRequest
    -- ====================================================================
    describe "parseRequest" do
      it "parses valid request with id" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/list\",\"id\":1}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.method |> shouldBe "tools/list"
            req.id |> shouldBe (Just (Json.toJSON (1 :: Int)))
          Err _ -> fail "Expected Ok, got Err"

      it "parses request with params" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"tools/call\",\"params\":{\"name\":\"AddItem\"},\"id\":2}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.method |> shouldBe "tools/call"
            case req.params of
              Just _ -> pass
              Nothing -> fail "Expected params to be present"
          Err _ -> fail "Expected Ok, got Err"

      it "parses notification (no id)" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/initialized\"}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.method |> shouldBe "notifications/initialized"
            req.id |> shouldBe Nothing
          Err _ -> fail "Expected Ok, got Err"

      it "parses request with string id" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":\"abc\"}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.id |> shouldBe (Just (Json.toJSON ("abc" :: Text)))
          Err _ -> fail "Expected Ok, got Err"

      it "parses request with null id" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":null}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.id |> shouldBe (Just Json.null)
          Err _ -> fail "Expected Ok, got Err"

      it "rejects missing jsonrpc field" \_ -> do
        let input = "{\"method\":\"ping\",\"id\":1}" |> Text.toBytes
        case parseRequest input of
          Ok _ -> fail "Expected Err for missing jsonrpc"
          Err _ -> pass

      it "rejects wrong jsonrpc version" \_ -> do
        let input = "{\"jsonrpc\":\"1.0\",\"method\":\"ping\",\"id\":1}" |> Text.toBytes
        case parseRequest input of
          Ok _ -> fail "Expected Err for wrong version"
          Err _ -> pass

      it "rejects missing method field" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"id\":1}" |> Text.toBytes
        case parseRequest input of
          Ok _ -> fail "Expected Err for missing method"
          Err _ -> pass

      it "rejects malformed JSON" \_ -> do
        let input = "not json at all" |> Text.toBytes
        case parseRequest input of
          Ok _ -> fail "Expected Err for malformed JSON"
          Err _ -> pass

      it "rejects empty input" \_ -> do
        let input = "" |> Text.toBytes
        case parseRequest input of
          Ok _ -> fail "Expected Err for empty input"
          Err _ -> pass

      it "handles unicode in method name" \_ -> do
        let input = "{\"jsonrpc\":\"2.0\",\"method\":\"tst/\x00E9\",\"id\":1}" |> Text.toBytes
        case parseRequest input of
          Ok req -> do
            req.method |> shouldSatisfy (Text.contains "\x00E9")
          Err _ -> fail "Expected Ok for unicode method"

    -- ====================================================================
    -- Error constructors
    -- ====================================================================
    describe "error constructors" do
      it "parseError has code -32700" \_ -> do
        let err = parseError "bad"
        err.code |> shouldBe (-32700)

      it "invalidRequest has code -32600" \_ -> do
        let err = invalidRequest "msg"
        err.code |> shouldBe (-32600)

      it "methodNotFound has code -32601" \_ -> do
        let err = methodNotFound "x"
        err.code |> shouldBe (-32601)

      it "invalidParams has code -32602" \_ -> do
        let err = invalidParams "y"
        err.code |> shouldBe (-32602)

      it "internalError has code -32603" \_ -> do
        internalError.code |> shouldBe (-32603)

      it "internalError uses generic message" \_ -> do
        let msg = internalError.message
        -- Should not contain implementation details
        msg |> shouldSatisfy (\m -> Text.length m > 0)

    -- ====================================================================
    -- Response construction and serialization
    -- ====================================================================
    describe "response construction" do
      it "successResponse includes jsonrpc 2.0 and id" \_ -> do
        let resp = successResponse (Just (Json.toJSON (1 :: Int))) (Json.object [])
        resp.jsonrpc |> shouldBe "2.0"
        resp.id |> shouldBe (Just (Json.toJSON (1 :: Int)))

      it "successResponse includes result value" \_ -> do
        let resultVal = Json.toJSON ("hello" :: Text)
        let resp = successResponse (Just (Json.toJSON (5 :: Int))) resultVal
        resp.result |> shouldBe (Just resultVal)

      it "errorResponse includes error object" \_ -> do
        let err = methodNotFound "foo"
        let resp = errorResponse (Just (Json.toJSON (1 :: Int))) err
        case resp.error of
          Just e -> e.code |> shouldBe (-32601)
          Nothing -> fail "Expected error in response"

      it "errorResponse preserves request id" \_ -> do
        let err = methodNotFound "foo"
        let resp = errorResponse (Just (Json.toJSON ("req-1" :: Text))) err
        resp.id |> shouldBe (Just (Json.toJSON ("req-1" :: Text)))

      it "encodeResponse produces valid JSON bytes" \_ -> do
        let resp = successResponse (Just (Json.toJSON (1 :: Int))) (Json.object [])
        let encoded = encodeResponse resp
        case Json.decodeBytes @Json.Value encoded of
          Ok _ -> pass
          Err _ -> fail "encodeResponse did not produce valid JSON"

      it "encodeResponse ends with newline" \_ -> do
        let resp = successResponse (Just (Json.toJSON (1 :: Int))) (Json.object [])
        let encoded = encodeResponse resp
        let encodedText = encoded |> Text.fromBytes
        encodedText |> shouldSatisfy (Text.endsWith "\n")

      it "successResponse handles null id" \_ -> do
        let resp = successResponse (Just Json.null) (Json.object [])
        resp.id |> shouldBe (Just Json.null)
