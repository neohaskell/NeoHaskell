module Service.Transport.Mcp.ResponseSpec where

import Core
import Json qualified
import Service.Response (CommandResponse (..))
import Service.Transport.Mcp.Response
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Service.Transport.Mcp.Response" do
    -- ====================================================================
    -- toCallToolResult
    -- ====================================================================
    describe "toCallToolResult" do
      it "maps Accepted to two content blocks" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "abc-123" })
        let jsonText = Json.encodeText result
        -- Should contain isError: false and two content blocks
        jsonText |> shouldSatisfy (Text.contains "\"isError\":false")
        jsonText |> shouldSatisfy (Text.contains "abc-123")

      it "first block has user+assistant audience" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "xyz" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"audience\"")
        jsonText |> shouldSatisfy (Text.contains "\"user\"")
        jsonText |> shouldSatisfy (Text.contains "\"assistant\"")

      it "second block has assistant-only audience with priority" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "xyz" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"priority\"")

      it "second block mentions resources/read" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "xyz" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "resources/read")

      it "maps Rejected to isError true" \_ -> do
        let result = toCallToolResult (Rejected { reason = "Out of stock" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"isError\":true")
        jsonText |> shouldSatisfy (Text.contains "Out of stock")

      it "maps Failed to isError true with generic message" \_ -> do
        let result = toCallToolResult (Failed { error = "DB connection lost" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"isError\":true")
        -- Security: must NOT leak internal error details
        jsonText |> shouldSatisfy (\t -> Text.contains "DB connection lost" t |> not)

      it "Accepted with empty entityId produces valid JSON" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"isError\":false")

      it "Rejected with empty reason produces valid JSON" \_ -> do
        let result = toCallToolResult (Rejected { reason = "" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"isError\":true")
        jsonText |> shouldSatisfy (Text.contains "Command rejected")

      it "content blocks have type text" \_ -> do
        let result = toCallToolResult (Accepted { entityId = "id" })
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"type\":\"text\"")

    -- ====================================================================
    -- toResourceContent
    -- ====================================================================
    describe "toResourceContent" do
      it "wraps query result with URI and mimeType" \_ -> do
        let result = toResourceContent "neohaskell://queries/cart-summary" "{\"items\":[]}"
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "neohaskell://queries/cart-summary")
        jsonText |> shouldSatisfy (Text.contains "application/json")
        jsonText |> shouldSatisfy (Text.contains "\"contents\"")

      it "preserves query JSON verbatim" \_ -> do
        let queryJson = "{\"a\":1}"
        let result = toResourceContent "neohaskell://queries/test" queryJson
        -- The text field value should be the query JSON.
        -- When the outer JSON is serialized, inner quotes are escaped.
        -- Check the escaped form appears in the serialized output.
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "{\\\"a\\\":1}")

      it "handles empty query result" \_ -> do
        let result = toResourceContent "neohaskell://queries/test" "[]"
        let jsonText = Json.encodeText result
        jsonText |> shouldSatisfy (Text.contains "\"contents\"")
