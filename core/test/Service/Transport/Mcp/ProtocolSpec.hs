module Service.Transport.Mcp.ProtocolSpec where

import Core
import ConcurrentVar qualified
import Json qualified
import Map qualified
import Service.Response (CommandResponse (..))
import Service.Transport (EndpointHandler, EndpointSchema (..), QueryEndpointHandler)
import Service.Transport.Mcp.JsonRpc (JsonRpcError (..), JsonRpcRequest (..), JsonRpcResponse (..))
import Service.Transport.Mcp.Protocol
import Task qualified
import Test
import Text qualified


-- ============================================================================
-- Test helpers
-- ============================================================================


-- | A mock command handler that returns Accepted with a fixed entity ID.
mockCommandHandler :: EndpointHandler
mockCommandHandler _ctx _body respond = do
  let response = Accepted { entityId = "mock-entity-id" }
  let responseBytes = Json.encodeText response |> Text.toBytes
  respond (response, responseBytes)


-- | A mock query handler that returns a fixed JSON result.
mockQueryHandler :: QueryEndpointHandler
mockQueryHandler _userClaims _expr _pageRequest =
  Task.yield "{\"items\":[],\"total\":0}"
    |> Task.mapError (\_ -> panic "unreachable")


-- | A basic EndpointSchema for testing.
mockCommandSchema :: EndpointSchema
mockCommandSchema = EndpointSchema
  { requestSchema = Just (SObject [])
  , responseSchema = SObject []
  , description = "Add item to cart"
  , deprecated = False
  , entityName = Just "CartEntity"
  }


-- | A basic query EndpointSchema for testing.
mockQuerySchema :: EndpointSchema
mockQuerySchema = EndpointSchema
  { requestSchema = Nothing
  , responseSchema = SObject []
  , description = "Cart summary"
  , deprecated = False
  , entityName = Nothing
  }


-- | Create a test server state with mock endpoints.
createTestState :: Task Text ServerState
createTestState = do
  let commandEndpoints = Map.empty |> Map.set "AddItem" mockCommandHandler
  let queryEndpoints = Map.empty |> Map.set "cart-summary" mockQueryHandler
  let commandSchemas = Map.empty |> Map.set "AddItem" mockCommandSchema
  let querySchemas = Map.empty |> Map.set "cart-summary" mockQuerySchema
  state <- newServerState "test-server" "1.0.0" commandEndpoints queryEndpoints commandSchemas querySchemas
  -- Mark as initialized for most tests
  _ <- ConcurrentVar.swap True state.initialized
  Task.yield state


-- | Create a test server state that is NOT initialized.
createUninitializedState :: Task Text ServerState
createUninitializedState = do
  newServerState "test-server" "1.0.0" Map.empty Map.empty Map.empty Map.empty


-- | Create a test server state with no endpoints.
createEmptyState :: Task Text ServerState
createEmptyState = do
  state <- newServerState "test-server" "1.0.0" Map.empty Map.empty Map.empty Map.empty
  _ <- ConcurrentVar.swap True state.initialized
  Task.yield state


-- | Helper to make a JsonRpcRequest.
makeRequest :: Text -> Maybe Json.Value -> Maybe Json.Value -> JsonRpcRequest
makeRequest method params reqId = JsonRpcRequest
  { method = method
  , params = params
  , id = reqId
  }


-- ============================================================================
-- Tests
-- ============================================================================


spec :: Spec Unit
spec = do
  describe "Service.Transport.Mcp.Protocol" do
    -- ====================================================================
    -- handleRequest — initialization
    -- ====================================================================
    describe "handleRequest — initialization" do
      it "responds to initialize with capabilities" \_ -> do
        state <- createUninitializedState
        let req = makeRequest "initialize" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response for initialize"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result in initialize response"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"capabilities\"")
                jsonText |> shouldSatisfy (Text.contains "\"tools\"")
                jsonText |> shouldSatisfy (Text.contains "\"resources\"")
                jsonText |> shouldSatisfy (Text.contains "\"prompts\"")
                jsonText |> shouldSatisfy (Text.contains "\"logging\"")

      it "includes serverInfo in initialize response" \_ -> do
        state <- createUninitializedState
        let req = makeRequest "initialize" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "test-server")
                jsonText |> shouldSatisfy (Text.contains "1.0.0")

      it "includes protocolVersion in initialize response" \_ -> do
        state <- createUninitializedState
        let req = makeRequest "initialize" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "2025-03-26")

      it "returns Nothing for notifications/initialized" \_ -> do
        state <- createUninitializedState
        -- First initialize
        let initReq = makeRequest "initialize" Nothing (Just (Json.toJSON (1 :: Int)))
        _ <- handleRequest state initReq
        -- Then send initialized notification
        let req = makeRequest "notifications/initialized" Nothing Nothing
        maybeResp <- handleRequest state req
        maybeResp |> shouldBe Nothing

      it "rejects non-init requests before initialization" \_ -> do
        state <- createUninitializedState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected error response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error in response"
              Just err -> err.code |> shouldBe (-32600)

      it "allows initialize before initialized notification" \_ -> do
        state <- createUninitializedState
        let req = makeRequest "initialize" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            resp.error |> shouldBe Nothing

    -- ====================================================================
    -- handleRequest — tools
    -- ====================================================================
    describe "handleRequest — tools" do
      it "tools/list returns registered commands as tools" \_ -> do
        state <- createTestState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"tools\"")

      it "tools/list includes tool name from schema key" \_ -> do
        state <- createTestState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "AddItem")

      it "tools/list includes description from EndpointSchema" \_ -> do
        state <- createTestState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "Add item to cart")

      it "tools/list includes inputSchema from requestSchema" \_ -> do
        state <- createTestState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"inputSchema\"")

      it "tools/list returns empty array when no commands" \_ -> do
        state <- createEmptyState
        let req = makeRequest "tools/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"tools\":[]")

      it "tools/call dispatches to correct handler" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "name" Json..= ("AddItem" :: Text)
              , "arguments" Json..= Json.object []
              ]
        let req = makeRequest "tools/call" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "mock-entity-id")

      it "tools/call returns error for unknown tool" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "name" Json..= ("NonExistent" :: Text)
              , "arguments" Json..= Json.object []
              ]
        let req = makeRequest "tools/call" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error for unknown tool"
              Just err -> do
                err.code |> shouldBe (-32602)
                err.message |> shouldSatisfy (Text.contains "Unknown tool")

      it "tools/call returns error for missing name param" \_ -> do
        state <- createTestState
        let params = Json.object []
        let req = makeRequest "tools/call" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error for missing name"
              Just err -> err.code |> shouldBe (-32602)

      it "tools/call passes anonymous context to handler" \_ -> do
        -- This is tested via the mock handler which always returns Accepted
        -- The real test is that it doesn't crash with anonymous context
        state <- createTestState
        let params = Json.object
              [ "name" Json..= ("AddItem" :: Text)
              , "arguments" Json..= Json.object []
              ]
        let req = makeRequest "tools/call" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            resp.error |> shouldBe Nothing

    -- ====================================================================
    -- handleRequest — resources
    -- ====================================================================
    describe "handleRequest — resources" do
      it "resources/list returns registered queries as resources" \_ -> do
        state <- createTestState
        let req = makeRequest "resources/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"resources\"")

      it "resources/list URIs use neohaskell:// scheme" \_ -> do
        state <- createTestState
        let req = makeRequest "resources/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "neohaskell://queries/cart-summary")

      it "resources/list returns empty array when no queries" \_ -> do
        state <- createEmptyState
        let req = makeRequest "resources/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"resources\":[]")

      it "resources/read dispatches to correct query handler" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "uri" Json..= ("neohaskell://queries/cart-summary" :: Text)
              ]
        let req = makeRequest "resources/read" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"contents\"")

      it "resources/read returns error for unknown resource" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "uri" Json..= ("neohaskell://queries/nonexistent" :: Text)
              ]
        let req = makeRequest "resources/read" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error for unknown resource"
              Just err -> do
                err.code |> shouldBe (-32602)
                err.message |> shouldSatisfy (Text.contains "Unknown resource")

      it "resources/read returns error for malformed URI" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "uri" Json..= ("not-a-valid-uri" :: Text)
              ]
        let req = makeRequest "resources/read" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error for malformed URI"
              Just err -> err.code |> shouldBe (-32602)

    -- ====================================================================
    -- handleRequest — built-in methods
    -- ====================================================================
    describe "handleRequest — built-in methods" do
      it "ping returns empty result" \_ -> do
        state <- createTestState
        let req = makeRequest "ping" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response for ping"
          Just resp -> do
            resp.error |> shouldBe Nothing
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldBe "{}"

      it "notifications/cancelled returns Nothing" \_ -> do
        state <- createTestState
        let req = makeRequest "notifications/cancelled" Nothing Nothing
        maybeResp <- handleRequest state req
        maybeResp |> shouldBe Nothing

      it "unknown method returns -32601" \_ -> do
        state <- createTestState
        let req = makeRequest "foo/bar" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected error response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error"
              Just err -> err.code |> shouldBe (-32601)

      it "prompts/list returns empty list" \_ -> do
        state <- createTestState
        let req = makeRequest "prompts/list" Nothing (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected response"
          Just resp -> do
            case resp.result of
              Nothing -> fail "Expected result"
              Just resultVal -> do
                let jsonText = Json.encodeText resultVal
                jsonText |> shouldSatisfy (Text.contains "\"prompts\":[]")

      it "prompts/get returns error" \_ -> do
        state <- createTestState
        let params = Json.object
              [ "name" Json..= ("test-prompt" :: Text)
              ]
        let req = makeRequest "prompts/get" (Just params) (Just (Json.toJSON (1 :: Int)))
        maybeResp <- handleRequest state req
        case maybeResp of
          Nothing -> fail "Expected error response"
          Just resp -> do
            case resp.error of
              Nothing -> fail "Expected error for prompts/get"
              Just _err -> pass

    -- ====================================================================
    -- newServerState
    -- ====================================================================
    describe "newServerState" do
      it "initializes with initialized = False" \_ -> do
        state <- newServerState "test" "1.0" Map.empty Map.empty Map.empty Map.empty
        isInit <- ConcurrentVar.peek state.initialized
        isInit |> shouldBe False

      it "pre-computes cachedToolsList as valid JSON" \_ -> do
        let commandSchemas = Map.empty |> Map.set "AddItem" mockCommandSchema
        state <- newServerState "test" "1.0" Map.empty Map.empty commandSchemas Map.empty
        -- cachedToolsList is now a Json.Value — verify it serializes to valid JSON
        let jsonText = Json.encodeText state.cachedToolsList
        jsonText |> shouldSatisfy (Text.contains "tools")

      it "pre-computes cachedResourcesList as valid JSON" \_ -> do
        let querySchemas = Map.empty |> Map.set "cart-summary" mockQuerySchema
        state <- newServerState "test" "1.0" Map.empty Map.empty Map.empty querySchemas
        let jsonText = Json.encodeText state.cachedResourcesList
        jsonText |> shouldSatisfy (Text.contains "resources")

      it "stores serverName and serverVersion" \_ -> do
        state <- newServerState "my-server" "2.0.0" Map.empty Map.empty Map.empty Map.empty
        state.serverName |> shouldBe "my-server"
        state.serverVersion |> shouldBe "2.0.0"
