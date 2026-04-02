module Service.Transport.McpSpec where

import ConcurrentVar qualified
import Core
import Json qualified
import Record qualified
import Service.Auth qualified as Auth
import Service.Response (CommandResponse (..))
import Service.Transport (Transport (..))
import Service.Transport.Mcp (McpTransport (..))
import Task qualified
import Test
import Test.Service.Command.Core (AddItemToCart (..))
import Text qualified
import Uuid qualified


-- ============================================================================
-- NameOf instance for test command
-- ============================================================================

type instance NameOf AddItemToCart = "AddItemToCart"


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Transport.Mcp" do
    -- ====================================================================
    -- McpTransport constructor
    -- ====================================================================
    describe "McpTransport constructor" do
      it "stores serverName" \_ -> do
        let transport = McpTransport { serverName = "test-svc", serverVersion = "1.0" }
        transport.serverName |> shouldBe "test-svc"

      it "stores serverVersion" \_ -> do
        let transport = McpTransport { serverName = "test-svc", serverVersion = "2.3.1" }
        transport.serverVersion |> shouldBe "2.3.1"

    -- ====================================================================
    -- runTransport
    -- ====================================================================
    describe "runTransport" do
      it "returns the task unchanged (identity)" \_ -> do
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }
        result <- runTransport transport (Task.yield unit)
        result |> shouldBe unit

    -- ====================================================================
    -- buildHandler
    -- ====================================================================
    describe "buildHandler" do
      it "decodes valid JSON and calls handler with parsed command" \_ -> do
        cartId <- Uuid.generate
        itemId <- Uuid.generate
        let cmd = AddItemToCart { cartId = cartId, itemId = itemId, amount = 5 }
        let body = Json.encodeText cmd |> Text.toBytes
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }

        let handler _ctx _receivedCmd =
              Task.yield (Accepted { entityId = "test-entity-id" })

        let endpointHandler =
              buildHandler transport (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        result |> shouldBe (Accepted { entityId = "test-entity-id" })

      it "returns Failed response for invalid JSON" \_ -> do
        let body = "not-valid-json" |> Text.toBytes
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }

        let handler _ctx _cmd =
              Task.yield (Accepted { entityId = "should-not-reach" })

        let endpointHandler =
              buildHandler transport (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        case result of
          Failed {} -> pass
          other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "returns Failed response for empty bytes" \_ -> do
        let body = "" |> Text.toBytes
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }

        let handler _ctx _cmd =
              Task.yield (Accepted { entityId = "should-not-reach" })

        let endpointHandler =
              buildHandler transport (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        case result of
          Failed {} -> pass
          other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "returns Failed response for valid JSON with wrong shape" \_ -> do
        let body = "{}" |> Text.toBytes
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }

        let handler _ctx _cmd =
              Task.yield (Accepted { entityId = "should-not-reach" })

        let endpointHandler =
              buildHandler transport (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        case result of
          Failed {} -> pass
          other -> fail [fmt|Expected Failed response, got: #{toText other}|]

      it "passes anonymous RequestContext to handler" \_ -> do
        cartId <- Uuid.generate
        itemId <- Uuid.generate
        let cmd = AddItemToCart { cartId = cartId, itemId = itemId, amount = 1 }
        let body = Json.encodeText cmd |> Text.toBytes
        let transport = McpTransport { serverName = "test", serverVersion = "1.0" }

        ctxVar <- ConcurrentVar.new

        let handler ctx _receivedCmd = do
              ConcurrentVar.set ctx.user ctxVar
              Task.yield (Accepted { entityId = "test-id" })

        let endpointHandler =
              buildHandler transport (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        capturedUser <- ConcurrentVar.get ctxVar
        capturedUser |> shouldBe Nothing
