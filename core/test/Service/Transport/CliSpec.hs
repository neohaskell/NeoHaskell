module Service.Transport.CliSpec where

import ConcurrentVar qualified
import Core
import Json qualified
import Record qualified
import Service.Auth qualified as Auth
import Service.Response (CommandResponse (..))
import Service.Transport (Transport (..))
import Service.Transport.Cli (CliTransport (..), cli)
import Task qualified
import Test
import Test.Service.Command.Core (AddItemToCart (..))
import Text qualified
import Uuid qualified


-- ============================================================================
-- NameOf instance for test command (not provided by Test.Service.Command.Core)
-- ============================================================================

type instance NameOf AddItemToCart = "AddItemToCart"


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Transport.Cli" do
    -- ====================================================================
    -- cli default constructor
    -- ====================================================================
    describe "cli defaults" do
      it "has default programName of 'app'" \_ -> do
        cli.programName |> shouldBe "app"

      it "has default version of '0.0.0'" \_ -> do
        cli.version |> shouldBe "0.0.0"

      it "has default empty description" \_ -> do
        cli.description |> shouldBe ""

    -- ====================================================================
    -- Custom CliTransport constructor
    -- ====================================================================
    describe "CliTransport constructor" do
      it "allows setting custom programName" \_ -> do
        let transport = CliTransport {programName = "myapp", version = "1.0.0", description = "My app"}
        transport.programName |> shouldBe "myapp"

      it "allows setting custom version and description" \_ -> do
        let transport = CliTransport {programName = "tool", version = "2.3.1", description = "A CLI tool"}
        transport.version |> shouldBe "2.3.1"
        transport.description |> shouldBe "A CLI tool"

    -- ====================================================================
    -- runTransport is identity
    -- ====================================================================
    describe "runTransport" do
      it "returns the task unchanged (identity)" \_ -> do
        result <- runTransport cli (Task.yield unit)
        result |> shouldBe unit

    -- ====================================================================
    -- buildHandler
    -- ====================================================================
    describe "buildHandler" do
      it "decodes valid JSON and calls handler with parsed command" \_ -> do
        cartId <- Uuid.generate
        itemId <- Uuid.generate
        let cmd = AddItemToCart {cartId = cartId, itemId = itemId, amount = 5}
        let body = Json.encodeText cmd |> Text.toBytes

        let handler _ctx _receivedCmd =
              Task.yield (Accepted {entityId = "test-entity-id"})

        let endpointHandler =
              buildHandler cli (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        result |> shouldBe (Accepted {entityId = "test-entity-id"})

      it "returns Failed response for invalid JSON" \_ -> do
        let body = "not-valid-json" |> Text.toBytes

        let handler _ctx _cmd =
              Task.yield (Accepted {entityId = "should-not-reach"})

        let endpointHandler =
              buildHandler cli (Record.Proxy @AddItemToCart) handler

        requestContext <- Auth.anonymousContext
        resultVar <- ConcurrentVar.new

        endpointHandler requestContext body (\(response, _responseBytes) ->
          ConcurrentVar.set response resultVar
          )

        result <- ConcurrentVar.get resultVar
        case result of
          Failed {} -> pass
          other -> fail [fmt|Expected Failed response, got: #{toText other}|]
