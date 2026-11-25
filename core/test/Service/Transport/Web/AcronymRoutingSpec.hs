-- | Check Web command registration when distinct names share one URL.
-- The service runner must reject that collision before serving requests.
module Service.Transport.Web.AcronymRoutingSpec where

import Core
import Data.ByteString.Lazy qualified as GhcLBS
import Decider qualified
import Bytes qualified
import Json qualified
import Map qualified
import Maybe qualified
import Network.HTTP.Types.Method qualified as HTTPMethod
import Network.HTTP.Types.Status qualified as HTTPStatus
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Service qualified
import Service.AccessControl (AccessError (Unauthenticated))
import Service.Auth (RequestContext)
import Service.Auth qualified as Auth
import Service.Response (CommandResponse (Unauthorized))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Service.Transport qualified as Transport
import Service.Transport.Internal (InternalTransport)
import Service.Transport.Mcp (McpTransport (..))
import Service.Transport.Web qualified as Web
import Service.Transport.Web.RequestBodyFailureSpec (WebDecodeProbeCommand, WebDecodeProbeEntity (..), WebDecodeProbeEvent (..))
import Task qualified
import Test
import Text qualified
import Uuid qualified


data WebDecodeProbeCOMMAND = WebDecodeProbeCOMMAND
  { probeId :: Uuid
  }


getEntityId :: WebDecodeProbeCOMMAND -> Maybe Uuid
getEntityId cmd = Just cmd.probeId


decide :: WebDecodeProbeCOMMAND -> Maybe WebDecodeProbeEntity -> RequestContext -> Decision WebDecodeProbeEvent
decide cmd maybeEntity _ctx = case maybeEntity of
  Nothing -> Decider.acceptNew [WebDecodeProbeCreated cmd.probeId]
  Just _ -> Decider.reject "Probe already exists"


type instance EntityOf WebDecodeProbeCOMMAND = WebDecodeProbeEntity


type instance TransportsOf WebDecodeProbeCOMMAND = '[Web.WebTransport, McpTransport, InternalTransport]


command ''WebDecodeProbeCOMMAND


spec :: Spec Unit
spec = do
  describe "Web command registration" do
    it "executes an acronym command through its kebab-case Web route" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @WebDecodeProbeCOMMAND |> Service.toServiceRunner
      let transports = Map.fromArray
            [ ("WebTransport", ServiceDefinition.TransportValue Web.server)
            , ("McpTransport", ServiceDefinition.TransportValue McpTransport {serverName = "test", serverVersion = "1.0"})
            ]
      (handlers, schemas, _) <- runner.getEndpointsByTransport eventStore transports
      let webHandlers = handlers |> Map.get "WebTransport" |> Maybe.withDefault Map.empty
      let webSchemas = schemas |> Map.get "WebTransport" |> Maybe.withDefault Map.empty
      let endpoints = Transport.Endpoints
            { Transport.transport = Web.server
            , Transport.commandEndpoints = webHandlers
            , Transport.queryEndpoints = Map.empty
            , Transport.queryByIdEndpoints = Map.empty
            , Transport.commandSchemas = webSchemas
            , Transport.querySchemas = Map.empty
            }
      let app request respond = do
            let runnable = Transport.assembleTransport endpoints
            runnable request (\response -> Task.fromIO (respond response)) |> Task.runOrPanic
      probeId <- Uuid.generate
      let request = Wai.defaultRequest
            { Wai.requestMethod = HTTPMethod.methodPost
            , Wai.rawPathInfo = "/commands/web-decode-probe-command"
            , Wai.pathInfo = ["commands", "web-decode-probe-command"]
            }
      let payload = Json.encodeText WebDecodeProbeCOMMAND {probeId}
            |> Text.toBytes
            |> Bytes.unwrap
            |> GhcLBS.fromStrict
      response <- WaiTest.runSession (WaiTest.srequest (WaiTest.SRequest request payload)) app |> Task.fromIO
      let statusCode = response.simpleStatus |> HTTPStatus.statusCode
      Task.unless (statusCode == 200) do
        let responseBody = response.simpleBody |> GhcLBS.toStrict |> Bytes.fromLegacy |> Text.fromBytes
        fail [fmt|Expected HTTP 200, got #{statusCode}: #{responseBody}|]
      let typedEventStore = eventStore |> EventStore.castEventStore @WebDecodeProbeEvent
      fetcher <- EntityFetcher.new typedEventStore (initialStateImpl @WebDecodeProbeEntity) (updateImpl @WebDecodeProbeEntity) |> Task.mapError toText
      found <- fetcher.fetch (EntityName "WebDecodeProbeEntity") (StreamId.toStreamId probeId) |> Task.mapError toText
      case found of
        EntityFetcher.EntityNotFound -> fail "Expected the Web command to create the probe"
        EntityFetcher.EntityFound probe -> probe.state.probeExists |> shouldBe True

    it "uses the URL key only for Web and keeps the canonical MCP name" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @WebDecodeProbeCOMMAND |> Service.toServiceRunner
      let transports = Map.fromArray
            [ ("WebTransport", ServiceDefinition.TransportValue Web.server)
            , ("McpTransport", ServiceDefinition.TransportValue McpTransport {serverName = "test", serverVersion = "1.0"})
            ]
      (handlers, schemas, dispatch) <- runner.getEndpointsByTransport eventStore transports
      let webHandlers = handlers |> Map.get "WebTransport"
      let webSchemas = schemas |> Map.get "WebTransport"
      let mcpHandlers = handlers |> Map.get "McpTransport"
      webHandlers |> shouldSatisfy (\bucket -> case bucket of {Just entries -> Map.contains "web-decode-probe-command" entries; Nothing -> False})
      webSchemas |> shouldSatisfy (\bucket -> case bucket of {Just entries -> Map.contains "web-decode-probe-command" entries; Nothing -> False})
      mcpHandlers |> shouldSatisfy (\bucket -> case bucket of {Just entries -> Map.contains "WebDecodeProbeCOMMAND" entries; Nothing -> False})
      dispatch |> Map.keys |> shouldBe ["WebDecodeProbeCOMMAND"]
      handlers |> Map.contains "InternalTransport" |> shouldBe False

    it "requires authorization on both explicitly selected entry points" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @WebDecodeProbeCOMMAND |> Service.toServiceRunner
      let adapters = Map.fromArray
            [ ("WebTransport", ServiceDefinition.TransportValue Web.server)
            , ("McpTransport", ServiceDefinition.TransportValue McpTransport {serverName = "test", serverVersion = "1.0"})
            ]
      (handlers, _, dispatch) <- runner.getEndpointsByTransport eventStore adapters
      let web = handlers |> Map.get "WebTransport" |> Maybe.andThen (Map.get "web-decode-probe-command")
      let internal = dispatch |> Map.get "WebDecodeProbeCOMMAND"
      case (web, internal) of
        (Just webHandler, Just internalHandler) -> do
          anonymous <- Auth.anonymousContext
          let body = Json.encodeText WebDecodeProbeCOMMAND {probeId = Uuid.nil} |> Text.toBytes
          let checkDenied (response, _) = response |> shouldBe (Unauthorized Unauthenticated)
          webHandler anonymous body checkDenied
          internalHandler Nothing anonymous body checkDenied
        _ -> fail "Expected both Web and internal command handlers"

    it "rejects an explicitly selected missing adapter with a startup error" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner = Service.new |> Service.command @WebDecodeProbeCOMMAND |> Service.toServiceRunner
      let adapters = Map.fromArray [("WebTransport", ServiceDefinition.TransportValue Web.server)]
      result <- runner.getEndpointsByTransport eventStore adapters |> Task.asResult
      case result of
        Err message -> message |> shouldSatisfy (Text.contains "Command WebDecodeProbeCOMMAND selects unregistered transport McpTransport")
        Ok _ -> fail "Expected the missing transport to fail startup"

    it "rejects distinct command names that map to one Web URL" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let runner =
            Service.new
              |> Service.command @WebDecodeProbeCommand
              |> Service.command @WebDecodeProbeCOMMAND
              |> Service.toServiceRunner
      let transports = Map.fromArray
            [ ("WebTransport", ServiceDefinition.TransportValue Web.server)
            , ("McpTransport", ServiceDefinition.TransportValue McpTransport {serverName = "test", serverVersion = "1.0"})
            ]
      result <- Task.asResultSafe do
        (handlers, _, _) <- runner.getEndpointsByTransport eventStore transports
        handlers |> Map.length |> Task.evaluate
      case result of
        Err errorMessage ->
          errorMessage
            |> shouldSatisfy (Text.contains "Duplicate command handler registered for: web-decode-probe-command")
        Ok _ -> fail "Expected a Web URL collision to reject service registration"
