{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Service.Transport.Web.RequestBodyFailureSpec where

import Basics
import AtomicVar (AtomicVar)
import AtomicVar qualified
import Control.Concurrent qualified as GhcConcurrent
import Control.Concurrent.MVar qualified as GhcMVar
import Control.Exception qualified as GhcException
import Data.ByteString qualified as GhcBS
import Data.ByteString.Lazy qualified as GhcLBS
import System.IO.Unsafe qualified as GhcUnsafe
import Bytes qualified
import Core
import Decider qualified
import Json qualified
import Map qualified
import Maybe qualified
import Network.HTTP.Types.Method qualified as HTTPMethod
import Network.HTTP.Types.Status qualified as HTTPStatus
import Network.Wai qualified as Wai
import Network.Wai.Test qualified as WaiTest
import Service qualified
import Service.Auth (RequestContext)
import Service.Command.Core (Event (..), TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.ServiceDefinition.Core qualified as ServiceDefinition
import Service.Transport (EndpointHandler)
import Service.Transport qualified as Transport
import Service.Transport.Web qualified as Web
import System.Timeout qualified as GhcTimeout
import Task qualified
import Test
import Text qualified
import Prelude qualified as GhcPrelude
import Uuid qualified
import Var qualified


type DecodeGate = (GhcMVar.MVar (), GhcMVar.MVar ())


decodeGate :: Var (Maybe DecodeGate)
decodeGate = GhcUnsafe.unsafePerformIO (Task.runNoErrors (Var.new Nothing))
{-# NOINLINE decodeGate #-}


waitForDecoderRelease :: ()
waitForDecoderRelease =
  GhcUnsafe.unsafePerformIO do
    maybeGate <- Var.get decodeGate |> Task.runNoErrors
    case maybeGate of
      GhcPrelude.Nothing -> GhcException.throwIO (GhcException.ErrorCall "decoder gate was not installed")
      GhcPrelude.Just (decodeStarted, decodeRelease) -> do
        GhcMVar.putMVar decodeStarted ()
        GhcMVar.takeMVar decodeRelease
{-# NOINLINE waitForDecoderRelease #-}


data WebDecodeProbeEvent = WebDecodeProbeCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON WebDecodeProbeEvent
instance Json.FromJSON WebDecodeProbeEvent


data WebDecodeProbeEntity = WebDecodeProbeEntity
  { storedProbeId :: Uuid
  , probeExists :: Bool
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON WebDecodeProbeEntity
instance Json.FromJSON WebDecodeProbeEntity


type instance EventOf WebDecodeProbeEntity = WebDecodeProbeEvent
type instance EntityOf WebDecodeProbeEvent = WebDecodeProbeEntity
type instance NameOf WebDecodeProbeEntity = "WebDecodeProbeEntity"


instance Entity WebDecodeProbeEntity where
  initialStateImpl = WebDecodeProbeEntity {storedProbeId = Uuid.nil, probeExists = False}
  updateImpl event entity =
    case event of
      WebDecodeProbeCreated probeId ->
        entity {storedProbeId = probeId, probeExists = True}


instance Event WebDecodeProbeEvent where
  getEventEntityIdImpl event =
    case event of
      WebDecodeProbeCreated probeId -> probeId


data WebDecodeProbeCommand = WebDecodeProbeCommand
  { probeId :: Uuid
  , mode :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.FromJSON WebDecodeProbeCommand where
  parseJSON = Json.withObject "WebDecodeProbeCommand" \fields -> do
    mode <- fields Json..: "mode"
    probeId <- fields Json..: "probeId"
    if mode == "decoder-failure"
      then GhcPrelude.error "private-command-decoder-sentinel"
      else if mode == "decoder-blocked"
        then GhcPrelude.seq waitForDecoderRelease (Json.yield WebDecodeProbeCommand {probeId, mode})
        else Json.yield WebDecodeProbeCommand {probeId, mode}


type instance EntityOf WebDecodeProbeCommand = WebDecodeProbeEntity
type instance TransportsOf WebDecodeProbeCommand = '[Web.WebTransport]


getEntityId :: WebDecodeProbeCommand -> Maybe Uuid
getEntityId command = Just command.probeId


decide :: WebDecodeProbeCommand -> Maybe WebDecodeProbeEntity -> RequestContext -> Decision WebDecodeProbeEvent
decide command maybeEntity _requestContext =
  case maybeEntity of
    Maybe.Nothing -> Decider.acceptNew [WebDecodeProbeCreated command.probeId]
    Maybe.Just _ -> Decider.reject "Probe already exists"


command ''WebDecodeProbeCommand


spec :: Spec Unit
spec = do
  describe "Service.Transport.Web command request body failures" do
    it "returns a generic 500 when requestBody throws" \_ -> do
      handlerCalled <- Var.new False
      let endpoints = requestBodyEndpoints (recordHandlerCall handlerCalled)
      let app = webApplication endpoints
      responseResult <-
        Task.fromIO
          ( GhcException.try @GhcException.SomeException
              (WaiTest.runSession (WaiTest.request throwingBodyRequest) app)
          )

      case responseResult of
        GhcPrelude.Left _ -> fail "Expected a response after request body failure"
        GhcPrelude.Right response -> do
          response.simpleStatus |> HTTPStatus.statusCode |> shouldBe 500
          let responseBody = response.simpleBody |> GhcLBS.toStrict |> Bytes.fromLegacy |> Text.fromBytes
          responseBody |> shouldBe "Internal server error"
          Text.contains "private-body-read-details" responseBody |> shouldBe False

      wasHandlerCalled <- Var.get handlerCalled
      wasHandlerCalled |> shouldBe False

    it "preserves cancellation while requestBody is blocked" \_ -> do
      bodyReadStarted <- Task.fromIO GhcMVar.newEmptyMVar
      bodyRelease <- Task.fromIO GhcMVar.newEmptyMVar
      outcomeVar <- Task.fromIO GhcMVar.newEmptyMVar
      let request = blockedBodyRequest bodyReadStarted bodyRelease
      let app = webApplication (requestBodyEndpoints neverCalledHandler)
      requestThread <- Task.fromIO do
        GhcConcurrent.forkIO do
          outcome <-
            GhcException.try @GhcException.SomeException
              (WaiTest.runSession (WaiTest.request request) app)
          GhcMVar.putMVar outcomeVar outcome

      bodyReadStart <-
        Task.fromIO
          (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar bodyReadStarted))
      case bodyReadStart of
        GhcPrelude.Nothing -> do
          GhcConcurrent.killThread requestThread |> Task.fromIO
          fail "The route did not start reading the request body"
        GhcPrelude.Just () -> pass

      GhcConcurrent.killThread requestThread |> Task.fromIO
      maybeOutcome <-
        Task.fromIO
          (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar outcomeVar))
      case maybeOutcome of
        GhcPrelude.Nothing -> fail "The canceled request did not finish"
        GhcPrelude.Just (GhcPrelude.Left exception) ->
          case GhcException.fromException exception :: GhcPrelude.Maybe GhcException.AsyncException of
            GhcPrelude.Just GhcException.ThreadKilled -> pass
            GhcPrelude.Just _ -> fail "Expected ThreadKilled from the canceled request"
            GhcPrelude.Nothing -> fail "Expected an asynchronous cancellation exception"
        GhcPrelude.Just (GhcPrelude.Right _) -> fail "Expected cancellation to escape the WAI request"

    it "reports one safe result when a recognized command decoder throws" \_ -> do
      (eventStore, appendCount) <- newCountedEventStore
      outcome <- Task.fromIO
        ( GhcException.try @GhcException.SomeException
            (runWebProbeRequest eventStore "decoder-failure" Uuid.nil)
        )

      case outcome of
        GhcPrelude.Left exception -> do
          let exceptionText = exception |> GhcPrelude.show |> Text.fromLinkedList
          Text.contains "private-command-decoder-sentinel" exceptionText |> shouldBe True
        GhcPrelude.Right _ -> fail "Expected the command decoder exception to escape the WAI request"

      appendCountValue <- AtomicVar.peek appendCount
      appendCountValue |> shouldBe 0
      eventStore.close

    it "reports one safe result when a recognized command decoder is cancelled" \_ -> do
      (eventStore, appendCount) <- newCountedEventStore
      decodeStarted <- Task.fromIO GhcMVar.newEmptyMVar
      decodeRelease <- Task.fromIO GhcMVar.newEmptyMVar
      requestOutcome <- Task.fromIO GhcMVar.newEmptyMVar
      Var.set (Just (decodeStarted, decodeRelease)) decodeGate

      let cleanup = do
            _ <- GhcMVar.tryPutMVar decodeRelease () |> Task.fromIO
            Var.set Nothing decodeGate
            eventStore.close
      Task.finally cleanup do
        requestThread <- Task.fromIO do
          GhcConcurrent.forkIO do
            outcome <-
              GhcException.try @GhcException.SomeException
                (runWebProbeRequest eventStore "decoder-blocked" Uuid.nil)
            GhcMVar.putMVar requestOutcome outcome

        decodeStart <- Task.fromIO (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar decodeStarted))
        case decodeStart of
          GhcPrelude.Nothing -> do
            GhcConcurrent.killThread requestThread |> Task.fromIO
            fail "The request did not enter command decoding"
          GhcPrelude.Just () -> pass

        GhcConcurrent.killThread requestThread |> Task.fromIO
        maybeOutcome <- Task.fromIO (GhcTimeout.timeout 5000000 (GhcMVar.takeMVar requestOutcome))
        case maybeOutcome of
          GhcPrelude.Nothing -> fail "The canceled command request did not finish"
          GhcPrelude.Just (GhcPrelude.Left exception) ->
            case GhcException.fromException exception :: GhcPrelude.Maybe GhcException.AsyncException of
              GhcPrelude.Just GhcException.ThreadKilled -> pass
              GhcPrelude.Just _ -> fail "Expected ThreadKilled from the canceled command request"
              GhcPrelude.Nothing -> fail "Expected command cancellation to escape the WAI request"
          GhcPrelude.Just (GhcPrelude.Right _) -> fail "Expected cancellation to escape the WAI request"

        appendCountValue <- AtomicVar.peek appendCount
        appendCountValue |> shouldBe 0

    it "keeps one result when a recognized Web command fails during execution" \_ -> do
      (eventStore, appendCount) <- newCountedEventStore
      let failedFetchStore =
            eventStore
              { EventStore.readAllStreamEvents = \_ _ ->
                  Task.fromIO (GhcException.throwIO (GhcException.ErrorCall "private-web-fetch-sentinel"))
              }
      outcome <- Task.fromIO
        ( GhcException.try @GhcException.SomeException
            (runWebProbeRequest failedFetchStore "valid" Uuid.nil)
        )

      case outcome of
        GhcPrelude.Left exception -> do
          let exceptionText = exception |> GhcPrelude.show |> Text.fromLinkedList
          Text.contains "private-web-fetch-sentinel" exceptionText |> shouldBe True
        GhcPrelude.Right _ -> fail "Expected the Web command execution exception to escape the request"

      appendCountValue <- AtomicVar.peek appendCount
      appendCountValue |> shouldBe 0
      eventStore.close


requestBodyEndpoints :: EndpointHandler -> Transport.Endpoints Web.WebTransport
requestBodyEndpoints handler =
  Transport.Endpoints
    { Transport.transport = Web.server
    , Transport.commandEndpoints = Map.fromArray [("BodyProbe", handler)]
    , Transport.queryEndpoints = Map.empty
    , Transport.commandSchemas = Map.empty
    , Transport.querySchemas = Map.empty
    }


recordHandlerCall :: Var Bool -> EndpointHandler
recordHandlerCall wasCalled _requestContext _body _respond =
  Var.set True wasCalled


neverCalledHandler :: EndpointHandler
neverCalledHandler _requestContext _body _respond = Task.yield ()


throwingBodyRequest :: Wai.Request
throwingBodyRequest =
  commandRequest
    { Wai.requestBody =
        GhcException.throwIO (GhcException.ErrorCall "private-body-read-details")
    }


blockedBodyRequest :: GhcMVar.MVar () -> GhcMVar.MVar GhcBS.ByteString -> Wai.Request
blockedBodyRequest bodyReadStarted bodyRelease =
  commandRequest
    { Wai.requestBody = do
        GhcMVar.putMVar bodyReadStarted ()
        GhcMVar.takeMVar bodyRelease
    }


commandRequest :: Wai.Request
commandRequest =
  Wai.defaultRequest
    { Wai.requestMethod = HTTPMethod.methodPost
    , Wai.rawPathInfo = "/commands/body-probe"
    , Wai.pathInfo = ["commands", "body-probe"]
    }


webApplication :: Transport.Endpoints Web.WebTransport -> Wai.Application
webApplication endpoints request respond = do
  let runnable = Transport.assembleTransport endpoints
  runnable request (\response -> Task.fromIO (respond response))
    |> Task.runOrPanic


newCountedEventStore :: Task Text (EventStore.EventStore Json.Value, AtomicVar Int)
newCountedEventStore = do
  eventStore <- InMemory.new |> Task.mapError toText
  appendCount <- AtomicVar.containing 0
  let countedEventStore =
        eventStore
          { EventStore.insert = \payload -> do
              AtomicVar.modify (\count -> count + 1) appendCount
              eventStore.insert payload
          }
  Task.yield (countedEventStore, appendCount)


runWebProbeRequest :: EventStore.EventStore Json.Value -> Text -> Uuid -> IO WaiTest.SResponse
runWebProbeRequest eventStore mode probeId =
  WaiTest.runSession
    ( WaiTest.srequest
        ( WaiTest.SRequest
            webProbeRequest
            ( Json.object
                [ "mode" Json..= mode
                , "probeId" Json..= probeId
                ]
                |> Json.encodeText
                |> Text.toBytes
                |> Bytes.unwrap
                |> GhcLBS.fromStrict
            )
        )
    )
    (webProbeApplication eventStore)


webProbeApplication :: EventStore.EventStore Json.Value -> Wai.Application
webProbeApplication eventStore request respond = do
  endpoints <- Task.runOrPanic (webProbeEndpoints eventStore)
  let runnable = Transport.assembleTransport endpoints
  runnable request (\response -> Task.fromIO (respond response))
    |> Task.runOrPanic


webProbeEndpoints :: EventStore.EventStore Json.Value -> Task Text (Transport.Endpoints Web.WebTransport)
webProbeEndpoints eventStore = do
  let serviceRunner =
        Service.new
          |> Service.command @WebDecodeProbeCommand
          |> Service.toServiceRunner
  let transports =
        Map.fromArray
          [ ("WebTransport", ServiceDefinition.TransportValue Web.server)
          ]
  (handlersByTransport, schemasByTransport, _) <- serviceRunner.getEndpointsByTransport eventStore transports
  let commandEndpoints = handlersByTransport |> Map.get "WebTransport" |> Maybe.withDefault Map.empty
  let commandSchemas = schemasByTransport |> Map.get "WebTransport" |> Maybe.withDefault Map.empty
  Task.yield
    Transport.Endpoints
      { Transport.transport = Web.server
      , Transport.commandEndpoints = commandEndpoints
      , Transport.queryEndpoints = Map.empty
      , Transport.commandSchemas = commandSchemas
      , Transport.querySchemas = Map.empty
      }


webProbeRequest :: Wai.Request
webProbeRequest =
  Wai.defaultRequest
    { Wai.requestMethod = HTTPMethod.methodPost
    , Wai.rawPathInfo = "/commands/web-decode-probe-command"
    , Wai.pathInfo = ["commands", "web-decode-probe-command"]
    }
