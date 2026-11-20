module Service.Transport.Web.BindSpec where

import Array qualified
import AtomicVar (AtomicVar)
import AtomicVar qualified
import AsyncTask qualified
import Bytes qualified
import Control.Concurrent.MVar qualified as GhcMVar
import Core
import Decider qualified
import Http.Client qualified as Http
import Http.Client.Internal qualified as HttpInternal
import Integration qualified
import Json qualified
import Maybe qualified
import Network.HTTP.Types qualified as GhcHttp
import Network.Socket qualified as GhcSocket
import Network.Wai qualified as GhcWai
import Network.Wai.Handler.Warp qualified as GhcWarp
import Service.Application (Application (..))
import Service.Application qualified as Application
import Service.Auth (RequestContext)
import Service.Command.Core (Event (..), TransportsOf)
import Service.CommandExecutor.TH (deriveKnownHash)
import Service.EventModel qualified as EventModel
import Service.EventStore (EventStore (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.Transport (Transport (..))
import Service.Transport.Web (WebTransport (..), applyBindOverrides, hostPreference, server, validateBindHost, validateBindPort, warpSettings)
import System.IO.Error (IOError)
import System.Timeout qualified as GhcTimeout
import Task qualified
import Test
import Text qualified
import Uuid qualified


data CleanupProbeCommand = CleanupProbeCommand
  deriving (Generic)


instance Json.ToJSON CleanupProbeCommand


type instance NameOf CleanupProbeCommand = "CleanupProbeCommand"


data WorkerFixture = WorkerFixture
  { started :: GhcMVar.MVar Int
  , release :: GhcMVar.MVar Unit
  , stopped :: GhcMVar.MVar Unit
  , barrierPassed :: GhcMVar.MVar Unit
  }


data ABindCleanupBarrier = ABindCleanupBarrier WorkerFixture


type instance NameOf ABindCleanupBarrier = "ABindCleanupBarrier"


deriveKnownHash "ABindCleanupBarrier"


data BindModelEntity = BindModelEntity
  { bindModelEntityId :: Uuid
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON BindModelEntity
instance Json.FromJSON BindModelEntity


data BindModelEvent = BindModelEventCreated Uuid
  deriving (Eq, Show, Generic)


instance Json.ToJSON BindModelEvent
instance Json.FromJSON BindModelEvent


type instance NameOf BindModelEntity = "BindModelEntity"
type instance EventOf BindModelEntity = BindModelEvent
type instance EntityOf BindModelEvent = BindModelEntity


instance Entity BindModelEntity where
  initialStateImpl = BindModelEntity {bindModelEntityId = Uuid.nil}
  updateImpl ev _ = case ev of
    BindModelEventCreated uid -> BindModelEntity {bindModelEntityId = uid}


instance Event BindModelEvent where
  getEventEntityIdImpl ev = case ev of
    BindModelEventCreated uid -> uid


data BindModelCommand = BindModelCommand
  deriving (Generic)


type instance EntityOf BindModelCommand = BindModelEntity
type instance TransportsOf BindModelCommand = '[WebTransport]


getEntityId :: BindModelCommand -> Maybe Uuid
getEntityId _ = Nothing


decide :: BindModelCommand -> Maybe BindModelEntity -> RequestContext -> Decision BindModelEvent
decide _ _ _ = Decider.acceptNew [BindModelEventCreated Uuid.nil]


deriveCommand ''BindModelCommand


bindFixtureFeatures :: EventModel.FeatureAction
bindFixtureFeatures = EventModel.feature "Bind" do
  EventModel.chapter "Commands" do
    EventModel.slice "Probe" do
      EventModel.entity @BindModelEntity do
        EventModel.command @BindModelCommand do
          pass


spec :: Spec Unit
spec = do
  describe "WebTransport bind host and port" do
    describe "defaults" do
      it "server binds all IPv4 interfaces by default (unchanged from Warp.run)" \_ -> do
        server.host |> shouldBe "*4"

      it "server listens on port 8080 by default" \_ -> do
        server.port |> shouldBe 8080

    describe "Application builders" do
      it "withHost records the bind host" \_ -> do
        let app = Application.new |> Application.withHost "127.0.0.1"
        app.bindHost |> shouldBe (Just "127.0.0.1")

      it "withPort records the bind port" \_ -> do
        let app = Application.new |> Application.withPort 9090
        app.bindPort |> shouldBe (Just 9090)

      it "bind host and port are unset on a new Application" \_ -> do
        Application.new.bindHost |> shouldBe Nothing
        Application.new.bindPort |> shouldBe Nothing

    describe "EventModel builders" do
      it "EventModel.withHost and EventModel.withPort lower onto Application" \_ -> do
        let assembled = EventModel.eventModel do
              EventModel.appName "BindFixture"
              EventModel.withTransport server
              EventModel.withHost "127.0.0.1"
              EventModel.withPort 9192
              EventModel.features do
                bindFixtureFeatures
        case assembled of
          Err problem -> fail [fmt|unexpected EventModel error: #{problem}|]
          Ok model -> do
            let app = EventModel.toApplication model
            app.bindHost |> shouldBe (Just "127.0.0.1")
            app.bindPort |> shouldBe (Just 9192)

      it "binds loopback and custom port assembled through EventModel" \_ -> do
        freePort <- getOwnedFreePort
        let assembled = EventModel.eventModel do
              EventModel.appName "BindRuntimeFixture"
              EventModel.withTransport server
              EventModel.withHost "127.0.0.1"
              EventModel.withPort freePort
              EventModel.features do
                bindFixtureFeatures
        case assembled of
          Err problem -> fail [fmt|unexpected EventModel error: #{problem}|]
          Ok model -> withOwnedBindAt freePort (EventModel.toApplication model)

    describe "hostPreference" do
      it "maps a literal IPv4 address to that host" \_ -> do
        show (hostPreference "127.0.0.1") |> shouldBe "Host \"127.0.0.1\""

      it "maps * to all interfaces" \_ -> do
        show (hostPreference "*") |> shouldBe "HostAny"

      it "maps *4 to all IPv4 interfaces" \_ -> do
        show (hostPreference "*4") |> shouldBe "HostIPv4"

      it "maps *6 to all IPv6 interfaces" \_ -> do
        show (hostPreference "*6") |> shouldBe "HostIPv6"

      it "maps a literal IPv6 address to that host" \_ -> do
        show (hostPreference "::1") |> shouldBe "Host \"::1\""
        show (hostPreference "[::1]") |> shouldBe "Host \"::1\""
        show (hostPreference "[2001:db8::8]") |> shouldBe "Host \"2001:db8::8\""

      it "does not unwrap bracketed names or wildcard preferences" \_ -> do
        show (hostPreference "[]") |> shouldBe "Host \"[]\""
        show (hostPreference "[*]") |> shouldBe "Host \"[*]\""
        show (hostPreference "[!6]") |> shouldBe "Host \"[!6]\""
        show (hostPreference "[localhost]") |> shouldBe "Host \"[localhost]\""

    describe "override composition" do
      it "withHost and withPort apply whether they come before or after withTransport" \_ -> do
        let before =
              Application.new
                |> Application.withHost "127.0.0.1"
                |> Application.withPort 9191
                |> Application.withTransport server
        let after =
              Application.new
                |> Application.withTransport server
                |> Application.withHost "127.0.0.1"
                |> Application.withPort 9191
        let bound app = applyBindOverrides app.bindHost app.bindPort server
        (bound before).host |> shouldBe "127.0.0.1"
        (bound before).port |> shouldBe 9191
        (bound after).host |> shouldBe "127.0.0.1"
        (bound after).port |> shouldBe 9191

      it "absent overrides preserve the transport's custom host and port" \_ -> do
        let untouched = applyBindOverrides Nothing Nothing customTransport
        untouched.host |> shouldBe "::1"
        untouched.port |> shouldBe 7070

      it "a host-only override keeps the transport's custom port" \_ -> do
        let hostOnly = applyBindOverrides (Just "127.0.0.1") Nothing customTransport
        hostOnly.host |> shouldBe "127.0.0.1"
        hostOnly.port |> shouldBe 7070

      it "a port-only override keeps the transport's custom host" \_ -> do
        let portOnly = applyBindOverrides Nothing (Just 9191) customTransport
        portOnly.host |> shouldBe "::1"
        portOnly.port |> shouldBe 9191

    describe "validateBindHost" do
      it "accepts the Warp wildcards and bare literal addresses or hostnames" \_ -> do
        let accepted = ["*", "*4", "*6", "127.0.0.1", "0.0.0.0", "::1", "::", "fe80::1", "localhost", "app.internal"]
        let rejected = accepted |> Array.takeIf (\candidate -> validateBindHost candidate != Nothing)
        rejected |> shouldBe []

      it "rejects an empty or blank host" \_ -> do
        validateBindHost "" |> shouldSatisfy (\reason -> reason != Nothing)
        validateBindHost "   " |> shouldSatisfy (\reason -> reason != Nothing)

      it "rejects a URL pasted into the host field" \_ -> do
        let reason = validateBindHost "http://127.0.0.1" |> Maybe.withDefault ""
        reason |> shouldSatisfy (Text.contains "URL")

      it "rejects a host that carries a port" \_ -> do
        let reason = validateBindHost "127.0.0.1:8080" |> Maybe.withDefault ""
        reason |> shouldSatisfy (Text.contains "withPort")

      it "rejects bracketed IPv6 host-port before startup" \_ -> do
        validateBindHost "::1" |> shouldBe Nothing
        case validateBindHost "[::1]:8080" of
          Nothing -> fail "REGRESSION_ASSERT:bracketed-host-port-accepted"
          Just reason -> reason |> shouldSatisfy (Text.contains "withPort")

      it "preserves balanced bracket hosts and strict-family wildcard validation" \_ -> do
        validateBindHost "[::1]" |> shouldBe Nothing
        validateBindHost "[2001:db8::8]" |> shouldBe Nothing
        validateBindHost "!4" |> shouldBe Nothing
        validateBindHost "!6" |> shouldBe Nothing

      it "rejects malformed brackets and preserves bare IPv6" \_ -> do
        validateBindHost "fe80::1" |> shouldBe Nothing
        let bracketed = ["[", "]", "[]", "[*]", "[!6]", "[localhost]", "[127.0.0.1]", "[::1", "::1]", "[::1]trailing"]
        let accepted = bracketed |> Array.takeIf (\candidate -> validateBindHost candidate == Nothing)
        accepted |> shouldBe []

      it "rejects a host with whitespace inside" \_ -> do
        validateBindHost "127.0.0.1 8080" |> shouldSatisfy (\reason -> reason != Nothing)

      it "rejects a host with leading or trailing spaces" \_ -> do
        validateBindHost "127.0.0.1" |> shouldBe Nothing
        let padded = [" 127.0.0.1", "127.0.0.1 ", " 127.0.0.1 "]
        let accepted = padded |> Array.takeIf (\candidate -> isWhitespaceRejection candidate |> not)
        accepted |> shouldBe []

      it "rejects a host surrounded by tabs or newlines" \_ -> do
        validateBindHost "localhost" |> shouldBe Nothing
        let padded = ["\tlocalhost", "localhost\n", "\r\nlocalhost\t"]
        let accepted = padded |> Array.takeIf (\candidate -> isWhitespaceRejection candidate |> not)
        accepted |> shouldBe []

    describe "validateBindPort" do
      it "accepts the full 1..65535 range" \_ -> do
        validateBindPort 1 |> shouldBe Nothing
        validateBindPort 8080 |> shouldBe Nothing
        validateBindPort 65535 |> shouldBe Nothing

      it "rejects port 0 and ports outside the range" \_ -> do
        let reason = validateBindPort 0 |> Maybe.withDefault ""
        reason |> shouldSatisfy (Text.contains "random port")
        validateBindPort (-1) |> shouldSatisfy (\outOfRange -> outOfRange != Nothing)
        validateBindPort 65536 |> shouldSatisfy (\outOfRange -> outOfRange != Nothing)

    describe "warpSettings" do
      it "carries the configured host and port into Warp" \_ -> do
        let transport = server {host = "127.0.0.1", port = 9191}
        let settings = warpSettings transport
        show (GhcWarp.getHost settings) |> shouldBe "Host \"127.0.0.1\""
        GhcWarp.getPort settings |> shouldBe 9191

    describe "Application.runWith registered bind settings" do
      it "serves the owned application on bare and bracketed IPv6 loopback" \_ -> do
        ["::1", "[::1]"] |> Task.forEach \host -> do
          eventStore <- InMemory.new |> Task.mapError toText
          freePort <- getOwnedFreePort
          (app, hits, body) <- newIdentityApplication
            (Application.new
              |> Application.withTransport server
              |> Application.withHost host
              |> Application.withPort freePort)
          let probe = do
                owned <- probeOwnedInstanceAt "[::1]" hits body freePort 50
                owned |> shouldBe True
          let run = AsyncTask.process (Application.runWith eventStore app) \running -> do
                outcome <- AsyncTask.race (AsyncTask.waitCatch running) probe
                case outcome of
                  AsyncTask.LeftWon result -> Task.throw [fmt|IPv6 Application ended early for #{host}: #{show result}|]
                  AsyncTask.RightWon _ -> pass
          run |> Task.finally eventStore.close

      it "applies host and port set before withTransport at runtime" \_ -> do
        withHeldLoopbackPort (GhcSocket.tupleToHostAddress (127, 0, 0, 1)) \occupiedPort -> do
          freePort <- getDifferentFreePort occupiedPort
          let custom = server {host = "*4", port = occupiedPort}
          let app = Application.new
                |> Application.withHost "127.0.0.1"
                |> Application.withPort freePort
                |> Application.withTransport custom
          withOwnedBindAt freePort app

      it "applies host and port set after withTransport at runtime" \_ -> do
        withHeldLoopbackPort (GhcSocket.tupleToHostAddress (127, 0, 0, 1)) \occupiedPort -> do
          freePort <- getDifferentFreePort occupiedPort
          let custom = server {host = "*4", port = occupiedPort}
          let app = Application.new
                |> Application.withTransport custom
                |> Application.withHost "127.0.0.1"
                |> Application.withPort freePort
          withOwnedBindAt freePort app

      it "preserves a custom registered host and port with absent overrides at runtime" \_ -> do
        freePort <- getOwnedFreePort
        let custom = server {host = "127.0.0.1", port = freePort}
        let app = Application.new |> Application.withTransport custom
        withOwnedBindAt freePort app

      it "preserves a custom registered port with a host-only override at runtime" \_ -> do
        freePort <- getOwnedFreePort
        let custom = server {host = "*4", port = freePort}
        let app = Application.new
              |> Application.withTransport custom
              |> Application.withHost "127.0.0.1"
        withOwnedBindAt freePort app

      it "preserves a custom registered host with a port-only override at runtime" \_ -> do
        withHeldLoopbackPort (GhcSocket.tupleToHostAddress (127, 0, 0, 1)) \occupiedPort -> do
          freePort <- getDifferentFreePort occupiedPort
          let custom = server {host = "127.0.0.1", port = occupiedPort}
          let app = Application.new
                |> Application.withTransport custom
                |> Application.withPort freePort
          withOwnedBindAt freePort app

    describe "owned runtime probe controls" do
      it "accepts the intended Application and excludes another owned interface" \_ -> do
        _ <- ownedNonLoopbackAddress
        freePort <- getOwnedFreePort
        let app = Application.new
              |> Application.withTransport server
              |> Application.withHost "127.0.0.1"
              |> Application.withPort freePort
        withOwnedBindAt freePort app

      it "a wildcard listener defeats the owned-interface bind probe" \_ -> do
        otherAddress <- ownedNonLoopbackAddress
        eventStore <- InMemory.new |> Task.mapError toText
        freePort <- getOwnedFreePort
        (app, hits, body) <- newIdentityApplication
          (Application.new
            |> Application.withTransport server
            |> Application.withPort freePort)
        let probe = do
              owned <- probeOwnedInstance hits body freePort 50
              owned |> shouldBe True
              available <- canBindAddress otherAddress freePort
              available |> shouldBe False
        let run = AsyncTask.process (Application.runWith eventStore app) \running -> do
              outcome <- AsyncTask.race (AsyncTask.waitCatch running) probe
              case outcome of
                AsyncTask.LeftWon result -> Task.throw [fmt|wildcard Application ended early: #{show result}|]
                AsyncTask.RightWon _ -> pass
        run |> Task.finally eventStore.close

      it "rejects an unrelated catch-all 200 after the intended Application bind fails" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        (identified, hits, body) <- newIdentityApplication Application.new
        -- Even copying the expected body is insufficient to impersonate the
        -- owned query handler. The control must actually receive a request.
        competitorHits <- AtomicVar.containing (0 :: Int)
        let competitor _ respond = do
              Task.runNoErrors (AtomicVar.modify (+ 1) competitorHits)
              respond (GhcWai.responseLBS GhcHttp.status200 []
                (body |> Text.toBytes |> Bytes.toLazyLegacy))
        let exercise = do
              outcome <- Task.fromIO do
                GhcWarp.testWithApplication (pure competitor) \port -> Task.runResult do
                  let app = identified
                        |> Application.withTransport server
                        |> Application.withHost "127.0.0.1"
                        |> Application.withPort port
                  -- First prove the intended Application cannot bind. Do not
                  -- assume task failure merely because a probe rejected it.
                  result <- Application.runWith eventStore app |> Task.asResult
                  case result of
                    Ok _ -> fail "intended Application unexpectedly took the competitor's port"
                    Err err -> err |> shouldSatisfy
                      (Text.contains [fmt|could not bind 127.0.0.1:#{port}|])
                  owned <- probeOwnedInstance hits body port 0
                  owned |> shouldBe False
                  count <- AtomicVar.peek hits
                  count |> shouldBe 0
                  received <- AtomicVar.peek competitorHits
                  received |> shouldSatisfy (\n -> n > 0)
              case outcome of
                Err err -> Task.throw err
                Ok _ -> pass
        exercise |> Task.finally eventStore.close

    describe "Application returned bind failure owns inbound shutdown" do
      it "closes a started inbound socket before returning the bind error" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        closeCalled <- AtomicVar.containing False
        let callerStore = eventStore
              { close = do
                  AtomicVar.set True closeCalled
                  eventStore.close
              }
        let exercise = withWorkerFixture \fixture -> do
              withHeldLoopbackPort loopback \occupiedPort -> do
                let app = Application.new
                      |> Application.withInbound @() (\_ -> ownedInbound fixture)
                      |> Application.withTransport (ABindCleanupBarrier fixture)
                      |> Application.withTransport server
                      |> Application.withHost "127.0.0.1"
                      |> Application.withPort occupiedPort
                result <-
                  Application.runWith callerStore app
                    |> Task.asResult
                    |> bounded "Application bind failure"
                case result of
                  Ok _ -> fail "Application unexpectedly bound the held port"
                  Err err -> err |> shouldSatisfy
                    (Text.contains [fmt|could not bind 127.0.0.1:#{occupiedPort}|])
                passed <- GhcMVar.tryReadMVar fixture.barrierPassed |> Task.fromIO
                passed |> shouldBe (Just unit)
                -- Caller-owned EventStore closure is not the shutdown evidence.
                closed <- AtomicVar.peek closeCalled
                closed |> shouldBe False
                assertWorkerStopped fixture
        exercise |> Task.finally callerStore.close

      it "the same worker stays live until explicitly released without Application cleanup" \_ -> do
        withWorkerFixture \fixture -> do
          let worker = Integration.runInbound (ownedInbound fixture) (\_ -> Task.yield unit)
                |> Task.mapError toText
          AsyncTask.process worker \running -> do
            _ <- assertWorkerLive fixture
            GhcMVar.putMVar fixture.release unit |> Task.fromIO
            -- Normal successful completion, not intentional cancellation.
            AsyncTask.waitFor running |> bounded "control worker completion"
            assertWorkerStopped fixture

    describe "running" do
      it "serves /health on loopback when bound to 127.0.0.1" \_ -> do
        -- A unique path alone is not identity; the owned handler must also run.
        eventStore <- InMemory.new |> Task.mapError toText
        freePort <- getFreePort
        healthPath <- uniqueHealthPath
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withHost "127.0.0.1"
                |> Application.withPort freePort
                |> Application.withHealthCheck @() (\_ -> healthPath)
        (ownedApp, hits, body) <- newIdentityApplication app
        running <- AsyncTask.run (Application.runWith eventStore ownedApp)
        let cancelApp = AsyncTask.cancel running |> Task.ignoreError
        let probe = do
              owned <- probeOwnedInstance hits body freePort 50
              owned |> shouldBe True
              status <- getHealthStatus healthPath freePort 50
              status |> shouldBe 200
              -- Loopback isolation: the port is still free on a non-loopback
              -- address, which a wildcard listener would have taken as well.
              nonLoopback <- nonLoopbackAddress
              canBind <- canBindElsewhere nonLoopback freePort
              canBind |> shouldBe True
        probe |> Task.finally cancelApp

      it "a wildcard bind occupies the port on every address (negative control for the loopback probe)" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        freePort <- getFreePort
        healthPath <- uniqueHealthPath
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withPort freePort
                |> Application.withHealthCheck @() (\_ -> healthPath)
        (ownedApp, hits, body) <- newIdentityApplication app
        running <- AsyncTask.run (Application.runWith eventStore ownedApp)
        let cancelApp = AsyncTask.cancel running |> Task.ignoreError
        let probe = do
              owned <- probeOwnedInstance hits body freePort 50
              owned |> shouldBe True
              status <- getHealthStatus healthPath freePort 50
              status |> shouldBe 200
              nonLoopback <- nonLoopbackAddress
              canBind <- canBindElsewhere nonLoopback freePort
              canBind |> shouldBe False
        probe |> Task.finally cancelApp

      it "rejects invalid static bind before run acquires an event store" \_ -> do
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withPort 0
        result <- Application.run app |> Task.asResult
        result |> shouldBe (Err "Application.withPort: bind port 0 asks the OS for a random port that nothing reports back; choose a fixed port between 1 and 65535")

      it "rejects invalid static bind before owned auth discovery" \_ -> do
        requests <- AtomicVar.containing (0 :: Int)
        let discovery _ respond = do
              Task.runNoErrors (AtomicVar.modify (+ 1) requests)
              respond (GhcWai.responseLBS GhcHttp.status200 [] "not-json")
        (appOutcome, count) <- Task.fromIOResult do
          GhcWarp.testWithApplication (pure discovery) \authPort -> Task.runResult do
            -- The owned fixture is live before the invalid application starts.
            status <- getHealthStatus "fixture-control" authPort 0
            status |> shouldBe 200
            AtomicVar.set 0 requests
            eventStore <- InMemory.new |> Task.mapError toText
            let app =
                  Application.new
                    |> Application.withTransport server
                    |> Application.withHost "http://127.0.0.1"
                    |> Application.withAuth @() (\_ -> [fmt|http://127.0.0.1:#{authPort}|])
            outcome <- Application.runWith eventStore app |> Task.asResult |> Task.asResultSafe @Text
            observed <- AtomicVar.peek requests
            Task.yield (outcome, observed)
        case count of
          0 -> pass
          _ -> fail "REGRESSION_ASSERT:invalid-bind-contacted-auth"
        case appOutcome of
          Err err -> fail [fmt|unexpected startup exception: #{err}|]
          Ok appResult -> case appResult of
            Ok _ -> fail "runWith accepted an invalid host"
            Err err -> err |> shouldBe "Application.withHost: bind host \"http://127.0.0.1\" looks like a URL or path; use a bare address such as \"127.0.0.1\", not \"http://127.0.0.1\""

      it "an invalid host fails before any socket is opened" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withHost "http://127.0.0.1"
        result <- Application.runWith eventStore app |> Task.asResult
        case result of
          Ok _ -> fail "runWith accepted a URL as the bind host"
          Err err -> err |> shouldSatisfy (Text.startsWith "Application.withHost:")

      it "an invalid port fails before any socket is opened" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withPort 0
        result <- Application.runWith eventStore app |> Task.asResult
        case result of
          Ok _ -> fail "runWith accepted port 0"
          Err err -> err |> shouldSatisfy (Text.startsWith "Application.withPort:")

      it "a port already in use fails with an error naming the host and port" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        (occupied, occupiedPort) <- occupyLoopbackPort
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withHost "127.0.0.1"
                |> Application.withPort occupiedPort
        let attempt = do
              result <- Application.runWith eventStore app |> Task.asResult
              case result of
                Ok _ -> fail "runWith started on a port that was already in use"
                Err err -> err |> shouldSatisfy (Text.contains [fmt|could not bind 127.0.0.1:#{occupiedPort}|])
        attempt |> Task.finally (GhcSocket.close occupied |> Task.fromIO)


-- A successful response must have run this instance's handler. The body alone
-- is not sufficient: the catch-all negative control deliberately copies it.
newIdentityApplication :: Application -> Task Text (Application, AtomicVar Int, Text)
newIdentityApplication app = do
  nonce <- uniqueHealthPath
  hits <- AtomicVar.containing (0 :: Int)
  let body = Json.encodeText nonce
  let handler _ _ _ = do
        AtomicVar.modify (+ 1) hits
        Task.yield body
  Task.yield (app |> Application.withQueryEndpoint "BindProbe" handler, hits, body)


probeOwnedInstance :: AtomicVar Int -> Text -> Int -> Int -> Task Text Bool
probeOwnedInstance hits expectedBody port attemptsLeft =
  probeOwnedInstanceAt "127.0.0.1" hits expectedBody port attemptsLeft


probeOwnedInstanceAt :: Text -> AtomicVar Int -> Text -> Int -> Int -> Task Text Bool
probeOwnedInstanceAt host hits expectedBody port attemptsLeft = do
  result <-
    Http.request
      |> Http.withUrl [fmt|http://#{host}:#{port}/queries/bind-probe|]
      |> Http.withTimeout 2
      |> HttpInternal.getRaw
      |> Task.asResult
  case result of
    Ok response -> do
      count <- AtomicVar.peek hits
      Task.yield
        ( response.statusCode == 200
            && Text.fromBytes response.body == expectedBody
            && count > 0
        )
    Err _ ->
      if attemptsLeft <= 0 then
        Task.throw "owned Application listener never answered"
      else do
        AsyncTask.sleep 100
        probeOwnedInstanceAt host hits expectedBody port (attemptsLeft - 1)


-- Never treat nonLoopbackAddress's fallback as a distinct-interface proof.
ownedNonLoopbackAddress :: Task Text GhcSocket.HostAddress
ownedNonLoopbackAddress = do
  address <- nonLoopbackAddress
  let (firstOctet, _, _, _) = GhcSocket.hostAddressToTuple address
  firstOctet |> shouldSatisfy (\octet -> octet != 127 && octet != 0)
  preflight <- canBindAddress address 0
  preflight |> shouldBe True
  Task.yield address


assertLiteralLoopbackBind :: Int -> Task Text Unit
assertLiteralLoopbackBind port = do
  address <- ownedNonLoopbackAddress
  available <- canBindAddress address port
  available |> shouldBe True


withOwnedBindAt :: Int -> Application -> Task Text Unit
withOwnedBindAt port configured = do
  eventStore <- InMemory.new |> Task.mapError toText
  (app, hits, body) <- newIdentityApplication configured
  let probe = do
        owned <- probeOwnedInstance hits body port 50
        owned |> shouldBe True
        assertLiteralLoopbackBind port
  let run =
        AsyncTask.process (Application.runWith eventStore app) \running -> do
          outcome <- AsyncTask.race (AsyncTask.waitCatch running) probe
          case outcome of
            AsyncTask.LeftWon result ->
              Task.throw [fmt|Application ended before the owned bind probe: #{show result}|]
            AsyncTask.RightWon _ -> pass
  run |> Task.finally eventStore.close


withAcquiredSocket :: (GhcSocket.Socket -> Task Text value) -> Task Text value
withAcquiredSocket useSocket =
  Task.withCancellationDeferred \restore -> do
    sock <-
      GhcSocket.socket GhcSocket.AF_INET GhcSocket.Stream GhcSocket.defaultProtocol
        |> Task.fromFailableIO @IOError
        |> Task.mapError toText
    restore (useSocket sock)
      |> Task.finally (GhcSocket.close sock |> Task.fromIO)


-- Catch only IOError; asynchronous cancellation propagates while closing the socket.
canBindAddress :: GhcSocket.HostAddress -> Int -> Task Text Bool
canBindAddress address port =
  withAcquiredSocket \sock -> do
    outcome <-
      GhcSocket.bind sock (GhcSocket.SockAddrInet (fromIntegral port) address)
        |> Task.fromFailableIO @IOError
        |> Task.asResult
    case outcome of
      Ok _ -> Task.yield True
      Err _ -> Task.yield False


-- The loopback port reservation is held throughout action.
withHeldLoopbackPort :: GhcSocket.HostAddress -> (Int -> Task Text value) -> Task Text value
withHeldLoopbackPort address action =
  withAcquiredSocket \sock -> do
    GhcSocket.bind sock (GhcSocket.SockAddrInet 0 address) |> Task.fromIO
    GhcSocket.listen sock 1 |> Task.fromIO
    port <- GhcSocket.socketPort sock |> Task.fromIO
    action (fromIntegral port)


getOwnedFreePort :: Task Text Int
getOwnedFreePort =
  withHeldLoopbackPort (GhcSocket.tupleToHostAddress (127, 0, 0, 1)) Task.yield


getDifferentFreePort :: Int -> Task Text Int
getDifferentFreePort excluded = do
  port <- getOwnedFreePort
  if port == excluded then getDifferentFreePort excluded else Task.yield port

instance Transport ABindCleanupBarrier where
  type Request ABindCleanupBarrier = Unit
  type Response ABindCleanupBarrier = Unit
  type RunnableTransport ABindCleanupBarrier = Unit

  assembleTransport _ = unit

  runTransport (ABindCleanupBarrier fixture) _ = do
    _ <- assertWorkerLive fixture
    GhcMVar.putMVar fixture.barrierPassed unit |> Task.fromIO

  buildHandler _ _ _ _ _ _ =
    Task.throw "ABindCleanupBarrier has no command endpoints"


loopback :: GhcSocket.HostAddress
loopback = GhcSocket.tupleToHostAddress (127, 0, 0, 1)


-- Limits failure duration, never substitutes elapsed time for an acknowledgment.
bounded :: Text -> Task Text value -> Task Text value
bounded label task = do
  outcome <- GhcTimeout.timeout 5000000 (Task.runResult task) |> Task.fromIO
  case outcome of
    Nothing -> Task.throw [fmt|#{label}: watchdog expired|]
    Just (Err err) -> Task.throw err
    Just (Ok value) -> Task.yield value


ownedInbound :: WorkerFixture -> Integration.Inbound
ownedInbound fixture =
  Integration.inbound @CleanupProbeCommand Integration.InboundConfig
    { run = \_emit ->
        Task.withCancellationDeferred \restore -> do
          sock <-
            GhcSocket.socket GhcSocket.AF_INET GhcSocket.Stream GhcSocket.defaultProtocol
              |> Task.fromFailableIO @IOError
              |> Task.mapError (toText .> Integration.UnexpectedError)
          let cleanup = do
                GhcSocket.close sock |> Task.fromIO
                GhcMVar.putMVar fixture.stopped unit |> Task.fromIO
          let runWorker = do
                GhcSocket.bind sock (GhcSocket.SockAddrInet 0 loopback) |> Task.fromIO
                GhcSocket.listen sock 1 |> Task.fromIO
                port <- GhcSocket.socketPort sock |> Task.fromIO
                GhcMVar.putMVar fixture.started (fromIntegral port) |> Task.fromIO
                GhcMVar.readMVar fixture.release |> Task.fromIO
          restore runWorker |> Task.finally cleanup
    }


assertWorkerLive :: WorkerFixture -> Task Text Int
assertWorkerLive fixture = do
  port <- GhcMVar.readMVar fixture.started |> Task.fromIO |> bounded "inbound start"
  stopped <- GhcMVar.tryReadMVar fixture.stopped |> Task.fromIO
  stopped |> shouldBe Nothing
  available <- canBindAddress loopback port
  available |> shouldBe False
  Task.yield port


assertWorkerStopped :: WorkerFixture -> Task Text Unit
assertWorkerStopped fixture = do
  -- Do not wait here: completion must already be observable at runWith return.
  stopped <- GhcMVar.tryReadMVar fixture.stopped |> Task.fromIO
  stopped |> shouldBe (Just unit)
  port <- GhcMVar.readMVar fixture.started |> Task.fromIO
  available <- canBindAddress loopback port
  available |> shouldBe True


withWorkerFixture :: (WorkerFixture -> Task Text value) -> Task Text value
withWorkerFixture action = do
  started <- GhcMVar.newEmptyMVar |> Task.fromIO
  release <- GhcMVar.newEmptyMVar |> Task.fromIO
  stopped <- GhcMVar.newEmptyMVar |> Task.fromIO
  barrierPassed <- GhcMVar.newEmptyMVar |> Task.fromIO
  let fixture = WorkerFixture {started, release, stopped, barrierPassed}
  let fallback = do
        -- Only runs AFTER assertions. A missing product cancellation must fail
        -- before this rescue releases the blocked worker.
        _ <- GhcMVar.tryPutMVar release unit |> Task.fromIO
        didStart <- GhcMVar.tryReadMVar started |> Task.fromIO
        case didStart of
          Nothing -> pass
          Just _ ->
            GhcMVar.readMVar stopped |> Task.fromIO |> bounded "fixture rescue"
  action fixture |> Task.finally fallback



-- | Does 'validateBindHost' reject this host because of whitespace?
isWhitespaceRejection :: Text -> Bool
isWhitespaceRejection candidate =
  validateBindHost candidate
    |> Maybe.withDefault ""
    |> Text.contains "whitespace"


-- | A transport with a custom host and port, for the single-setting cases.
customTransport :: WebTransport
customTransport = server {host = "::1", port = 7070}


-- | A health path no other test (or stray process) serves.
uniqueHealthPath :: Task Text Text
uniqueHealthPath = do
  uuid <- Uuid.generate
  let suffix = Uuid.toText uuid |> Text.replace "-" ""
  Task.yield [fmt|health-#{suffix}|]


-- | Ask the OS for a currently free loopback port.
getFreePort :: Task Text Int
getFreePort = do
  let hints = GhcSocket.defaultHints {GhcSocket.addrSocketType = GhcSocket.Stream}
  addrInfos <- GhcSocket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0") |> Task.fromIO
  case addrInfos of
    [] -> Task.throw "no loopback address available"
    (addr : _) -> do
      sock <- GhcSocket.openSocket addr |> Task.fromIO
      GhcSocket.bind sock (GhcSocket.addrAddress addr) |> Task.fromIO
      port <- GhcSocket.socketPort sock |> Task.fromIO
      GhcSocket.close sock |> Task.fromIO
      Task.yield (fromIntegral port)


-- | Hold a loopback port open so a later bind on it must fail.
occupyLoopbackPort :: Task Text (GhcSocket.Socket, Int)
occupyLoopbackPort = do
  let hints = GhcSocket.defaultHints {GhcSocket.addrSocketType = GhcSocket.Stream}
  addrInfos <- GhcSocket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0") |> Task.fromIO
  case addrInfos of
    [] -> Task.throw "no loopback address available"
    (addr : _) -> do
      sock <- GhcSocket.openSocket addr |> Task.fromIO
      GhcSocket.bind sock (GhcSocket.addrAddress addr) |> Task.fromIO
      GhcSocket.listen sock 1 |> Task.fromIO
      port <- GhcSocket.socketPort sock |> Task.fromIO
      Task.yield (sock, fromIntegral port)


-- | An IPv4 address of this machine that is not loopback. The OS fills it in
-- when a UDP socket is pointed at a routable address; nothing is sent.
nonLoopbackAddress :: Task Text GhcSocket.HostAddress
nonLoopbackAddress = Task.fromIO do
  sock <- GhcSocket.socket GhcSocket.AF_INET GhcSocket.Datagram GhcSocket.defaultProtocol
  GhcSocket.connect sock (GhcSocket.SockAddrInet 9 (GhcSocket.tupleToHostAddress (10, 255, 255, 255)))
  localName <- GhcSocket.getSocketName sock
  GhcSocket.close sock
  case localName of
    GhcSocket.SockAddrInet _ address -> pure address
    _ -> pure (GhcSocket.tupleToHostAddress (127, 0, 0, 1))


-- | Can a fresh TCP socket still bind @address:port@? True when the running
-- server only holds loopback, False when a wildcard listener owns the port on
-- every address. Warp sets SO_REUSEADDR, so this probe discriminates on
-- address, not on the reuse flag.
canBindElsewhere :: GhcSocket.HostAddress -> Int -> Task Text Bool
canBindElsewhere address port = canBindAddress address port


-- | GET the health path on loopback, retrying while the server is still binding.
getHealthStatus :: Text -> Int -> Int -> Task Text Int
getHealthStatus healthPath port attemptsLeft = do
  result <-
    Http.request
      |> Http.withUrl [fmt|http://127.0.0.1:#{port}/#{healthPath}|]
      |> Http.withTimeout 2
      |> HttpInternal.getRaw
      |> Task.asResult
  case result of
    Ok response -> Task.yield response.statusCode
    Err _ ->
      if attemptsLeft <= 0
        then Task.throw "/health never answered on the bound loopback port"
        else do
          AsyncTask.sleep 100
          getHealthStatus healthPath port (attemptsLeft - 1)
