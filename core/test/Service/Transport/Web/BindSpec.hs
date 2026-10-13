module Service.Transport.Web.BindSpec where

import Array qualified
import AsyncTask qualified
import Core
import Http.Client qualified as Http
import Http.Client.Internal qualified as HttpInternal
import IO qualified
import Data.IORef qualified as GhcIORef
import Control.Exception qualified as GhcException
import Network.Wai qualified as GhcWai
import Network.HTTP.Types qualified as GhcHttp
import Maybe qualified
import Network.Socket qualified as GhcSocket
import Network.Wai.Handler.Warp qualified as GhcWarp
import Service.Application (Application (..))
import Service.Application qualified as Application
import Service.EventStore.InMemory qualified as InMemory
import Service.Transport.Web (WebTransport (..), applyBindOverrides, hostPreference, server, validateBindHost, validateBindPort, warpSettings)
import Task qualified
import Test
import Text qualified
import Uuid qualified


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

      it "rejects malformed brackets and preserves bare IPv6" \_ -> do
        validateBindHost "fe80::1" |> shouldBe Nothing
        let bracketed = ["[::1", "::1]", "[::1]trailing"]
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

    describe "running" do
      it "serves /health on loopback when bound to 127.0.0.1" \_ -> do
        -- Instance identity: the health path is unique to this test, so a
        -- 200 can only come from the application started here.
        eventStore <- InMemory.new |> Task.mapError toText
        freePort <- getFreePort
        healthPath <- uniqueHealthPath
        let app =
              Application.new
                |> Application.withTransport server
                |> Application.withHost "127.0.0.1"
                |> Application.withPort freePort
                |> Application.withHealthCheck @() (\_ -> healthPath)
        running <- AsyncTask.run (Application.runWith eventStore app)
        let cancelApp = AsyncTask.cancel running |> Task.ignoreError
        let probe = do
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
        running <- AsyncTask.run (Application.runWith eventStore app)
        let cancelApp = AsyncTask.cancel running |> Task.ignoreError
        let probe = do
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
        requests <- GhcIORef.newIORef (0 :: Int) |> Task.fromIO
        let discovery _ respond = do
              GhcIORef.modifyIORef' requests (+ 1)
              respond (GhcWai.responseLBS GhcHttp.status200 [] "not-json")
        (result, count) <- Task.fromIO do
          GhcWarp.testWithApplication (pure discovery) \authPort -> do
            outcome <- IO.try (Task.runResult do
              -- The owned fixture is live before the invalid application starts.
              status <- getHealthStatus "fixture-control" authPort 0
              status |> shouldBe 200
              GhcIORef.writeIORef requests 0 |> Task.fromIO
              eventStore <- InMemory.new |> Task.mapError toText
              let app =
                    Application.new
                      |> Application.withTransport server
                      |> Application.withHost "http://127.0.0.1"
                      |> Application.withAuth @() (\_ -> [fmt|http://127.0.0.1:#{authPort}|])
              Application.runWith eventStore app |> Task.asResult)
            case outcome of
              Err err -> case GhcException.fromException err of
                Just (asyncError :: GhcException.SomeAsyncException) -> GhcException.throwIO asyncError
                Nothing -> pure ()
              Ok _ -> pure ()
            count <- GhcIORef.readIORef requests
            pure (outcome, count)
        case count of
          0 -> pass
          _ -> fail "REGRESSION_ASSERT:invalid-bind-contacted-auth"
        case result of
          Err err -> fail [fmt|unexpected startup exception: #{toText err}|]
          Ok (Err err) -> fail [fmt|owned auth fixture failed: #{err}|]
          Ok (Ok appResult) -> case appResult of
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
canBindElsewhere address port = Task.fromIO do
  sock <- GhcSocket.socket GhcSocket.AF_INET GhcSocket.Stream GhcSocket.defaultProtocol
  outcome <- GhcSocket.bind sock (GhcSocket.SockAddrInet (fromIntegral port) address) |> IO.try
  GhcSocket.close sock
  case outcome of
    Ok _ -> pure True
    Err _ -> pure False


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
