module Http.ClientRawSpec where

import Array qualified
import Basics
import Bytes qualified
import Core
import Http.Client qualified as Http
import Http.Client.Internal qualified as HttpInternal
import Json qualified
import Maybe qualified
import Task qualified
import Test
import Text qualified

-- WAI/Warp for mock server
import qualified Control.Concurrent as GhcConcurrent
import qualified Data.ByteString.Lazy as GhcLBS
import qualified Network.HTTP.Types as GhcHTTP
import qualified Network.Socket as GhcSocket
import qualified Network.Wai as GhcWai
import qualified Network.Wai.Handler.Warp as GhcWarp


spec :: Spec Unit
spec = do
  describe "Http.Client.Internal.getRaw" do
    it "returns Ok with statusCode 401 on 401 response" \_ -> do
      testPort <- getFreePort
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock401App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> HttpInternal.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                resp.statusCode |> shouldBe 401
                resp.body |> Bytes.length |> shouldSatisfy (\len -> len > 0)
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "returns Ok with statusCode 429 on 429 response" \_ -> do
      testPort <- getFreePort
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock429App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> HttpInternal.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                resp.statusCode |> shouldBe 429
                let headerResult = resp.headers |> Array.find (\(name, _) -> Text.toLower name == "retry-after")
                case headerResult of
                  Just _ -> Task.yield ()
                  Nothing -> fail "Expected Retry-After header"
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "returns Ok with statusCode 200 on 200 response" \_ -> do
      testPort <- getFreePort
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock200App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> HttpInternal.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                resp.statusCode |> shouldBe 200
                resp.body |> Bytes.length |> shouldSatisfy (\len -> len > 0)
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "response body is accessible and decodable" \_ -> do
      testPort <- getFreePort
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mockJsonApp) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> HttpInternal.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                let decoded = Json.decodeBytes @TestJson resp.body
                case decoded of
                  Ok json -> json.message |> shouldBe "success"
                  Err _ -> fail "Expected valid JSON in response body"
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "returns Err on network error" \_ -> do
      -- Use a high port unlikely to be in use for network error test
      response <-
        Http.request
          |> Http.withUrl "http://localhost:59999/test"
          |> Http.withTimeout 1
          |> HttpInternal.getRaw
          |> Task.asResult
      case response of
        Err (Http.Error _msg) -> Task.yield ()
        Err (Http.InvalidUrl _) -> fail "Unexpected InvalidUrl from Internal.getRaw"
        Err (Http.ResponseTooLarge _) -> fail "Unexpected ResponseTooLarge from Internal.getRaw"
        Ok _ -> fail "Expected error for unreachable host"

  describe "Http.Client.Internal.sendRaw" do
    it "sendRaw rejects an oversized chunk before source EOF" \_ -> do
      sourceRelease <- GhcConcurrent.newEmptyMVar |> Task.fromIO
      sourceFlushed <- GhcConcurrent.newEmptyMVar |> Task.fromIO
      clientResult <- GhcConcurrent.newEmptyMVar |> Task.fromIO
      let streamingApp request respond =
            if GhcWai.rawPathInfo request == "/control"
              then mock200App request respond
              else respond (GhcWai.responseStream GhcHTTP.status200 [] \write flush -> do
                write "12345"
                flush
                GhcConcurrent.putMVar sourceFlushed ()
                GhcConcurrent.takeMVar sourceRelease)
      withMockServer streamingApp \testPort -> do
        control <-
          Http.request
            |> Http.withUrl [fmt|http://127.0.0.1:#{testPort}/control|]
            |> Http.withTimeout 10
            |> HttpInternal.getRaw
            |> Task.asResult
        case control of
          Ok response -> response.statusCode |> shouldBe 200
          Err err -> fail [fmt|Fixture positive control failed: #{show err}|]
        let requestTask :: Task Text (Result Http.Error (Http.Response Bytes))
            requestTask =
              Http.request
                |> Http.withUrl [fmt|http://127.0.0.1:#{testPort}/paced|]
                |> Http.withMaxResponseSize 4
                |> Http.withTimeout 10
                |> (\req -> HttpInternal.sendRaw Http.Get req Bytes.empty)
                |> Task.asResult
        clientThread <- GhcConcurrent.forkIO (do
          result <- Task.runOrPanic requestTask
          GhcConcurrent.putMVar clientResult result) |> Task.fromIO
        let checkBeforeEof = do
              GhcConcurrent.threadDelay 2000000 |> Task.fromIO
              flushed <- GhcConcurrent.tryTakeMVar sourceFlushed |> Task.fromIO
              case flushed of
                Nothing -> fail "Fixture did not flush its oversized chunk"
                Just () -> Task.yield ()
              GhcConcurrent.threadDelay 2000000 |> Task.fromIO
              observed <- GhcConcurrent.tryTakeMVar clientResult |> Task.fromIO
              case observed of
                Just (Err (Http.ResponseTooLarge limit)) -> limit |> shouldBe 4
                Just other -> fail [fmt|Expected ResponseTooLarge before EOF, got: #{show other}|]
                Nothing -> fail "REGRESSION_ASSERT:sendRaw-awaits-source-EOF"
        let cleanup = do
              _ <- GhcConcurrent.tryPutMVar sourceRelease () |> Task.fromIO
              GhcConcurrent.killThread clientThread |> Task.fromIO
        checkBeforeEof |> Task.finally cleanup

    it "sendRaw PUT delivers method, raw body and custom header" \_ -> do
      withMockServer mockEchoApp \testPort -> do
        response <-
          Http.request
            |> Http.withUrl [fmt|http://localhost:#{testPort}/bucket/key|]
            |> Http.addHeader "X-Probe" "sigv4"
            |> (\req -> HttpInternal.sendRaw Http.Put req (Text.toBytes "payload-bytes"))
            |> Task.asResult
        case response of
          Ok resp -> do
            resp.statusCode |> shouldBe 200
            headerValue "X-Echo-Method" resp |> shouldBe (Just "PUT")
            headerValue "X-Echo-Probe" resp |> shouldBe (Just "sigv4")
            resp.body |> Text.fromBytes |> shouldBe "payload-bytes"
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "sendRaw HEAD returns headers and an empty body without throwing" \_ -> do
      withMockServer mockEchoApp \testPort -> do
        response <-
          Http.request
            |> Http.withUrl [fmt|http://localhost:#{testPort}/bucket/key|]
            |> (\req -> HttpInternal.sendRaw Http.Head req Bytes.empty)
            |> Task.asResult
        case response of
          Ok resp -> do
            resp.statusCode |> shouldBe 200
            headerValue "X-Echo-Method" resp |> shouldBe (Just "HEAD")
            resp.body |> Bytes.length |> shouldBe 0
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "sendRaw DELETE surfaces a 404 as a status code, not an error" \_ -> do
      withMockServer (mockStatusApp GhcHTTP.status404) \testPort -> do
        response <-
          Http.request
            |> Http.withUrl [fmt|http://localhost:#{testPort}/bucket/missing|]
            |> (\req -> HttpInternal.sendRaw Http.Delete req Bytes.empty)
            |> Task.asResult
        case response of
          Ok resp -> resp.statusCode |> shouldBe 404
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "sendRaw PUT surfaces a 403 as a status code, not an error" \_ -> do
      withMockServer (mockStatusApp GhcHTTP.status403) \testPort -> do
        response <-
          Http.request
            |> Http.withUrl [fmt|http://localhost:#{testPort}/bucket/key|]
            |> (\req -> HttpInternal.sendRaw Http.Put req (Text.toBytes "x"))
            |> Task.asResult
        case response of
          Ok resp -> resp.statusCode |> shouldBe 403
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "sendRaw enforces maxResponseBytes on the response body" \_ -> do
      withMockServer mockEchoApp \testPort -> do
        response <-
          Http.request
            |> Http.withUrl [fmt|http://localhost:#{testPort}/bucket/key|]
            |> Http.withMaxResponseSize 4
            |> (\req -> HttpInternal.sendRaw Http.Put req (Text.toBytes "more-than-four"))
            |> Task.asResult
        case response of
          Err (Http.ResponseTooLarge limit) -> limit |> shouldBe 4
          Err err -> fail [fmt|Expected ResponseTooLarge, got: #{show err}|]
          Ok _ -> fail "Expected ResponseTooLarge, got Ok"

  describe "Http.Client.sendSecure" do
    it "sendSecure rejects a plain http:// URL with InvalidUrl" \_ -> do
      response <-
        Http.request
          |> Http.withUrl "http://localhost:59999/bucket/key?token=secret"
          |> (\req -> Http.sendSecure Http.Put req Bytes.empty)
          |> Task.asResult
      case response of
        Err (Http.InvalidUrl sanitized) -> Text.contains "secret" sanitized |> shouldBe False
        Err err -> fail [fmt|Expected InvalidUrl, got: #{show err}|]
        Ok _ -> fail "Expected InvalidUrl, got Ok"

    it "methodName spells every wire token" \_ -> do
      [Http.Get, Http.Head, Http.Post, Http.Put, Http.Patch, Http.Delete]
        |> Array.map Http.methodName
        |> shouldBe ["GET", "HEAD", "POST", "PUT", "PATCH", "DELETE"]


-- ============================================================================
-- Test Helpers
-- ============================================================================

-- | Run a WAI application on a free loopback port for the duration of the test.
withMockServer :: GhcWai.Application -> (Int -> Task Text Unit) -> Task Text Unit
withMockServer app runTest = do
  testPort <- getFreePort
  serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort app) |> Task.fromIO
  GhcConcurrent.threadDelay 50000 |> Task.fromIO
  let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
  runTest testPort |> Task.finally cleanup


-- | Look up a response header case-insensitively.
headerValue :: Text -> Http.Response Bytes -> Maybe Text
headerValue name response =
  response.headers
    |> Array.find (\(headerName, _) -> Text.toLower headerName == Text.toLower name)
    |> Maybe.map (\(_, value) -> value)


-- ============================================================================
-- Test Helpers (ports)
-- ============================================================================

-- | Allocate a free port dynamically to avoid collisions in parallel test runs.
-- Binds to port 0 (OS assigns free port), reads the assigned port, then closes the socket.
getFreePort :: Task _ Int
getFreePort = do
  Task.fromIO do
    let hints = GhcSocket.defaultHints { GhcSocket.addrSocketType = GhcSocket.Stream }
    addrInfos <- GhcSocket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0")
    case addrInfos of
      [] -> pure 19900 -- Fallback port if getAddrInfo fails
      (addr : _) -> do
        sock <- GhcSocket.openSocket addr
        GhcSocket.bind sock (GhcSocket.addrAddress addr)
        port <- GhcSocket.socketPort sock
        GhcSocket.close sock
        pure (fromIntegral port)


-- ============================================================================
-- Mock Applications
-- ============================================================================

-- | Mock app that returns 401 Unauthorized
mock401App :: GhcWai.Application
mock401App _request respond = do
  let responseBody = GhcLBS.fromStrict "{\"error\":\"unauthorized\"}"
  respond
    ( GhcWai.responseLBS
        GhcHTTP.status401
        [(GhcHTTP.hContentType, "application/json")]
        responseBody
    )


-- | Mock app that returns 429 Too Many Requests with Retry-After header
mock429App :: GhcWai.Application
mock429App _request respond = do
  let responseBody = GhcLBS.fromStrict "{\"error\":\"rate_limited\"}"
  respond
    ( GhcWai.responseLBS
        GhcHTTP.status429
        [ (GhcHTTP.hContentType, "application/json"),
          ("Retry-After", "60")
        ]
        responseBody
    )


-- | Mock app that returns 200 OK
mock200App :: GhcWai.Application
mock200App _request respond = do
  let responseBody = GhcLBS.fromStrict "{\"status\":\"ok\"}"
  respond
    ( GhcWai.responseLBS
        GhcHTTP.status200
        [(GhcHTTP.hContentType, "application/json")]
        responseBody
    )


-- | Mock app that echoes the request method, the X-Probe header and the raw body.
mockEchoApp :: GhcWai.Application
mockEchoApp request respond = do
  body <- GhcWai.strictRequestBody request
  let probeHeader =
        GhcWai.requestHeaders request
          |> Array.fromLinkedList
          |> Array.find (\(name, _) -> name == "X-Probe")
  let probe = case probeHeader of
        Nothing -> ""
        Just (_, value) -> value
  respond
    ( GhcWai.responseLBS
        GhcHTTP.status200
        [ (GhcHTTP.hContentType, "application/octet-stream"),
          ("X-Echo-Method", GhcWai.requestMethod request),
          ("X-Echo-Probe", probe)
        ]
        body
    )


-- | Mock app that answers every request with the given status and an empty body.
mockStatusApp :: GhcHTTP.Status -> GhcWai.Application
mockStatusApp status _request respond =
  respond (GhcWai.responseLBS status [] GhcLBS.empty)


-- | Mock app that returns valid JSON
mockJsonApp :: GhcWai.Application
mockJsonApp _request respond = do
  let responseBody = GhcLBS.fromStrict "{\"message\":\"success\",\"code\":200}"
  respond
    ( GhcWai.responseLBS
        GhcHTTP.status200
        [(GhcHTTP.hContentType, "application/json")]
        responseBody
    )


-- ============================================================================
-- Test JSON Structure
-- ============================================================================

-- | Test JSON structure for decoding
data TestJson = TestJson
  { message :: Text,
    code :: Int
  }
  deriving (Show, Generic)

instance Json.FromJSON TestJson where
  parseJSON = Json.withObject "TestJson" \obj -> do
    message <- obj Json..: "message"
    code <- obj Json..: "code"
    pure TestJson {message, code}
