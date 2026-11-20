module Http.ClientRawSpec where

import Array qualified
import Basics
import Bytes qualified
import ConcurrentVar qualified
import Core
import Data.ByteString qualified as GhcBS
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

  describe "Http.Client.Internal.postFormRaw" do
    it "postFormRaw returns a non-2xx status with its exact body bytes" \_ -> do
      let rejection = Bytes.pack [0xff, 0xfe, 0x00, 0x7b]
      let respondTo _ = GhcWai.responseLBS GhcHTTP.status400 [("X-Fixture", "rejected")] (Bytes.toLazyLegacy rejection)
      withFormEndpoint respondTo \testPort _ -> do
        response <-
          HttpInternal.postFormRaw (formRequest testPort "/form") [("grant_type", "refresh_token")]
            |> Task.asResult
        case response of
          Ok resp -> do
            resp.statusCode |> shouldBe 400
            resp.body |> shouldBe rejection
            headerValue "X-Fixture" resp |> shouldBe (Just "rejected")
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "postFormRaw returns a 2xx body without decoding it" \_ -> do
      let respondTo _ = GhcWai.responseLBS GhcHTTP.status200 [(GhcHTTP.hContentType, "application/json")] "not-json"
      withFormEndpoint respondTo \testPort _ -> do
        response <-
          HttpInternal.postFormRaw (formRequest testPort "/form") [("grant_type", "refresh_token")]
            |> Task.asResult
        case response of
          Ok resp -> do
            resp.statusCode |> shouldBe 200
            resp.body |> Text.fromBytes |> shouldBe "not-json"
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]

    it "postFormRaw POSTs the fields url-encoded in caller order" \_ -> do
      withFormEndpoint emptyJsonResponse \testPort requests -> do
        _ <-
          HttpInternal.postFormRaw (formRequest testPort "/form") specialFormFields
            |> Task.mapError toText
        seen <- requests
        seen |> Array.map (\observed -> observed.method) |> shouldBe ["POST"]
        seen |> Array.map (\observed -> observed.contentType) |> shouldBe [Just "application/x-www-form-urlencoded"]
        seen
          |> Array.map formFields
          |> shouldBe
            [ [ ("grant_type", Just "refresh_token"),
                ("note", Just "a b&c=d+e%f"),
                ("name", Just "café"),
                ("empty", Just "")
              ]
            ]

    it "postFormRaw sends the same method, content type and body as Http.postForm" \_ -> do
      withFormEndpoint emptyJsonResponse \testPort requests -> do
        _ <-
          Http.postForm @Json.Value (formRequest testPort "/form") specialFormFields
            |> Task.mapError toText
        _ <-
          HttpInternal.postFormRaw (formRequest testPort "/form") specialFormFields
            |> Task.mapError toText
        seen <- requests
        let wire :: ObservedRequest -> (Text, Maybe Text, Bytes)
            wire observed = (observed.method, observed.contentType, observed.body)
        Array.length seen |> shouldBe 2
        Array.get 1 seen |> Maybe.map wire |> shouldBe (Array.get 0 seen |> Maybe.map wire)

    it "postFormRaw sends caller headers and no Accept header of its own" \_ -> do
      withFormEndpoint emptyJsonResponse \testPort requests -> do
        let request = formRequest testPort "/form" |> Http.addHeader "X-Probe" "caller-header"
        _ <-
          HttpInternal.postFormRaw request [("grant_type", "refresh_token")]
            |> Task.mapError toText
        seen <- requests
        seen |> Array.map (\observed -> observed.probe) |> shouldBe [Just "caller-header"]
        seen |> Array.map (\observed -> observed.accept) |> shouldBe [Nothing]

    it "postFormRaw returns a redirect status without contacting its target" \_ -> do
      withFormEndpoint redirectToTarget \testPort requests -> do
        response <-
          HttpInternal.postFormRaw (formRequest testPort "/form") [("client_secret", "super-secret-value")]
            |> Task.asResult
        case response of
          Ok resp -> resp.statusCode |> shouldBe 302
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]
        seen <- requests
        seen |> Array.map (\observed -> observed.path) |> shouldBe ["/form"]

    it "postFormRaw follows a redirect only when the request allows one" \_ -> do
      withFormEndpoint redirectToTarget \testPort requests -> do
        let request = formRequest testPort "/form" |> Http.withRedirects 1
        response <-
          HttpInternal.postFormRaw request [("grant_type", "refresh_token")]
            |> Task.asResult
        case response of
          Ok resp -> do
            resp.statusCode |> shouldBe 200
            resp.body |> Text.fromBytes |> shouldBe "redirect-target"
          Err err -> fail [fmt|Expected Ok, got Err: #{show err}|]
        seen <- requests
        seen |> Array.map (\observed -> observed.path) |> shouldBe ["/form", "/target"]

    it "postFormRaw enforces maxResponseBytes on the response body" \_ -> do
      let respondTo _ = GhcWai.responseLBS GhcHTTP.status200 [] "more-than-four"
      withFormEndpoint respondTo \testPort _ -> do
        let request = formRequest testPort "/form" |> Http.withMaxResponseSize 4
        response <-
          HttpInternal.postFormRaw request [("grant_type", "refresh_token")]
            |> Task.asResult
        case response of
          Err (Http.ResponseTooLarge limit) -> limit |> shouldBe 4
          Err err -> fail [fmt|Expected ResponseTooLarge, got: #{show err}|]
          Ok _ -> fail "Expected ResponseTooLarge, got Ok"

    it "postFormRaw stops at the response timeout with a sanitized error" \_ -> do
      let slowApp _request respond = do
            GhcConcurrent.threadDelay 3000000
            respond (GhcWai.responseLBS GhcHTTP.status200 [] "late")
      withLoopbackServer slowApp \testPort -> do
        let request = formRequest testPort "/form?token=query-secret" |> Http.withTimeout 1
        response <-
          HttpInternal.postFormRaw request [("client_secret", "super-secret-value")]
            |> Task.asResult
        case response of
          Err (Http.Error message) -> do
            message |> shouldBeSanitized
            Text.contains "response timeout" message |> shouldBe True
          Err err -> fail [fmt|Expected a sanitized Error, got: #{show err}|]
          Ok _ -> fail "Expected the response timeout to stop the request"

    it "postFormRaw keeps form fields and the URL query out of a connection error" \_ -> do
      withRefusingPort \refusingPort -> do
        response <-
          HttpInternal.postFormRaw
            (formRequest refusingPort "/form?token=query-secret")
            [("client_secret", "super-secret-value")]
            |> Task.asResult
        case response of
          Err (Http.Error message) -> do
            message |> shouldBeSanitized
            Text.contains "connection failed" message |> shouldBe True
          Err err -> fail [fmt|Expected a sanitized Error, got: #{show err}|]
          Ok _ -> fail "Expected an error for a refused connection"

    it "postFormRaw reports an invalid URL without its path or query" \_ -> do
      let request = Http.request |> Http.withUrl "://missing-scheme/form?token=query-secret"
      response <-
        HttpInternal.postFormRaw request [("client_secret", "super-secret-value")]
          |> Task.asResult
      case response of
        Err (Http.Error message) -> do
          Text.contains "super-secret-value" message |> shouldBe False
          Text.contains "query-secret" message |> shouldBe False
          Text.contains "/form" message |> shouldBe False
        Err err -> fail [fmt|Expected a sanitized Error, got: #{show err}|]
        Ok _ -> fail "Expected an error for an invalid URL"

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
-- Test Helpers (form endpoint)
-- ============================================================================

-- | What the form endpoint received in one request.
data ObservedRequest = ObservedRequest
  { method :: Text,
    path :: Text,
    contentType :: Maybe Text,
    accept :: Maybe Text,
    probe :: Maybe Text,
    body :: Bytes
  }


-- | Run a WAI application on a loopback port that Warp binds before the test starts.
withLoopbackServer :: GhcWai.Application -> (Int -> Task Text Unit) -> Task Text Unit
withLoopbackServer app runTest = do
  let settings = GhcWarp.defaultSettings |> GhcWarp.setHost "127.0.0.1"
  GhcWarp.testWithApplicationSettings
    settings
    (Task.runOrPanic (Task.yield app :: Task Text GhcWai.Application))
    (\testPort -> Task.runOrPanic (runTest testPort))
    |> Task.fromIO


-- | Reserve a loopback port that refuses connections for the duration of the test.
-- The socket stays bound and never listens, so no other process can take the port.
withRefusingPort :: (Int -> Task Text Unit) -> Task Text Unit
withRefusingPort runTest = do
  let hints = GhcSocket.defaultHints {GhcSocket.addrSocketType = GhcSocket.Stream}
  addresses <- GhcSocket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0") |> Task.fromIO
  case addresses of
    [] -> fail "The loopback address did not resolve"
    address : _ -> do
      socket <- GhcSocket.openSocket address |> Task.fromIO
      let reserveAndRun = do
            GhcSocket.bind socket (GhcSocket.addrAddress address) |> Task.fromIO
            port <- GhcSocket.socketPort socket |> Task.fromIO
            runTest (fromIntegral port)
      reserveAndRun |> Task.finally (GhcSocket.close socket |> Task.fromIO)


-- | Answer every request through @respondTo@ and give the test the requests received so far.
withFormEndpoint ::
  (ObservedRequest -> GhcWai.Response) ->
  (Int -> Task Text (Array ObservedRequest) -> Task Text Unit) ->
  Task Text Unit
withFormEndpoint respondTo runTest = do
  received <- ConcurrentVar.containing (Array.empty :: Array ObservedRequest)
  let app request respond = do
        requestBody <- GhcWai.strictRequestBody request
        let observed =
              ObservedRequest
                { method = GhcWai.requestMethod request |> utf8Text,
                  path = GhcWai.rawPathInfo request |> utf8Text,
                  contentType = requestHeader GhcHTTP.hContentType request,
                  accept = requestHeader GhcHTTP.hAccept request,
                  probe = requestHeader "X-Probe" request,
                  body = requestBody |> Bytes.fromLazyLegacy
                }
        Task.runOrPanic (ConcurrentVar.modify (Array.push observed) received :: Task Text Unit)
        respond (respondTo observed)
  withLoopbackServer app \testPort -> runTest testPort (ConcurrentVar.peek received)


-- | A request for a path on the loopback form endpoint.
formRequest :: Int -> Text -> Http.Request
formRequest testPort path = Http.request |> Http.withUrl [fmt|http://127.0.0.1:#{testPort}#{path}|]


-- | Fields whose values need url-encoding: a space, reserved characters, non-ASCII text and an empty value.
specialFormFields :: Array (Text, Text)
specialFormFields =
  [ ("grant_type", "refresh_token"),
    ("note", "a b&c=d+e%f"),
    ("name", "café"),
    ("empty", "")
  ]


-- | Decode the url-encoded request body into its fields, in wire order.
formFields :: ObservedRequest -> Array (Text, Maybe Text)
formFields observed =
  observed.body
    |> Bytes.unwrap
    |> GhcHTTP.parseQuery
    |> Array.fromLinkedList
    |> Array.map (\(name, value) -> (utf8Text name, Maybe.map utf8Text value))


utf8Text :: GhcBS.ByteString -> Text
utf8Text bytes = bytes |> Bytes.fromLegacy |> Text.fromBytes


requestHeader :: GhcHTTP.HeaderName -> GhcWai.Request -> Maybe Text
requestHeader name request =
  GhcWai.requestHeaders request
    |> Array.fromLinkedList
    |> Array.find (\(headerName, _) -> headerName == name)
    |> Maybe.map (\(_, value) -> utf8Text value)


emptyJsonResponse :: ObservedRequest -> GhcWai.Response
emptyJsonResponse _ = GhcWai.responseLBS GhcHTTP.status200 [(GhcHTTP.hContentType, "application/json")] "{}"


-- | Redirect @/form@ to @/target@, which answers 200.
redirectToTarget :: ObservedRequest -> GhcWai.Response
redirectToTarget observed =
  if observed.path == "/form"
    then GhcWai.responseLBS GhcHTTP.status302 [(GhcHTTP.hLocation, "/target")] GhcLBS.empty
    else GhcWai.responseLBS GhcHTTP.status200 [] "redirect-target"


-- | A transport error names the host but never the form fields or the URL path and query.
shouldBeSanitized :: Text -> Task Text Unit
shouldBeSanitized message = do
  Text.contains "super-secret-value" message |> shouldBe False
  Text.contains "query-secret" message |> shouldBe False
  Text.contains "/form" message |> shouldBe False
  Text.contains "127.0.0.1" message |> shouldBe True


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
