module Http.ClientRawSpec where

import Array qualified
import Basics
import Bytes qualified
import Core
import Http.Client qualified as Http
import Json qualified
import Task qualified
import Test
import Text qualified

-- WAI/Warp for mock server
import qualified Control.Concurrent as GhcConcurrent
import qualified Data.ByteString.Lazy as GhcLBS
import qualified Network.HTTP.Types as GhcHTTP
import qualified Network.Wai as GhcWai
import qualified Network.Wai.Handler.Warp as GhcWarp


spec :: Spec Unit
spec = do
  describe "Http.getRaw" do
    it "returns Ok with statusCode 401 on 401 response" \_ -> do
      let testPort = 19877
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock401App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> Http.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                resp.statusCode |> shouldBe 401
                resp.body |> Bytes.length |> shouldSatisfy (\len -> len > 0)
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "returns Ok with statusCode 429 on 429 response" \_ -> do
      let testPort = 19878
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock429App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> Http.getRaw
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
      let testPort = 19879
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mock200App) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> Http.getRaw
                |> Task.asResult
            case response of
              Ok resp -> do
                resp.statusCode |> shouldBe 200
                resp.body |> Bytes.length |> shouldSatisfy (\len -> len > 0)
              Err _ -> fail "Expected Ok, got Err"
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup

    it "response body is accessible and decodable" \_ -> do
      let testPort = 19880
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mockJsonApp) |> Task.fromIO
      GhcConcurrent.threadDelay 50000 |> Task.fromIO
      let runTest = do
            response <-
              Http.request
                |> Http.withUrl [fmt|http://localhost:#{testPort}/test|]
                |> Http.getRaw
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
      response <-
        Http.request
          |> Http.withUrl "http://localhost:19881/test"
          |> Http.withTimeout 1
          |> Http.getRaw
          |> Task.asResult
      case response of
        Err (Http.Error _msg) -> Task.yield ()
        Ok _ -> fail "Expected error for unreachable host"


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
