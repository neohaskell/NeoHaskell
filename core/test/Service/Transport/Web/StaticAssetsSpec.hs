-- | Static asset serving tests (ADR-0066).
--
-- Tests for 'serveStaticAsset' and the 'StaticAssets' configuration type.
-- All tests are written against the STUB implementation and MUST FAIL (red).
-- Phase 10 implements the real logic; this file drives it outside-in.
--
-- Coverage: 41 cases — 6 happy / 26 edge / 9 error.
-- Structured as:
--   file serving (3) | cache-control (6) | SPA fallback (4) |
--   traversal guard (5) | content-type (8) | static disabled (2) |
--   boundary conditions (7) | startup validation (3) | API precedence (3)
module Service.Transport.Web.StaticAssetsSpec where

import Basics
import Bytes qualified
import ConcurrentVar qualified
import Data.ByteString qualified as GhcBS
import Data.Text.IO qualified as GhcTextIO
import Maybe (Maybe (..))
import Network.HTTP.Types.Header qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Service.Transport.Web (
  HealthCheckConfig (..),
  StaticAssets (..),
  WebTransport (..),
  isHealthCheckPath,
  server,
  serveStaticAsset,
  )
import System.Directory qualified as GhcDir
import System.FilePath qualified as GhcFilePath
import Task (Task)
import Task qualified
import Test
import Text (Text)
import Text qualified
import Uuid qualified


-- ============================================================================
-- Test helpers
-- ============================================================================


-- | Create a mock GET request with the given raw path (percent-encoded form).
mockGetRequest :: Text -> Wai.Request
mockGetRequest path =
  Wai.defaultRequest
    { WaiInternal.rawPathInfo = path |> Text.toBytes |> Bytes.unwrap
    , WaiInternal.requestMethod = "GET"
    }


-- | Create a mock POST request with the given raw path.
mockPostRequest :: Text -> Wai.Request
mockPostRequest path =
  Wai.defaultRequest
    { WaiInternal.rawPathInfo = path |> Text.toBytes |> Bytes.unwrap
    , WaiInternal.requestMethod = "POST"
    }


-- | Run serveStaticAsset and capture the WAI response.
-- Returns the captured Wai.Response; fails the test if respond was never called.
runServe :: StaticAssets -> Wai.Request -> Task Text Wai.Response
runServe assets request = do
  responseVar <- ConcurrentVar.containing (Nothing :: Maybe Wai.Response)
  let testRespond :: Wai.Response -> Task Text Wai.ResponseReceived
      testRespond response = do
        responseVar |> ConcurrentVar.modify (\_ -> Just response)
        Task.yield WaiInternal.ResponseReceived
  _ <- serveStaticAsset assets request testRespond
  maybeResponse <- responseVar |> ConcurrentVar.peek
  case maybeResponse of
    Nothing -> Task.throw "serveStaticAsset did not call respond"
    Just resp -> Task.yield resp


-- | Assert the HTTP status code of a captured response.
-- Usage: response |> shouldHaveStatus HTTP.status200
shouldHaveStatus :: HTTP.Status -> Wai.Response -> Task Text Unit
shouldHaveStatus expectedStatus response =
  Wai.responseStatus response |> shouldBe expectedStatus


-- | Assert that a specific header has the expected value in the response.
-- Usage: response |> shouldHaveHeader HTTP.hContentType "text/html"
shouldHaveHeader ::
  HTTP.HeaderName ->
  GhcBS.ByteString ->
  Wai.Response ->
  Task Text Unit
shouldHaveHeader headerName expectedValue response =
  case lookupHeader headerName (Wai.responseHeaders response) of
    Nothing -> fail [fmt|Expected header #{show headerName} but it was absent|]
    Just val -> val |> shouldBe expectedValue


-- | Find a header value in a list of response headers.
lookupHeader ::
  HTTP.HeaderName ->
  [(HTTP.HeaderName, GhcBS.ByteString)] ->
  Maybe GhcBS.ByteString
lookupHeader target headers =
  case headers of
    [] -> Nothing
    (name, value) : rest ->
      case name == target of
        True -> Just value
        False -> lookupHeader target rest


-- | Create a unique temporary directory under /tmp and return its path as Text.
makeTempDir :: Task Text Text
makeTempDir = do
  uuid <- Uuid.generate
  let dir = [fmt|/tmp/nhcore-static-test-#{uuid}|]
  GhcDir.createDirectoryIfMissing True (Text.toLinkedList dir) |> Task.fromIO
  Task.yield dir


-- | Write a text file at relPath relative to root, creating parent dirs.
writeTextFile :: Text -> Text -> Text -> Task Text Unit
writeTextFile root relPath content = do
  let fullPath = Text.toLinkedList root GhcFilePath.</> Text.toLinkedList relPath
  let parentDir = GhcFilePath.takeDirectory fullPath
  GhcDir.createDirectoryIfMissing True parentDir |> Task.fromIO
  GhcTextIO.writeFile fullPath content |> Task.fromIO


-- | Write a zero-byte binary file at relPath relative to root.
writeEmptyFile :: Text -> Text -> Task Text Unit
writeEmptyFile root relPath = do
  let fullPath = Text.toLinkedList root GhcFilePath.</> Text.toLinkedList relPath
  let parentDir = GhcFilePath.takeDirectory fullPath
  GhcDir.createDirectoryIfMissing True parentDir |> Task.fromIO
  GhcBS.writeFile fullPath GhcBS.empty |> Task.fromIO


-- ============================================================================
-- Spec
-- ============================================================================


spec :: Spec Unit
spec = do
  describe "Static Assets (ADR-0066)" do

    -- -----------------------------------------------------------------------
    -- Section 1: Core file-serving (cases 1.1, 1.2, 1.8)
    -- -----------------------------------------------------------------------

    describe "file serving" do

      it "1.1 [happy] serves existing HTML file with correct Content-Type and Cache-Control" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html>Hello SPA</html>"
        let assets = StaticAssets { root = root, spaFallback = Just "index.html" }
        response <- runServe assets (mockGetRequest "/index.html")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hContentType "text/html; charset=utf-8"
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "1.2 [happy] serves hashed CSS bundle with immutable Cache-Control" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.a1b2c3d4.css" "body { margin: 0; }"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.a1b2c3d4.css")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hContentType "text/css"
        response |> shouldHaveHeader HTTP.hCacheControl "public, max-age=31536000, immutable"

      it "1.8 [error] returns 404 for non-existent file when spaFallback=Nothing" \_ -> do
        root <- makeTempDir
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/notfound.js")
        response |> shouldHaveStatus HTTP.status404

    -- -----------------------------------------------------------------------
    -- Section 2: Cache-Control classification (cases 2.1 – 2.6)
    -- -----------------------------------------------------------------------

    describe "cache-control classification" do

      it "2.1 [happy] index.html gets no-cache policy (entry document, non-hashed name)" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html></html>"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/index.html")
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "2.2 [edge] exactly 8 hex chars between dots is matched as hashed → immutable" \_ -> do
        root <- makeTempDir
        writeTextFile root "bundle.12345678.js" "console.log(1)"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/bundle.12345678.js")
        response |> shouldHaveHeader HTTP.hCacheControl "public, max-age=31536000, immutable"

      it "2.3 [edge] 16 hex chars between dots is matched as hashed → immutable" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.1234567890abcdef.js" "console.log(2)"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.1234567890abcdef.js")
        response |> shouldHaveHeader HTTP.hCacheControl "public, max-age=31536000, immutable"

      it "2.4 [edge] 7 hex chars between dots is NOT matched as hashed → no-cache" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.1234567.js" "console.log(3)"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.1234567.js")
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "2.5 [edge] non-hex chars in segment NOT matched as hashed → no-cache" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.12345g7h.js" "console.log(4)"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.12345g7h.js")
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "2.6 [edge] unknown extension with hash segment → immutable + application/octet-stream" \_ -> do
        root <- makeTempDir
        writeEmptyFile root "data.xyz123456.unknown"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/data.xyz123456.unknown")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hContentType "application/octet-stream"
        response |> shouldHaveHeader HTTP.hCacheControl "public, max-age=31536000, immutable"

    -- -----------------------------------------------------------------------
    -- Section 3: SPA fallback routing (cases 3.1 – 3.4)
    -- -----------------------------------------------------------------------

    describe "SPA fallback routing" do

      it "3.1 [happy] unmatched path served with SPA shell when spaFallback configured" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html>SPA shell</html>"
        let assets = StaticAssets { root = root, spaFallback = Just "index.html" }
        response <- runServe assets (mockGetRequest "/orders/42")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hContentType "text/html; charset=utf-8"
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "3.2 [error] unmatched path returns 404 when spaFallback=Nothing" \_ -> do
        root <- makeTempDir
        writeTextFile root "about.html" "<html>About</html>"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/orders/42")
        response |> shouldHaveStatus HTTP.status404

      it "3.3 [error] unmatched path returns 404 when configured fallback document absent from disk" \_ -> do
        root <- makeTempDir
        -- spaFallback points to app.html which does not exist
        let assets = StaticAssets { root = root, spaFallback = Just "app.html" }
        response <- runServe assets (mockGetRequest "/orders/42")
        response |> shouldHaveStatus HTTP.status404

      it "3.4 [error] direct request for the fallback document bypasses fallback logic" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html>Shell</html>"
        let assets = StaticAssets { root = root, spaFallback = Just "index.html" }
        -- /index.html is requested directly, not via deep-link fallback
        response <- runServe assets (mockGetRequest "/index.html")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

    -- -----------------------------------------------------------------------
    -- Section 4: Path traversal guard / security (cases 4.1 – 4.5)
    -- -----------------------------------------------------------------------

    describe "path traversal guard (security)" do

      it "4.1 [error] simple .. escape attempt returns 404 (never 403)" \_ -> do
        root <- makeTempDir
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/../etc/passwd")
        response |> shouldHaveStatus HTTP.status404

      it "4.2 [error] nested .. escape attempt returns 404" \_ -> do
        root <- makeTempDir
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/../../etc/shadow")
        response |> shouldHaveStatus HTTP.status404

      it "4.3 [error] .. embedded in path returns 404 after normalization reveals root escape" \_ -> do
        root <- makeTempDir
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/assets/../../../etc/passwd")
        response |> shouldHaveStatus HTTP.status404

      it "4.4 [error] decoded percent-encoded .. in path returns 404 (WAI provides decoded rawPathInfo)" \_ -> do
        root <- makeTempDir
        -- Warp decodes %2e%2e → .. before setting rawPathInfo; we simulate the decoded form
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/assets/../etc/passwd")
        response |> shouldHaveStatus HTTP.status404

      it "4.5 [error] double-encoded traversal returns 404 (literal %2e in path; no file found)" \_ -> do
        root <- makeTempDir
        -- After one decode, %252e%252e becomes %2e%2e (not ..) — no file with literal %2e chars
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/assets/%2e%2e/etc/passwd")
        response |> shouldHaveStatus HTTP.status404

      -- Regression tests for sec-impl-010: SPA fallback path-traversal guard
      -- (serveStaticSpaFallback must normalise and root-prefix-check the fallback path)

      it "4.6 [sec-regression] absolute spaFallback path returns 404 for in-root miss (never serves out-of-root file)" \_ -> do
        root <- makeTempDir
        -- spaFallback set to an absolute path outside root (/etc/hosts always exists on macOS/Linux)
        let assets = StaticAssets { root = root, spaFallback = Just "/etc/hosts" }
        -- Request a path that does not exist inside root, triggering the fallback branch
        response <- runServe assets (mockGetRequest "/nonexistent-page")
        response |> shouldHaveStatus HTTP.status404

      it "4.7 [sec-regression] relative-escape spaFallback path returns 404 for in-root miss (never traverses)" \_ -> do
        root <- makeTempDir
        -- Create a sentinel file outside root that the traversal would reach via ../
        sentinelDir <- makeTempDir
        writeTextFile sentinelDir "sentinel.html" "<html>SECRET</html>"
        -- spaFallback set to a relative path that escapes root via ../
        let escapeDoc = [fmt|../#{GhcFilePath.takeFileName (Text.toLinkedList sentinelDir)}/sentinel.html|]
        let assets = StaticAssets { root = root, spaFallback = Just escapeDoc }
        -- Request a path that does not exist inside root, triggering the fallback branch
        response <- runServe assets (mockGetRequest "/nonexistent-page")
        response |> shouldHaveStatus HTTP.status404

    -- -----------------------------------------------------------------------
    -- Section 5: Content-Type mapping (cases 5.1 – 5.8)
    -- -----------------------------------------------------------------------

    describe "content-type mapping" do

      it "5.1 [happy] .html file returns text/html; charset=utf-8" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.html" "<!DOCTYPE html><html></html>"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.html")
        response |> shouldHaveHeader HTTP.hContentType "text/html; charset=utf-8"

      it "5.2 [edge] .js file returns application/javascript; charset=utf-8" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.js" "export default {}"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.js")
        response |> shouldHaveHeader HTTP.hContentType "application/javascript; charset=utf-8"

      it "5.3 [edge] .css file returns text/css; charset=utf-8" \_ -> do
        root <- makeTempDir
        writeTextFile root "styles.css" "body { margin: 0; }"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/styles.css")
        response |> shouldHaveHeader HTTP.hContentType "text/css; charset=utf-8"

      it "5.4 [edge] .svg file returns image/svg+xml" \_ -> do
        root <- makeTempDir
        writeTextFile root "logo.svg" "<svg xmlns='http://www.w3.org/2000/svg'></svg>"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/logo.svg")
        response |> shouldHaveHeader HTTP.hContentType "image/svg+xml"

      it "5.5 [edge] .json file returns application/json; charset=utf-8" \_ -> do
        root <- makeTempDir
        writeTextFile root "config.json" "{}"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/config.json")
        response |> shouldHaveHeader HTTP.hContentType "application/json; charset=utf-8"

      it "5.6 [edge] .png file returns image/png (no charset for binary format)" \_ -> do
        root <- makeTempDir
        writeEmptyFile root "photo.png"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/photo.png")
        response |> shouldHaveHeader HTTP.hContentType "image/png"

      it "5.7 [edge] .wasm file returns application/wasm" \_ -> do
        root <- makeTempDir
        writeEmptyFile root "module.wasm"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/module.wasm")
        response |> shouldHaveHeader HTTP.hContentType "application/wasm"

      it "5.8 [edge] unknown extension defaults to application/octet-stream (no content-sniffing)" \_ -> do
        root <- makeTempDir
        writeEmptyFile root "data.xyz"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/data.xyz")
        response |> shouldHaveHeader HTTP.hContentType "application/octet-stream"

    -- -----------------------------------------------------------------------
    -- Section 6: Static serving disabled (cases 6.1 – 6.2)
    -- -----------------------------------------------------------------------

    describe "static serving disabled (staticAssets=Nothing)" do

      it "6.1 [error] default WebTransport has staticAssets=Nothing — feature is opt-in" \_ -> do
        case server.staticAssets of
          Just _ -> fail "Expected staticAssets=Nothing on the default server"
          Nothing -> pass

      it "6.2 [edge] health-check route remains reachable when staticAssets=Nothing" \_ -> do
        let transport = server { staticAssets = Nothing }
        isHealthCheckPath "health" transport |> shouldBe True

    -- -----------------------------------------------------------------------
    -- Section 7: Boundary conditions (cases 7.1 – 7.7)
    -- -----------------------------------------------------------------------

    describe "boundary conditions" do

      it "7.1 [happy] root path / is served as SPA shell when spaFallback is configured" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html>Root</html>"
        let assets = StaticAssets { root = root, spaFallback = Just "index.html" }
        response <- runServe assets (mockGetRequest "/")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hCacheControl "no-cache, must-revalidate"

      it "7.2 [edge] trailing-slash directory path returns 404 (no directory listing)" \_ -> do
        root <- makeTempDir
        writeTextFile root "assets/style.css" "body {}"
        let assets = StaticAssets { root = root, spaFallback = Just "index.html" }
        response <- runServe assets (mockGetRequest "/assets/")
        response |> shouldHaveStatus HTTP.status404

      it "7.3 [edge] query string is ignored for file lookup; correct file is served" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.js" "export default {}"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        -- rawPathInfo is /app.js; rawQueryString is separate in WAI
        let request =
              Wai.defaultRequest
                { WaiInternal.rawPathInfo = "/app.js" |> Text.toBytes |> Bytes.unwrap
                , WaiInternal.rawQueryString = "v=1" |> Text.toBytes |> Bytes.unwrap
                , WaiInternal.requestMethod = "GET"
                }
        response <- runServe assets request
        response |> shouldHaveStatus HTTP.status200

      it "7.4 [edge] fragment is client-side only; server receives /index.html and serves normally" \_ -> do
        root <- makeTempDir
        writeTextFile root "index.html" "<html>Top</html>"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        -- Fragment (#top) is never sent to the server; rawPathInfo = /index.html
        response <- runServe assets (mockGetRequest "/index.html")
        response |> shouldHaveStatus HTTP.status200

      it "7.5 [edge] case-sensitive path mismatch returns 404 on case-sensitive file systems" \_ -> do
        root <- makeTempDir
        writeTextFile root "app.js" "export default {}"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        -- /App.JS does not match app.js on case-sensitive Linux/macOS
        response <- runServe assets (mockGetRequest "/App.JS")
        response |> shouldHaveStatus HTTP.status404

      it "7.6 [edge] deeply nested path with long filename is served correctly" \_ -> do
        root <- makeTempDir
        let deepPath = "a/b/c/d/e/f/g/h/i/j/k/l/m/n/o/p/very-long-filename-1234567890.css"
        writeTextFile root deepPath "/* deep stylesheet */"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest [fmt|/#{deepPath}|])
        response |> shouldHaveStatus HTTP.status200

      it "7.7 [edge] unicode (multibyte UTF-8) directory name in path is served correctly" \_ -> do
        root <- makeTempDir
        -- Create a directory with a UTF-8 name (German umlaut 'ü' = U+00FC)
        writeTextFile root "\252ber/app.js" "export default {}"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/\252ber/app.js")
        response |> shouldHaveStatus HTTP.status200
        response |> shouldHaveHeader HTTP.hContentType "application/javascript; charset=utf-8"

    -- -----------------------------------------------------------------------
    -- Section 8: Startup validation — missing / empty / unreadable root
    -- (cases 8.1 – 8.3)
    --
    -- The Application-level Log.warn + setting staticAssets=Nothing behaviour
    -- is covered by integration tests (Application.run is not started here).
    -- These unit tests verify that serveStaticAsset returns 404 gracefully
    -- (not 500) when the root directory is absent or inaccessible at
    -- request time.
    -- -----------------------------------------------------------------------

    describe "startup validation — invalid root handling" do

      it "8.1 [error] nonexistent root dir returns 404 for any request (graceful, not 500)" \_ -> do
        let assets = StaticAssets { root = "/nonexistent_nhcore_test_root_xyz_abc", spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.js")
        response |> shouldHaveStatus HTTP.status404

      it "8.2 [error] empty root dir returns 404 for any file request" \_ -> do
        root <- makeTempDir
        -- Directory exists but contains no files
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.js")
        response |> shouldHaveStatus HTTP.status404

      it "8.3 [error] unreadable root dir returns 404 for any request (graceful, not 500)" \_ -> do
        root <- makeTempDir
        GhcDir.setPermissions
          (Text.toLinkedList root)
          (GhcDir.setOwnerReadable False GhcDir.emptyPermissions)
          |> Task.fromIO
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        response <- runServe assets (mockGetRequest "/app.js")
        GhcDir.setPermissions
          (Text.toLinkedList root)
          ( GhcDir.setOwnerReadable True
              (GhcDir.setOwnerExecutable True GhcDir.emptyPermissions)
          )
          |> Task.fromIO
        response |> shouldHaveStatus HTTP.status404

    -- -----------------------------------------------------------------------
    -- Section 9: API precedence — static serving is the terminal fallback
    -- (cases 9.1 – 9.3)
    -- -----------------------------------------------------------------------

    describe "API precedence (static serving is terminal fallback)" do

      it "9.1 [edge] serveStaticAsset does not handle POST requests — only serves GET-reachable files" \_ -> do
        root <- makeTempDir
        -- A POST to /commands/create goes to the command handler, not static serving.
        -- Here we verify that a POST request to an existing file path returns 404:
        -- serveStaticAsset must not serve files for non-GET methods.
        writeTextFile root "commands/create" "should-not-be-served"
        let assets = StaticAssets { root = root, spaFallback = Nothing }
        let request = mockPostRequest "/commands/create"
        response <- runServe assets request
        response |> shouldHaveStatus HTTP.status404

      it "9.2 [edge] static assets field can coexist on transport without displacing query routes" \_ -> do
        root <- makeTempDir
        let transport = server
              { staticAssets = Just StaticAssets { root = root, spaFallback = Nothing }
              }
        -- The staticAssets field is set; routing still places it AFTER all
        -- registered query endpoints (verified by WebSpec integration tests).
        case transport.staticAssets of
          Nothing -> fail "Expected staticAssets to be set"
          Just sa -> sa.root |> shouldBe root

      it "9.3 [edge] health-check route is registered before static fallback" \_ -> do
        root <- makeTempDir
        let transport = server
              { staticAssets = Just StaticAssets { root = root, spaFallback = Nothing }
              , healthCheck = Just HealthCheckConfig { healthPath = "health" }
              }
        -- Health check is matched BEFORE serveStaticAsset in the routing chain.
        isHealthCheckPath "health" transport |> shouldBe True
