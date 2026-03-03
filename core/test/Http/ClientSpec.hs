module Http.ClientSpec where

import Core
import Http.Client (Error (..))
import Http.Client qualified as Http
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Http.Client" do
    describe "Error Handling (security/reliability)" do
      -- CRITICAL: HTTP operations must never crash the process.
      -- Network failures, invalid URLs, and server errors should
      -- return typed errors, not throw exceptions.

      describe "get" do
        it "returns Error for invalid URL (does not crash)" \_ -> do
          let req = Http.request |> Http.withUrl "not-a-valid-url"
          result <- Http.get @() req |> Task.asResult
          case result of
            Err (Error _msg) -> Task.yield () -- Expected: typed error
            Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.get"
            Ok _ -> fail "Expected error for invalid URL"

        it "returns Error for unreachable host (does not crash)" \_ -> do
          -- Use a definitely-unreachable address
          let req =
                Http.request
                  |> Http.withUrl "http://localhost:59999/nonexistent"
                  |> Http.withTimeout 1
          result <- Http.get @() req |> Task.asResult
          case result of
            Err (Error _msg) -> Task.yield () -- Expected: typed error
            Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.get"
            Ok _ -> fail "Expected error for unreachable host"

      describe "post" do
        it "returns Error for invalid URL (does not crash)" \_ -> do
          let req = Http.request |> Http.withUrl "invalid://url"
          result <- Http.post @() req () |> Task.asResult
          case result of
            Err (Error _msg) -> Task.yield ()
            Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.post"
            Ok _ -> fail "Expected error for invalid URL"

      describe "postForm" do
        it "returns Error for invalid URL (does not crash)" \_ -> do
          let req = Http.request |> Http.withUrl "://missing-scheme"
          result <- Http.postForm @() req [] |> Task.asResult
          case result of
            Err (Error _msg) -> Task.yield ()
            Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.postForm"
            Ok _ -> fail "Expected error for invalid URL"

        it "returns Error for connection refused (does not crash)" \_ -> do
          let req =
                Http.request
                  |> Http.withUrl "http://127.0.0.1:59998/oauth/token"
                  |> Http.withTimeout 1
          result <- Http.postForm @() req [("grant_type", "test")] |> Task.asResult
          case result of
            Err (Error _msg) -> Task.yield ()
            Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.postForm"
            Ok _ -> fail "Expected error for connection refused"

    describe "Default Timeout (reliability)" do
      -- Production safety: HTTP requests should have a default timeout
      -- to prevent indefinite hangs under load

      it "request has default timeout (not infinite)" \_ -> do
        -- The default request should have a reasonable timeout set
        -- This is verified by checking the timeout field
        let req = Http.request
        -- Default timeout should be set (10 seconds)
        req.timeoutSeconds |> shouldBe (Just 10)

    -- NOTE: Performance requirement - no Console.log in hot path
    -- At 50k req/s, logging every request is a throughput killer.
    -- This is verified by code review, not runtime test.
    -- The Http.Client module should have NO Console.log calls.

    describe "Error Sanitization (security)" do
      -- CRITICAL: Error messages must not leak sensitive data
      -- HTTP exceptions can include request body (which may contain secrets)

      it "error messages do not contain request details" \_ -> do
        -- When a request fails, the error message should be sanitized
        -- to prevent leaking sensitive request body/headers
        let req =
              Http.request
                |> Http.withUrl "http://localhost:59997/test"
                |> Http.withTimeout 1
        result <- Http.postForm @() req [("client_secret", "super-secret-value")] |> Task.asResult
        case result of
          Err (Error msg) -> do
            -- The error message should NOT contain the secret
            msg |> shouldSatisfy (\t -> not (Text.contains "super-secret-value" t))
            -- Should contain useful info (host/category)
            msg |> shouldSatisfy (\t -> Text.contains "localhost" t)
          Err (InvalidUrl _) -> fail "Unexpected InvalidUrl from Http.postForm"
          Ok _ -> fail "Expected error"

    -- NOTE: getRaw is NOT exported from Http.Client (compile-time guarantee).
    -- Callers must use Http.Client.Internal to access getRaw for trusted/localhost callers.

    describe "getSecure" do
      it "rejects http:// URL with InvalidUrl error" \_ -> do
        let req = Http.request |> Http.withUrl "http://example.com"
        result <- Http.getSecure req |> Task.asResult
        case result of
          Err (InvalidUrl _sanitized) -> Task.yield ()
          Err (Error _) -> fail "Expected InvalidUrl error, not generic Error"
          Ok _ -> fail "Expected error for http:// URL"

      it "rejects HTTP:// URL (uppercase not recognized as https://)" \_ -> do
        -- Text.startsWith is case-sensitive; 'HTTP://' does not start with 'https://'
        let req = Http.request |> Http.withUrl "HTTP://example.com"
        result <- Http.getSecure req |> Task.asResult
        case result of
          Err (InvalidUrl _) -> Task.yield ()
          Err (Error _) -> fail "Expected InvalidUrl error, not generic Error"
          Ok _ -> fail "Expected error for uppercase HTTP:// URL"

      it "error message strips query parameters (sanitizeUrlText applied)" \_ -> do
        let req = Http.request |> Http.withUrl "http://example.com/path?secret=super-secret-token&other=value"
        result <- Http.getSecure req |> Task.asResult
        case result of
          Err (InvalidUrl sanitized) ->
            -- SECURITY: Query params must be stripped to prevent secret leakage in logs
            sanitized |> shouldSatisfy (\u -> not (Text.contains "super-secret-token" u))
          Err (Error _) -> fail "Expected InvalidUrl error, not generic Error"
          Ok _ -> fail "Expected error for http:// URL"

      it "https:// URL passes scheme check (does not return InvalidUrl)" \_ -> do
        -- https:// passes the scheme guard; connection failure expected but NOT InvalidUrl
        let req =
              Http.request
                |> Http.withUrl "https://localhost:59996/neohaskell-test-nonexistent"
                |> Http.withTimeout 1
        result <- Http.getSecure req |> Task.asResult
        case result of
          Err (InvalidUrl _) -> fail "https:// URLs must NOT be rejected with InvalidUrl"
          Err (Error _) -> Task.yield () -- Connection error expected
          Ok _ -> Task.yield () -- OK if something is running on port 59996
