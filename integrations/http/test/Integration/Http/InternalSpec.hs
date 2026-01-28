{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Http.InternalSpec where

import Core
import Integration qualified
import Integration.Http.Auth qualified as Auth
import Integration.Http.Internal qualified as Internal
import Integration.Http.Retry qualified as Retry
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Integration.Http.Internal" do
    describe "expandEnvVars" do
      it "returns text unchanged when no variables" \_ -> do
        result <- Internal.expandEnvVars "https://api.example.com/v1/orders"
          |> Task.mapError toText
        result |> shouldBe "https://api.example.com/v1/orders"

      it "handles empty string" \_ -> do
        result <- Internal.expandEnvVars ""
          |> Task.mapError toText
        result |> shouldBe ""

      it "expands HOME variable" \_ -> do
        -- HOME is almost always set on Unix systems
        result <- Internal.expandEnvVars "path: ${HOME}/test"
          |> Task.mapError toText
        -- Result should NOT contain the literal ${HOME}
        result |> Text.contains "${HOME}" |> shouldBe False
        -- Result should contain "path: " prefix
        result |> Text.contains "path: " |> shouldBe True

      it "expands variable at start of string" \_ -> do
        result <- Internal.expandEnvVars "${HOME}"
          |> Task.mapError toText
        -- Should not be empty (HOME exists) and should not contain ${
        result |> Text.contains "${" |> shouldBe False
        Text.length result |> shouldSatisfy (> 0)

      it "expands variable at end of string" \_ -> do
        result <- Internal.expandEnvVars "home=${HOME}"
          |> Task.mapError toText
        result |> Text.contains "${HOME}" |> shouldBe False
        result |> Text.contains "home=" |> shouldBe True

      it "expands multiple variables" \_ -> do
        -- Use HOME twice since we can't set custom env vars
        result <- Internal.expandEnvVars "${HOME}:${HOME}"
          |> Task.mapError toText
        result |> Text.contains "${" |> shouldBe False
        result |> Text.contains ":" |> shouldBe True

      it "throws AuthenticationError on missing variable" \_ -> do
        result <- Internal.expandEnvVars "${NONEXISTENT_VAR_XYZ_12345}"
          |> Task.asResult
        case result of
          Err (Integration.AuthenticationError msg) ->
            msg |> Text.contains "NONEXISTENT_VAR_XYZ_12345" |> shouldBe True
          Err _ -> fail "Expected AuthenticationError"
          Ok _ -> fail "Expected error for missing variable"

      it "handles text without ${} patterns" \_ -> do
        result <- Internal.expandEnvVars "no variables here"
          |> Task.mapError toText
        result |> shouldBe "no variables here"

      it "handles text with $ but not full pattern" \_ -> do
        result <- Internal.expandEnvVars "price: $100"
          |> Task.mapError toText
        result |> shouldBe "price: $100"

    describe "buildAuthHeader" do
      it "returns Nothing for NoAuth" \_ -> do
        result <- Internal.buildAuthHeader Auth.NoAuth
          |> Task.mapError toText
        result |> shouldBe Nothing

      it "builds Bearer header with literal token" \_ -> do
        result <- Internal.buildAuthHeader (Auth.Bearer "literal-token")
          |> Task.mapError toText
        case result of
          Just (name, value) -> do
            name |> shouldBe "Authorization"
            value |> shouldBe "Bearer literal-token"
          Nothing -> fail "Expected Just header"

      it "builds Basic header with base64 encoding" \_ -> do
        -- Basic auth: base64("user:pass") = "dXNlcjpwYXNz"
        result <- Internal.buildAuthHeader (Auth.Basic { username = "user", password = "pass" })
          |> Task.mapError toText
        case result of
          Just (name, value) -> do
            name |> shouldBe "Authorization"
            value |> Text.contains "Basic " |> shouldBe True
            value |> Text.contains "dXNlcjpwYXNz" |> shouldBe True
          Nothing -> fail "Expected Just header"

      it "builds Basic header with empty password" \_ -> do
        -- Common for API keys used as username with empty password
        -- base64("apikey:") = "YXBpa2V5Og=="
        result <- Internal.buildAuthHeader (Auth.Basic { username = "apikey", password = "" })
          |> Task.mapError toText
        case result of
          Just (name, value) -> do
            name |> shouldBe "Authorization"
            value |> shouldBe "Basic YXBpa2V5Og=="
          Nothing -> fail "Expected Just header"

      it "builds ApiKey header" \_ -> do
        result <- Internal.buildAuthHeader (Auth.ApiKey { headerName = "X-Api-Key", headerValue = "key123" })
          |> Task.mapError toText
        case result of
          Just (name, value) -> do
            name |> shouldBe "X-Api-Key"
            value |> shouldBe "key123"
          Nothing -> fail "Expected Just header"

      it "builds ApiKey with custom header name" \_ -> do
        result <- Internal.buildAuthHeader (Auth.ApiKey { headerName = "X-Custom-Auth", headerValue = "secret" })
          |> Task.mapError toText
        case result of
          Just (name, value) -> do
            name |> shouldBe "X-Custom-Auth"
            value |> shouldBe "secret"
          Nothing -> fail "Expected Just header"

    describe "calculateBackoff" do
      it "first attempt uses initialDelayMs" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 3
              , initialDelayMs = 1000
              , maxDelayMs = 30000
              , retryableStatuses = []
              }
        result <- Internal.calculateBackoff config 1
          |> Task.mapError toText
        -- First attempt: delay = 1000 * 2^0 = 1000, plus jitter [0, 250]
        result |> shouldSatisfy (\d -> d >= 1000 && d <= 1250)

      it "second attempt doubles delay" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 3
              , initialDelayMs = 1000
              , maxDelayMs = 30000
              , retryableStatuses = []
              }
        result <- Internal.calculateBackoff config 2
          |> Task.mapError toText
        -- Second attempt: delay = 1000 * 2^1 = 2000, plus jitter [0, 500]
        result |> shouldSatisfy (\d -> d >= 2000 && d <= 2500)

      it "third attempt quadruples initial delay" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 3
              , initialDelayMs = 1000
              , maxDelayMs = 30000
              , retryableStatuses = []
              }
        result <- Internal.calculateBackoff config 3
          |> Task.mapError toText
        -- Third attempt: delay = 1000 * 2^2 = 4000, plus jitter [0, 1000]
        result |> shouldSatisfy (\d -> d >= 4000 && d <= 5000)

      it "caps at maxDelayMs" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 10
              , initialDelayMs = 1000
              , maxDelayMs = 5000
              , retryableStatuses = []
              }
        result <- Internal.calculateBackoff config 10
          |> Task.mapError toText
        -- 10th attempt: delay = min(1000 * 2^9, 5000) = 5000, plus jitter [0, 1250]
        result |> shouldSatisfy (\d -> d >= 5000 && d <= 6250)

      it "handles zero initial delay" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 3
              , initialDelayMs = 0
              , maxDelayMs = 30000
              , retryableStatuses = []
              }
        result <- Internal.calculateBackoff config 1
          |> Task.mapError toText
        -- delay = 0 * 2^0 = 0, jitter = random in [0, max(1, 0/4)] = [0, 1]
        result |> shouldSatisfy (\d -> d >= 0 && d <= 1)

      it "exponential growth is correct" \_ -> do
        let config = Retry.Retry
              { maxAttempts = 5
              , initialDelayMs = 100
              , maxDelayMs = 100000
              , retryableStatuses = []
              }
        -- Test without jitter influence by checking minimum values
        d1 <- Internal.calculateBackoff config 1 |> Task.mapError toText
        d2 <- Internal.calculateBackoff config 2 |> Task.mapError toText
        d3 <- Internal.calculateBackoff config 3 |> Task.mapError toText
        -- d1 >= 100 (base), d2 >= 200, d3 >= 400
        d1 |> shouldSatisfy (>= 100)
        d2 |> shouldSatisfy (>= 200)
        d3 |> shouldSatisfy (>= 400)
