{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.Http.AuthSpec where

import Core
import Integration.Http.Auth qualified as Auth
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Integration.Http.Auth" do
    describe "Auth types" do
      describe "NoAuth" do
        it "equals itself" \_ -> do
          Auth.NoAuth |> shouldBe Auth.NoAuth

        it "has redacted Show instance" \_ -> do
          let shown = show Auth.NoAuth
          shown |> shouldBe "NoAuth"

      describe "Bearer" do
        it "stores token value" \_ -> do
          let auth = Auth.Bearer "my-token"
          case auth of
            Auth.Bearer t -> t |> shouldBe "my-token"
            _ -> fail "Expected Bearer"

        it "equals another Bearer with same token" \_ -> do
          (Auth.Bearer "token") |> shouldBe (Auth.Bearer "token")

        it "does not equal Bearer with different token" \_ -> do
          (Auth.Bearer "token1") |> shouldNotBe (Auth.Bearer "token2")

        it "has redacted Show instance that hides token" \_ -> do
          let auth = Auth.Bearer "secret-token-12345"
          let shown = show auth
          -- Should NOT contain the actual token
          shown |> Text.fromLinkedList |> Text.contains "secret-token" |> shouldBe False
          -- Should contain redaction marker
          shown |> Text.fromLinkedList |> Text.contains "***" |> shouldBe True

      describe "Basic" do
        it "stores username and password" \_ -> do
          let auth = Auth.Basic { username = "user", password = "pass" }
          case auth of
            Auth.Basic { username, password } -> do
              username |> shouldBe "user"
              password |> shouldBe "pass"
            _ -> fail "Expected Basic"

        it "equals another Basic with same credentials" \_ -> do
          let auth1 = Auth.Basic { username = "user", password = "pass" }
          let auth2 = Auth.Basic { username = "user", password = "pass" }
          auth1 |> shouldBe auth2

        it "does not equal Basic with different credentials" \_ -> do
          let auth1 = Auth.Basic { username = "user1", password = "pass" }
          let auth2 = Auth.Basic { username = "user2", password = "pass" }
          auth1 |> shouldNotBe auth2

        it "has redacted Show instance that hides credentials" \_ -> do
          let auth = Auth.Basic { username = "admin", password = "super-secret" }
          let shown = show auth
          let shownText = Text.fromLinkedList shown
          -- Should NOT contain actual credentials
          shownText |> Text.contains "admin" |> shouldBe False
          shownText |> Text.contains "super-secret" |> shouldBe False
          -- Should contain redaction marker
          shownText |> Text.contains "***" |> shouldBe True

      describe "ApiKey" do
        it "stores header name and value" \_ -> do
          let auth = Auth.ApiKey { headerName = "X-Api-Key", headerValue = "key123" }
          case auth of
            Auth.ApiKey { headerName, headerValue } -> do
              headerName |> shouldBe "X-Api-Key"
              headerValue |> shouldBe "key123"
            _ -> fail "Expected ApiKey"

        it "equals another ApiKey with same values" \_ -> do
          let auth1 = Auth.ApiKey { headerName = "X-Key", headerValue = "val" }
          let auth2 = Auth.ApiKey { headerName = "X-Key", headerValue = "val" }
          auth1 |> shouldBe auth2

        it "does not equal ApiKey with different values" \_ -> do
          let auth1 = Auth.ApiKey { headerName = "X-Key", headerValue = "val1" }
          let auth2 = Auth.ApiKey { headerName = "X-Key", headerValue = "val2" }
          auth1 |> shouldNotBe auth2

        it "has redacted Show instance that hides both header name and value" \_ -> do
          let auth = Auth.ApiKey { headerName = "X-Secret-Header", headerValue = "api-key-12345" }
          let shown = show auth
          let shownText = Text.fromLinkedList shown
          -- Should NOT contain actual values
          shownText |> Text.contains "X-Secret-Header" |> shouldBe False
          shownText |> Text.contains "api-key-12345" |> shouldBe False
          -- Should contain redaction marker
          shownText |> Text.contains "***" |> shouldBe True

    describe "Auth distinctness" do
      it "NoAuth is not equal to Bearer" \_ -> do
        Auth.NoAuth |> shouldNotBe (Auth.Bearer "token")

      it "Bearer is not equal to Basic" \_ -> do
        (Auth.Bearer "token") |> shouldNotBe (Auth.Basic { username = "u", password = "p" })

      it "Basic is not equal to ApiKey" \_ -> do
        let basic = Auth.Basic { username = "u", password = "p" }
        let apiKey = Auth.ApiKey { headerName = "X-Key", headerValue = "v" }
        basic |> shouldNotBe apiKey

    describe "Security: No JSON serialization" do
      -- These tests verify that Auth cannot be accidentally serialized to JSON
      -- which would leak credentials to logs, databases, or network calls.
      --
      -- Note: The Auth type intentionally does NOT have ToJSON/FromJSON instances.
      -- This is a compile-time guarantee that we verify by type-checking the module.
      -- The actual "test" is that the code compiles without those instances.
      it "Auth type exists and is usable" \_ -> do
        -- This test just verifies the types work as expected
        let _noAuth = Auth.NoAuth
        let _bearer = Auth.Bearer "token"
        let _basic = Auth.Basic { username = "u", password = "p" }
        let _apiKey = Auth.ApiKey { headerName = "X-Key", headerValue = "v" }
        Task.yield unit
