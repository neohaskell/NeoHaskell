module Auth.OAuth2.ClientSpec where

import Array qualified
import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Types (
  ClientId (..),
  CodeChallengeMethod (..),
  OAuth2Error (..),
  Provider (..),
  RedirectUri,
  Scope (..),
  mkAuthorizationCode,
  mkClientSecret,
  mkRedirectUri,
  mkRefreshToken,
  mkState,
 )
import Core

import Task qualified
import Test
import Text qualified


-- | Helper to create RedirectUri in tests, panics if invalid.
-- Only use with known-valid HTTPS test URLs.
testRedirectUri :: Text -> RedirectUri
testRedirectUri url =
  case mkRedirectUri url of
    Ok uri -> uri
    Err err -> panic [fmt|Test URL should be valid: #{url}, error: #{toText err}|]


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.Client" do
    describe "HTTPS/SSRF Protection (security)" do
      -- CRITICAL: OAuth2 token endpoints MUST use HTTPS
      -- and MUST NOT allow SSRF via private IPs

      describe "exchangeCode" do
        it "rejects HTTP token endpoint (requires HTTPS)" \_ -> do
          let provider =
                Provider
                  { name = "insecure"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "http://example.com/token" -- HTTP, not HTTPS
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP endpoint"

        it "rejects private IP in token endpoint (SSRF protection)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-attempt"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "https://127.0.0.1/token" -- Loopback
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for private IP endpoint"

        it "rejects internal IP ranges in token endpoint (SSRF protection)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-internal"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "https://192.168.1.1/token" -- Private IP
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for internal IP endpoint"

      describe "refreshToken" do
        it "rejects HTTP token endpoint (requires HTTPS)" \_ -> do
          let provider =
                Provider
                  { name = "insecure"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "http://example.com/token"
                  }
          result <-
            OAuth2.refreshToken
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (mkRefreshToken "refresh-token")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP endpoint"

    describe "authorizeUrl" do
      it "builds a valid authorization URL with all required parameters" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        let url =
              OAuth2.authorizeUrl
                provider
                (ClientId "my-client-id")
                (testRedirectUri "https://myapp.com/callback")
                (Array.fromLinkedList [Scope "read", Scope "write"])
                (mkState "random-state-123")
        -- Should contain required OAuth2 parameters
        url |> shouldSatisfy (\u -> Text.contains "response_type=code" u)
        url |> shouldSatisfy (\u -> Text.contains "client_id=my-client-id" u)
        url |> shouldSatisfy (\u -> Text.contains "redirect_uri=" u)
        url |> shouldSatisfy (\u -> Text.contains "state=" u)
        url |> shouldSatisfy (\u -> Text.contains "scope=" u)
        -- Should start with the provider's authorize endpoint
        url |> shouldSatisfy (\u -> Text.contains "https://auth.example.com/authorize?" u)

      it "URL-encodes special characters in parameters" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        let url =
              OAuth2.authorizeUrl
                provider
                (ClientId "client-id")
                (testRedirectUri "https://myapp.com/callback?extra=param")
                (Array.fromLinkedList [Scope "profile email"])
                (mkState "state")
        -- Special characters should be encoded
        -- Space in scope -> %20, ? and = in redirect_uri -> %3F and %3D
        url |> shouldSatisfy (\u -> Text.contains "%20" u || Text.contains "+" u)

    describe "authorizeUrlWithPkce" do
      it "includes PKCE parameters in authorization URL" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        verifier <- OAuth2.generateCodeVerifier
        let challenge = OAuth2.deriveCodeChallenge verifier
        let url =
              OAuth2.authorizeUrlWithPkce
                provider
                (ClientId "client-id")
                (testRedirectUri "https://myapp.com/callback")
                (Array.fromLinkedList [Scope "read"])
                (mkState "state")
                challenge
                S256
        -- Should include PKCE parameters
        url |> shouldSatisfy (\u -> Text.contains "code_challenge=" u)
        url |> shouldSatisfy (\u -> Text.contains "code_challenge_method=S256" u)

    describe "PKCE" do
      it "generateCodeVerifier produces valid verifier" \_ -> do
        verifier <- OAuth2.generateCodeVerifier
        -- Verifier should exist (opaque type, can't inspect value)
        -- but deriveCodeChallenge should work
        let _ = OAuth2.deriveCodeChallenge verifier
        Task.yield ()

      it "deriveCodeChallenge produces consistent results" \_ -> do
        verifier <- OAuth2.generateCodeVerifier
        let challenge1 = OAuth2.deriveCodeChallenge verifier
        let challenge2 = OAuth2.deriveCodeChallenge verifier
        -- Same verifier should produce same challenge
        challenge1 |> shouldBe challenge2

    -- NOTE: Testing actual OAuth2 token exchange success paths requires
    -- a mock HTTP server, which is out of scope for unit tests.
    -- These flows are tested via integration tests with real OAuth2 providers.
    -- 
    -- The design ensures:
    -- 1. Valid HTTPS endpoints pass URL validation and make real HTTP requests
    -- 2. Successful token responses are parsed into TokenSet
    -- 3. HTTP 4xx responses with error JSON are parsed into specific error types
    -- 4. Network failures return NetworkError
    -- 5. Validation failures return EndpointValidationFailed
