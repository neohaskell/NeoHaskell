module Auth.OAuth2.ClientSpec where

import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Types (
  AuthorizationCode (..),
  ClientId (..),
  ClientSecret (..),
  OAuth2Error (..),
  Provider (..),
  RedirectUri (..),
  RefreshToken (..),
 )
import Core
import Task qualified
import Test


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
              (ClientSecret "client-secret")
              (RedirectUri "https://myapp.com/callback")
              (AuthorizationCode "auth-code")
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
              (ClientSecret "client-secret")
              (RedirectUri "https://myapp.com/callback")
              (AuthorizationCode "auth-code")
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
              (ClientSecret "client-secret")
              (RedirectUri "https://myapp.com/callback")
              (AuthorizationCode "auth-code")
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
              (ClientSecret "client-secret")
              (RefreshToken "refresh-token")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP endpoint"

    -- NOTE: Testing actual OAuth2 error parsing requires a mock HTTP server
    -- which is out of scope for unit tests. The error parsing logic is
    -- tested implicitly through integration tests with real OAuth2 providers.
    -- 
    -- The design ensures:
    -- 1. HTTP 4xx responses with error JSON are parsed into specific error types
    -- 2. Network failures return NetworkError
    -- 3. Validation failures return EndpointValidationFailed
