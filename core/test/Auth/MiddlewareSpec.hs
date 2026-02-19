module Auth.MiddlewareSpec where

import Auth.Claims (UserClaims (..))
import Auth.Error (AuthError (..))
import Auth.Middleware (AuthContext (..), checkAuth, checkAuthWithToken, extractToken, extractTokenFromQuery)
import Auth.Options (AuthOptions (..))
import Core
import Data.Text.Encoding qualified as GhcTextEncoding
import Network.HTTP.Types.Header qualified as HTTP
import Network.Wai qualified as Wai
import Network.Wai.Internal qualified as WaiInternal
import Test
import Test.Auth.Jwt.Core qualified as JwtCore


spec :: Spec Unit
spec = do
  describe "Auth.Middleware" do
    describe "extractToken" do
      it "extracts Bearer token from Authorization header" \_ -> do
        let request = mockRequestWithAuth "Bearer my-jwt-token"
        extractToken request `shouldBe` Just "my-jwt-token"

      it "returns Nothing when no Authorization header" \_ -> do
        let request = mockRequestWithoutAuth
        extractToken request `shouldBe` Nothing

      it "returns Nothing when Authorization header is not Bearer" \_ -> do
        let request = mockRequestWithAuth "Basic dXNlcjpwYXNz"
        extractToken request `shouldBe` Nothing

      it "handles empty Bearer token" \_ -> do
        let request = mockRequestWithAuth "Bearer "
        extractToken request `shouldBe` Just ""

      it "trims whitespace around Bearer token" \_ -> do
        let request = mockRequestWithAuth "  Bearer  my-token  "
        extractToken request `shouldBe` Just "my-token"

      it "handles case-insensitive Bearer with whitespace" \_ -> do
        let request = mockRequestWithAuth "  bearer   token123  "
        extractToken request `shouldBe` Just "token123"

      -- Regression: extractToken must NEVER read query params (scoped security)
      it "ignores token query parameter (header-only)" \_ -> do
        let request = mockRequestWithQueryParam "token" "query-param-token"
        extractToken request `shouldBe` Nothing

      it "ignores token query parameter when non-Bearer header present" \_ -> do
        let request = mockRequestWithAuthAndQueryParam "Basic dXNlcjpwYXNz" "token" "query-param-token"
        extractToken request `shouldBe` Nothing

    describe "extractTokenFromQuery" do
      it "extracts token from query parameter" \_ -> do
        let request = mockRequestWithQueryParam "token" "my-jwt-token"
        extractTokenFromQuery request `shouldBe` Just "my-jwt-token"

      it "returns Nothing when no token query parameter" \_ -> do
        let request = mockRequestWithoutAuth
        extractTokenFromQuery request `shouldBe` Nothing

      it "returns Nothing when token param has no value" \_ -> do
        let request = mockRequestWithQueryParamNoValue "token"
        extractTokenFromQuery request `shouldBe` Nothing

      it "returns Nothing when query param has different name" \_ -> do
        let request = mockRequestWithQueryParam "auth" "some-token"
        extractTokenFromQuery request `shouldBe` Nothing

      it "ignores Authorization header (query-only)" \_ -> do
        let request = mockRequestWithAuth "Bearer header-token"
        extractTokenFromQuery request `shouldBe` Nothing

      it "extracts token when both header and query param present" \_ -> do
        let request = mockRequestWithAuthAndQueryParam "Bearer header-token" "token" "query-token"
        extractTokenFromQuery request `shouldBe` Just "query-token"

    describe "checkAuthWithToken" do
      it "validates a pre-extracted valid token" \_ -> do
        keys <- JwtCore.testKeys
        manager <- JwtCore.createTestManager keys
        token <- JwtCore.signValidToken keys
        let config = JwtCore.testConfig
        result <- checkAuthWithToken (Just manager) config token
        case result of
          Ok ctx -> do
            ctx.isAuthenticated `shouldBe` True
            case ctx.claims of
              Just claims -> claims.sub `shouldBe` "test-user-123"
              Nothing -> fail "Expected claims"
          Err err -> fail [fmt|Expected Ok but got Err: #{toText err}|]

      it "rejects an expired pre-extracted token" \_ -> do
        keys <- JwtCore.testKeys
        manager <- JwtCore.createTestManager keys
        token <- JwtCore.signExpiredToken keys
        let config = JwtCore.testConfig
        result <- checkAuthWithToken (Just manager) config token
        result |> shouldSatisfy isTokenExpired

      it "returns error when auth not configured" \_ -> do
        let config = JwtCore.testConfig
        result <- checkAuthWithToken Nothing config "some-token"
        result |> shouldSatisfy isAuthInfraUnavailable

    describe "checkAuth" do
      describe "Everyone (public endpoints)" do
        it "allows requests without auth" \_ -> do
          let config = JwtCore.testConfig
          result <- checkAuth Nothing config Everyone mockRequestWithoutAuth
          case result of
            Ok ctx -> ctx.isAuthenticated `shouldBe` False
            Err _err -> fail "Expected Ok for public endpoint"

      describe "Authenticated (protected endpoints)" do
        it "rejects requests without token" \_ -> do
          -- No JWKS manager means auth not configured
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          let config = JwtCore.testConfig
          result <- checkAuth (Just manager) config Authenticated mockRequestWithoutAuth
          result |> shouldSatisfy isTokenMissing

        it "accepts requests with valid token" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          token <- JwtCore.signValidToken keys
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          result <- checkAuth (Just manager) config Authenticated request
          case result of
            Ok ctx -> do
              ctx.isAuthenticated `shouldBe` True
              case ctx.claims of
                Just claims -> claims.sub `shouldBe` "test-user-123"
                Nothing -> fail "Expected claims"
            Err err -> fail [fmt|Expected Ok but got Err: #{toText err}|]

        it "rejects requests with expired token" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          token <- JwtCore.signExpiredToken keys
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          result <- checkAuth (Just manager) config Authenticated request
          result |> shouldSatisfy isTokenExpired


-- | Create a mock WAI request with an Authorization header
mockRequestWithAuth :: Text -> Wai.Request
mockRequestWithAuth authHeader =
  Wai.defaultRequest
    { WaiInternal.requestHeaders =
        [(HTTP.hAuthorization, GhcTextEncoding.encodeUtf8 authHeader)]
    }


-- | Create a mock WAI request without Authorization header
mockRequestWithoutAuth :: Wai.Request
mockRequestWithoutAuth = Wai.defaultRequest


-- | Create a mock WAI request with a query parameter
mockRequestWithQueryParam :: Text -> Text -> Wai.Request
mockRequestWithQueryParam paramName paramValue =
  Wai.defaultRequest
    { WaiInternal.queryString =
        [(GhcTextEncoding.encodeUtf8 paramName, Just (GhcTextEncoding.encodeUtf8 paramValue))]
    }


-- | Create a mock WAI request with a query parameter that has no value
mockRequestWithQueryParamNoValue :: Text -> Wai.Request
mockRequestWithQueryParamNoValue paramName =
  Wai.defaultRequest
    { WaiInternal.queryString =
        [(GhcTextEncoding.encodeUtf8 paramName, Nothing)]
    }


-- | Create a mock WAI request with both Authorization header and query parameter
mockRequestWithAuthAndQueryParam :: Text -> Text -> Text -> Wai.Request
mockRequestWithAuthAndQueryParam authHeader paramName paramValue =
  Wai.defaultRequest
    { WaiInternal.requestHeaders =
        [(HTTP.hAuthorization, GhcTextEncoding.encodeUtf8 authHeader)]
    , WaiInternal.queryString =
        [(GhcTextEncoding.encodeUtf8 paramName, Just (GhcTextEncoding.encodeUtf8 paramValue))]
    }


-- Helper predicates
isTokenMissing :: Result AuthError AuthContext -> Bool
isTokenMissing result =
  case result of
    Err TokenMissing -> True
    _ -> False


isTokenExpired :: Result AuthError AuthContext -> Bool
isTokenExpired result =
  case result of
    Err TokenExpired -> True
    _ -> False


isAuthInfraUnavailable :: Result AuthError AuthContext -> Bool
isAuthInfraUnavailable result =
  case result of
    Err (AuthInfraUnavailable _) -> True
    _ -> False
