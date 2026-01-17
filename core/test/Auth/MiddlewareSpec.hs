module Auth.MiddlewareSpec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Auth.Error (AuthError (..))
import Auth.Middleware (AuthContext (..), checkAuth, extractToken)
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
          isTokenMissing `shouldSatisfy` result

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
          isTokenExpired `shouldSatisfy` result

      describe "RequireAllPermissions" do
        it "accepts when user has all required permissions" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          let perms = Array.fromLinkedList ["read:users", "write:users"]
          token <- JwtCore.signTokenWithPermissions keys perms
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          let required = Array.fromLinkedList ["read:users", "write:users"]
          result <- checkAuth (Just manager) config (RequireAllPermissions required) request
          isOkContext `shouldSatisfy` result

        it "rejects when user is missing a permission" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          let perms = Array.fromLinkedList ["read:users"]
          token <- JwtCore.signTokenWithPermissions keys perms
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          let required = Array.fromLinkedList ["read:users", "write:users"]
          result <- checkAuth (Just manager) config (RequireAllPermissions required) request
          isInsufficientPermissions `shouldSatisfy` result

      describe "RequireAnyPermission" do
        it "accepts when user has at least one required permission" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          let perms = Array.fromLinkedList ["read:users"]
          token <- JwtCore.signTokenWithPermissions keys perms
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          let required = Array.fromLinkedList ["read:users", "admin"]
          result <- checkAuth (Just manager) config (RequireAnyPermission required) request
          isOkContext `shouldSatisfy` result

        it "rejects when user has none of the required permissions" \_ -> do
          keys <- JwtCore.testKeys
          manager <- JwtCore.createTestManager keys
          let perms = Array.fromLinkedList ["other:permission"]
          token <- JwtCore.signTokenWithPermissions keys perms
          let config = JwtCore.testConfig
          let request = mockRequestWithAuth [fmt|Bearer #{token}|]
          let required = Array.fromLinkedList ["read:users", "admin"]
          result <- checkAuth (Just manager) config (RequireAnyPermission required) request
          isInsufficientPermissions `shouldSatisfy` result


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


isInsufficientPermissions :: Result AuthError AuthContext -> Bool
isInsufficientPermissions result =
  case result of
    Err (InsufficientPermissions _) -> True
    _ -> False


isOkContext :: Result AuthError AuthContext -> Bool
isOkContext result =
  case result of
    Ok _ -> True
    _ -> False
