module Auth.JwtSpec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Auth.Config (AuthConfig (..))
import Auth.Error (AuthError (..))
import Core
import Test
import Test.Auth.Jwt.Core qualified as JwtCore


spec :: Spec Unit
spec = do
  describe "Auth.Jwt" do
    describe "validateToken" do
      -- RFC 8725 hardening tests
      describe "RFC 8725 JOSE hardening" do
        it "rejects alg=none tokens" \_ -> do
          -- alg=none is a security vulnerability (CVE-2015-9235)
          let token = JwtCore.makeAlgNoneToken
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenFormat config token
          result |> shouldSatisfy isAlgorithmNotAllowed

        it "rejects algorithms not in allowlist" \_ -> do
          -- Token signed with HS256 should be rejected when only RSA/EC allowed
          let token = JwtCore.makeHS256Token
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenFormat config token
          result |> shouldSatisfy isAlgorithmNotAllowed

        it "accepts ES256 tokens when in allowlist" \_ -> do
          -- Generate keys and sign a valid token
          keys <- JwtCore.testKeys
          token <- JwtCore.signValidToken keys
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isOk

        it "accepts RS256 tokens when in allowlist" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signValidTokenRS256 keys
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.rs256Key]) token
          result |> shouldSatisfy isOk

      -- Token format validation
      describe "token format" do
        it "rejects malformed tokens" \_ -> do
          let token = "not.a.valid.jwt"
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenFormat config token
          result |> shouldSatisfy isTokenMalformed

        it "rejects empty tokens" \_ -> do
          let token = ""
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenFormat config token
          result |> shouldSatisfy isTokenMalformed

      -- Time validation
      describe "time validation" do
        it "rejects expired tokens" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signExpiredToken keys
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isTokenExpired

        it "rejects not-yet-valid tokens (nbf in future)" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signNotYetValidToken keys
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isTokenNotYetValid

        it "accepts tokens within clock skew tolerance" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signRecentlyExpiredToken keys
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isOk

      -- Issuer validation
      describe "issuer validation" do
        it "rejects tokens with wrong issuer" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithIssuer keys "https://wrong.issuer.com"
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isIssuerMismatch

        it "accepts tokens with correct issuer" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithIssuer keys "https://auth.example.com"
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isOk

      -- Audience validation
      describe "audience validation" do
        it "rejects tokens with wrong audience when configured" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithAudience keys "wrong-audience"
          let config = JwtCore.testConfigWithAudience "my-api"
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isAudienceMismatch

        it "accepts tokens with correct audience" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithAudience keys "my-api"
          let config = JwtCore.testConfigWithAudience "my-api"
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isOk

        it "accepts tokens when no audience configured" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signValidToken keys
          let config = JwtCore.testConfig -- No audience configured
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          result |> shouldSatisfy isOk

      -- Claims extraction
      describe "claims extraction" do
        it "extracts sub claim as user ID" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithSub keys "user-abc-123"
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          case result of
            Ok claims -> claims.sub `shouldBe` "user-abc-123"
            Err _err -> fail "Expected Ok but got Err"

        it "extracts email claim when present" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithEmail keys "user@example.com"
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          case result of
            Ok claims -> claims.email `shouldBe` Just "user@example.com"
            Err _err -> fail "Expected Ok but got Err"

        it "extracts permissions from configured claim" \_ -> do
          keys <- JwtCore.testKeys
          let perms = Array.fromLinkedList ["read:users", "write:users"]
          token <- JwtCore.signTokenWithPermissions keys perms
          let config = JwtCore.testConfig
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          case result of
            Ok claims -> claims.permissions `shouldBe` perms
            Err _err -> fail "Expected Ok but got Err"

        it "extracts tenantId when configured" \_ -> do
          keys <- JwtCore.testKeys
          token <- JwtCore.signTokenWithTenantId keys "tenant-xyz"
          let config = JwtCore.testConfig {tenantIdClaim = Just "tenant_id"}
          result <- JwtCore.validateTokenWithKeys config (Array.fromLinkedList [keys.es256Key]) token
          case result of
            Ok claims -> claims.tenantId `shouldBe` Just "tenant-xyz"
            Err _err -> fail "Expected Ok but got Err"


-- Helper predicates for test assertions
isAlgorithmNotAllowed :: Result AuthError UserClaims -> Bool
isAlgorithmNotAllowed result =
  case result of
    Err (AlgorithmNotAllowed _) -> True
    _ -> False


isTokenMalformed :: Result AuthError UserClaims -> Bool
isTokenMalformed result =
  case result of
    Err (TokenMalformed _) -> True
    _ -> False


isTokenExpired :: Result AuthError UserClaims -> Bool
isTokenExpired result =
  case result of
    Err TokenExpired -> True
    _ -> False


isTokenNotYetValid :: Result AuthError UserClaims -> Bool
isTokenNotYetValid result =
  case result of
    Err TokenNotYetValid -> True
    _ -> False


isIssuerMismatch :: Result AuthError UserClaims -> Bool
isIssuerMismatch result =
  case result of
    Err (IssuerMismatch _ _) -> True
    _ -> False


isAudienceMismatch :: Result AuthError UserClaims -> Bool
isAudienceMismatch result =
  case result of
    Err (AudienceMismatch _ _) -> True
    _ -> False


isOk :: Result AuthError UserClaims -> Bool
isOk result =
  case result of
    Ok _ -> True
    _ -> False
