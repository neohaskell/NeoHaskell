module Auth.JwtSpec where

import Auth.Claims (UserClaims)
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
          result <- JwtCore.validateToken config token
          isAlgorithmNotAllowed `shouldSatisfy` result

        it "rejects algorithms not in allowlist" \_ -> do
          -- Token signed with HS256 should be rejected when only RSA/EC allowed
          let token = JwtCore.makeHS256Token
          let config = JwtCore.testConfig
          result <- JwtCore.validateToken config token
          isAlgorithmNotAllowed `shouldSatisfy` result

        it "accepts ES256 tokens when in allowlist" \_ -> do
          pending "Requires proper token generation"

        it "accepts RS256 tokens when in allowlist" \_ -> do
          pending "Requires proper token generation"

      -- Token format validation
      describe "token format" do
        it "rejects malformed tokens" \_ -> do
          let token = "not.a.valid.jwt"
          let config = JwtCore.testConfig
          result <- JwtCore.validateToken config token
          isTokenMalformed `shouldSatisfy` result

        it "rejects empty tokens" \_ -> do
          let token = ""
          let config = JwtCore.testConfig
          result <- JwtCore.validateToken config token
          isTokenMalformed `shouldSatisfy` result

      -- Time validation
      describe "time validation" do
        it "rejects expired tokens" \_ -> do
          pending "Requires proper token generation with exp claim"

        it "rejects not-yet-valid tokens (nbf in future)" \_ -> do
          pending "Requires proper token generation with nbf claim"

        it "accepts tokens within clock skew tolerance" \_ -> do
          pending "Requires proper token generation with exp claim"

      -- Issuer validation
      describe "issuer validation" do
        it "rejects tokens with wrong issuer" \_ -> do
          pending "Requires proper token generation with iss claim"

        it "accepts tokens with correct issuer" \_ -> do
          pending "Requires proper token generation with iss claim"

      -- Audience validation
      describe "audience validation" do
        it "rejects tokens with wrong audience when configured" \_ -> do
          pending "Requires proper token generation with aud claim"

        it "accepts tokens with correct audience" \_ -> do
          pending "Requires proper token generation with aud claim"

        it "accepts tokens when no audience configured" \_ -> do
          pending "Requires proper token generation with aud claim"

      -- Claims extraction
      describe "claims extraction" do
        it "extracts sub claim as user ID" \_ -> do
          pending "Requires proper token generation"

        it "extracts email claim when present" \_ -> do
          pending "Requires proper token generation"

        it "extracts permissions from configured claim" \_ -> do
          pending "Requires proper token generation"

        it "extracts permissions from 'roles' claim when configured" \_ -> do
          pending "Requires proper token generation"

        it "extracts tenantId when configured" \_ -> do
          pending "Requires proper token generation"


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
