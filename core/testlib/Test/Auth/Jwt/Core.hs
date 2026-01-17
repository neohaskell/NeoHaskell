-- | Test utilities for JWT validation.
-- Provides functions to generate test tokens for various scenarios.
module Test.Auth.Jwt.Core (
  -- * Test configuration
  testConfig,
  testKeys,
  TestKeys (..),
  -- * Token generation
  makeValidToken,
  makeAlgNoneToken,
  makeHS256Token,
  makeES256Token,
  makeRS256Token,
  makeExpiredToken,
  makeNotYetValidToken,
  makeRecentlyExpiredToken,
  makeTokenWithIssuer,
  makeTokenWithAudience,
  makeTokenWithSub,
  makeTokenWithEmail,
  makeTokenWithPermissions,
  makeTokenWithRoles,
  makeTokenWithTenantId,
  -- * Validation (re-export for tests)
  validateToken,
) where

import Array qualified
import Auth.Claims (UserClaims)
import Auth.Config (AuthConfig (..), defaultAllowedAlgorithms)
import Auth.Error (AuthError (..))
import Auth.Jwt qualified
import Basics
import Crypto.JOSE qualified as Jose
import Maybe (Maybe (..))
import Result (Result)
import Task (Task)
import Task qualified
import Text (Text)


-- | Test keys for signing tokens
data TestKeys = TestKeys
  { es256Key :: Jose.JWK,
    rs256Key :: Jose.JWK,
    hs256Key :: Jose.JWK
  }


-- | Generate test keys at module load time
testKeys :: Task Text TestKeys
testKeys = Task.fromIO do
  -- Generate EC key for ES256
  es256 <- Jose.genJWK (Jose.ECGenParam Jose.P_256)
  -- Generate RSA key for RS256
  rs256 <- Jose.genJWK (Jose.RSAGenParam 2048)
  -- Generate symmetric key for HS256
  hs256 <- Jose.genJWK (Jose.OctGenParam 256)
  pure
    TestKeys
      { es256Key = es256,
        rs256Key = rs256,
        hs256Key = hs256
      }


-- | Default test configuration
testConfig :: AuthConfig
testConfig =
  AuthConfig
    { issuer = "https://auth.example.com",
      jwksUri = "https://auth.example.com/.well-known/jwks.json",
      audience = Nothing,
      permissionsClaim = "permissions",
      tenantIdClaim = Nothing,
      clockSkewSeconds = 60,
      refreshIntervalSeconds = 900,
      missingKidCooldownSeconds = 60,
      maxStaleSeconds = 86400,
      allowedAlgorithms = defaultAllowedAlgorithms,
      supportedCritHeaders = Array.empty
    }


-- | Create a valid ES256 token with default claims
makeValidToken :: Text
makeValidToken = makeES256Token


-- | Create a token with alg=none (MUST be rejected)
makeAlgNoneToken :: Text
makeAlgNoneToken = do
  -- Manually construct an alg=none token
  -- Header: {"alg":"none","typ":"JWT"}
  -- Payload: {"sub":"user","iss":"https://auth.example.com"}
  "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJzdWIiOiJ1c2VyIiwiaXNzIjoiaHR0cHM6Ly9hdXRoLmV4YW1wbGUuY29tIn0."


-- | Create a token signed with HS256 (symmetric, should be rejected for public key validation)
makeHS256Token :: Text
makeHS256Token = do
  -- This needs to be generated dynamically, for now placeholder
  -- In real implementation, we'd sign with a symmetric key
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ1c2VyIiwiaXNzIjoiaHR0cHM6Ly9hdXRoLmV4YW1wbGUuY29tIn0.placeholder"


-- | Create a valid ES256 token
makeES256Token :: Text
makeES256Token = do
  -- Placeholder - will be implemented with proper signing
  "placeholder-es256-token"


-- | Create a valid RS256 token
makeRS256Token :: Text
makeRS256Token = do
  -- Placeholder - will be implemented with proper signing
  "placeholder-rs256-token"


-- | Create an expired token
makeExpiredToken :: Text
makeExpiredToken = do
  -- Token with exp in the past
  "placeholder-expired-token"


-- | Create a token not yet valid (nbf in future)
makeNotYetValidToken :: Text
makeNotYetValidToken = do
  -- Token with nbf in the future
  "placeholder-not-yet-valid-token"


-- | Create a token that expired recently (within clock skew tolerance)
makeRecentlyExpiredToken :: Text
makeRecentlyExpiredToken = do
  -- Token expired 30 seconds ago
  "placeholder-recently-expired-token"


-- | Create a token with specific issuer
makeTokenWithIssuer :: Text -> Text
makeTokenWithIssuer _issuer = do
  "placeholder-issuer-token"


-- | Create a token with specific audience
makeTokenWithAudience :: Text -> Text
makeTokenWithAudience _audience = do
  "placeholder-audience-token"


-- | Create a token with specific sub claim
makeTokenWithSub :: Text -> Text
makeTokenWithSub _sub = do
  "placeholder-sub-token"


-- | Create a token with email claim
makeTokenWithEmail :: Text -> Text
makeTokenWithEmail _email = do
  "placeholder-email-token"


-- | Create a token with permissions claim
makeTokenWithPermissions :: [Text] -> Text
makeTokenWithPermissions _permissions = do
  "placeholder-permissions-token"


-- | Create a token with roles claim
makeTokenWithRoles :: [Text] -> Text
makeTokenWithRoles _roles = do
  "placeholder-roles-token"


-- | Create a token with tenant ID
makeTokenWithTenantId :: Text -> Text
makeTokenWithTenantId _tenantId = do
  "placeholder-tenant-token"


-- | Validate a JWT token against the auth configuration.
-- Wrapper that uses empty key set for basic tests.
validateToken ::
  forall err.
  AuthConfig ->
  Text ->
  Task err (Result AuthError UserClaims)
validateToken config token = do
  -- Use the real implementation with empty keys for format validation tests
  Auth.Jwt.validateToken config Array.empty token
