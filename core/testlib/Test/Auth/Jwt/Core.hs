{-# OPTIONS_GHC -Wno-deprecations #-}

-- | Test utilities for JWT validation.
-- Provides functions to generate test tokens for various scenarios.
module Test.Auth.Jwt.Core (
  -- * Test configuration
  testConfig,
  testConfigWithAudience,
  testKeys,
  TestKeys (..),
  -- * Token generation (IO-based for proper signing)
  signValidToken,
  signExpiredToken,
  signNotYetValidToken,
  signRecentlyExpiredToken,
  signTokenWithIssuer,
  signTokenWithAudience,
  signTokenWithSub,
  signTokenWithEmail,
  signTokenWithPermissions,
  signTokenWithTenantId,
  signTokenWithClaims,
  -- * Static tokens (pre-computed for specific tests)
  makeAlgNoneToken,
  makeHS256Token,
  makeMalformedToken,
  -- * Validation (re-export for tests)
  validateToken,
  validateTokenWithKeys,
  -- * Test manager (for middleware tests)
  createTestManager,
) where

import Array (Array)
import Array qualified
import AtomicVar qualified
import Auth.Claims (UserClaims)
import Auth.Config (AuthConfig (..), defaultAllowedAlgorithms)
import Auth.Error (AuthError)
import Auth.Jwt qualified
import Auth.Jwks (JwksManager (..), KeySnapshot (..))
import Basics
import Control.Lens ((&), (?~))
import Control.Lens qualified as Lens
import Crypto.JOSE qualified as Jose
import Crypto.JOSE.Header qualified as JoseHeader
import Crypto.JWT qualified as JWT
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as GhcLBS
import Data.Int qualified as GhcInt
import Data.Text.Encoding qualified as GhcTextEncoding
import Data.Time.Clock qualified as GhcTime
import Data.Time.Clock.POSIX qualified as GhcPosix
import Map qualified
import Maybe (Maybe (..))
import Prelude qualified
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


-- | Generate test keys
-- NOTE: RSA key gen is slow. Use 1024-bit for tests (NOT for production).
testKeys :: Task Text TestKeys
testKeys = Task.fromIO do
  -- Generate EC key for ES256 (fast)
  es256 <- Jose.genJWK (Jose.ECGenParam Jose.P_256)
  -- Generate RSA key for RS256 (1024-bit for speed - tests only!)
  rs256 <- Jose.genJWK (Jose.RSAGenParam 1024)
  -- Generate symmetric key for HS256 (fast)
  hs256 <- Jose.genJWK (Jose.OctGenParam 256)
  Prelude.pure
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


-- | Test configuration with audience validation
testConfigWithAudience :: Text -> AuthConfig
testConfigWithAudience aud =
  testConfig {audience = Just aud}


-- | Create a token with alg=none (MUST be rejected)
-- This is a pre-computed static token for security testing
makeAlgNoneToken :: Text
makeAlgNoneToken = do
  -- Manually construct an alg=none token
  -- Header: {"alg":"none","typ":"JWT"}
  -- Payload: {"sub":"user","iss":"https://auth.example.com"}
  "eyJhbGciOiJub25lIiwidHlwIjoiSldUIn0.eyJzdWIiOiJ1c2VyIiwiaXNzIjoiaHR0cHM6Ly9hdXRoLmV4YW1wbGUuY29tIn0."


-- | Create a token signed with HS256 (symmetric, should be rejected for public key validation)
-- Header: {"alg":"HS256","typ":"JWT"}
-- Payload: {"sub":"user","iss":"https://auth.example.com"}
makeHS256Token :: Text
makeHS256Token = do
  "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ1c2VyIiwiaXNzIjoiaHR0cHM6Ly9hdXRoLmV4YW1wbGUuY29tIn0.placeholder"


-- | Create a malformed token for testing format validation
makeMalformedToken :: Text
makeMalformedToken = "not.a.valid.jwt.token"


-- | Sign a valid token with ES256
signValidToken :: TestKeys -> Task Text Text
signValidToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now -- 1 hour from now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign an expired token
signExpiredToken :: TestKeys -> Task Text Text
signExpiredToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime (-3600) now -- 1 hour ago
  let iatTime = GhcTime.addUTCTime (-7200) now -- 2 hours ago
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate iatTime
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token not yet valid (nbf in future)
signNotYetValidToken :: TestKeys -> Task Text Text
signNotYetValidToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let nbfTime = GhcTime.addUTCTime 3600 now -- 1 hour from now
  let expTime = GhcTime.addUTCTime 7200 now -- 2 hours from now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimNbf ?~ JWT.NumericDate nbfTime
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token that expired recently (within clock skew tolerance)
signRecentlyExpiredToken :: TestKeys -> Task Text Text
signRecentlyExpiredToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime (-30) now -- 30 seconds ago (within 60s skew)
  let iatTime = GhcTime.addUTCTime (-3630) now -- 1 hour + 30 seconds ago
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate iatTime
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with specific issuer
signTokenWithIssuer :: TestKeys -> Text -> Task Text Text
signTokenWithIssuer keys iss = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ textToStringOrUri iss
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with specific audience
signTokenWithAudience :: TestKeys -> Text -> Task Text Text
signTokenWithAudience keys aud = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimAud ?~ JWT.Audience [textToStringOrUri aud]
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with specific sub claim
signTokenWithSub :: TestKeys -> Text -> Task Text Text
signTokenWithSub keys sub = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ textToStringOrUri sub
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with email claim
signTokenWithEmail :: TestKeys -> Text -> Task Text Text
signTokenWithEmail keys email = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
          & JWT.addClaim "email" (Aeson.String email)
  signWithES256 keys.es256Key claims


-- | Sign a token with permissions claim
signTokenWithPermissions :: TestKeys -> Array Text -> Task Text Text
signTokenWithPermissions keys perms = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let permsList = perms |> Array.toLinkedList |> Prelude.map Aeson.String
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
          & JWT.addClaim "permissions" (Aeson.toJSON permsList)
  signWithES256 keys.es256Key claims


-- | Sign a token with tenant ID
signTokenWithTenantId :: TestKeys -> Text -> Task Text Text
signTokenWithTenantId keys tenantId = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ "test-user-123"
          & JWT.claimIss ?~ "https://auth.example.com"
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
          & JWT.addClaim "tenant_id" (Aeson.String tenantId)
  signWithES256 keys.es256Key claims


-- | Sign a token with arbitrary custom claims
signTokenWithClaims ::
  TestKeys ->
  Text -> -- sub
  Text -> -- issuer
  Maybe Text -> -- email
  Array Text -> -- permissions
  Maybe Text -> -- tenant ID
  Task Text Text
signTokenWithClaims keys sub iss emailMaybe perms tenantMaybe = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let permsList = perms |> Array.toLinkedList |> Prelude.map Aeson.String
  let baseClaims =
        JWT.emptyClaimsSet
          & JWT.claimSub ?~ textToStringOrUri sub
          & JWT.claimIss ?~ textToStringOrUri iss
          & JWT.claimIat ?~ JWT.NumericDate now
          & JWT.claimExp ?~ JWT.NumericDate expTime
          & JWT.addClaim "permissions" (Aeson.toJSON permsList)
  let withEmail = case emailMaybe of
        Just email -> baseClaims & JWT.addClaim "email" (Aeson.String email)
        Nothing -> baseClaims
  let finalClaims = case tenantMaybe of
        Just tid -> withEmail & JWT.addClaim "tenant_id" (Aeson.String tid)
        Nothing -> withEmail
  signWithES256 keys.es256Key finalClaims


-- | Convert Text to StringOrURI (using the string prism)
-- Note: This will fail for text containing ":" that isn't a valid URI
textToStringOrUri :: Text -> JWT.StringOrURI
textToStringOrUri txt = JWT.string Lens.# txt


-- | Internal: Sign claims with ES256
signWithES256 :: Jose.JWK -> JWT.ClaimsSet -> Task Text Text
signWithES256 key claims = Task.fromIO do
  let header = Jose.newJWSHeader (JoseHeader.RequiredProtection, Jose.ES256)
  result <- JWT.runJOSE @JWT.JWTError (JWT.signClaims key header claims)
  case result of
    Prelude.Left _err -> Prelude.error "Failed to sign token"
    Prelude.Right jwt -> do
      let compactBytes = Jose.encodeCompact jwt
      Prelude.pure (compactBytes |> GhcLBS.toStrict |> GhcTextEncoding.decodeUtf8)


-- | Validate a JWT token against the auth configuration.
-- Wrapper that uses empty key set for basic format validation tests.
validateToken ::
  forall err.
  AuthConfig ->
  Text ->
  Task err (Result AuthError UserClaims)
validateToken config token = do
  Auth.Jwt.validateToken config Array.empty token


-- | Validate a JWT token with a specific key set
validateTokenWithKeys ::
  forall err.
  AuthConfig ->
  Array Jose.JWK ->
  Text ->
  Task err (Result AuthError UserClaims)
validateTokenWithKeys config keys token = do
  Auth.Jwt.validateToken config keys token


-- | Create a test JwksManager with pre-populated keys.
-- This manager has no background refresh - it's purely for testing.
-- Keys are extracted from TestKeys and stored directly in the snapshot.
createTestManager :: TestKeys -> Task Text JwksManager
createTestManager keys = do
  -- Get current time for snapshot timestamp
  now <- getCurrentSeconds

  -- Build key map with ES256 key (used for signing test tokens)
  -- We use a placeholder kid since test tokens don't have kid in header
  let keyMap = Map.fromArray [("test-key-es256", keys.es256Key)]

  let snapshot =
        KeySnapshot
          { keysByKid = keyMap,
            fetchedAt = now,
            isStale = False
          }

  -- Create AtomicVars for the manager
  snapshotVar <- AtomicVar.containing snapshot
  refreshTaskVar <- AtomicVar.containing Nothing
  runningVar <- AtomicVar.containing False -- Not running background refresh

  Task.yield
    JwksManager
      { config = testConfig,
        keySnapshot = snapshotVar,
        refreshTask = refreshTaskVar,
        isRunning = runningVar
      }


-- | Get current Unix timestamp in seconds.
getCurrentSeconds :: Task err GhcInt.Int64
getCurrentSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: GhcInt.Int64 = Prelude.floor (Prelude.realToFrac posixTime :: Prelude.Double)
  Task.yield seconds
