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
  signValidTokenRS256,
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
  validateTokenFormat,
  validateTokenWithKeys,
  -- * Test manager (for middleware tests)
  createTestManager,
) where

import Array (Array)
import Array qualified
import AtomicVar qualified
import Auth.Claims (UserClaims)
import Auth.Config (AuthConfig (..), defaultAllowedAlgorithms, mkAuthConfig)
import Auth.Error (AuthError)
import Auth.Jwt qualified
import Auth.Jwks (JwksManager (..), KeySnapshot (..), RefreshState (..))
import Basics
import Control.Lens ((&), (?~))
import Control.Lens qualified as Lens
import Crypto.JOSE qualified as Jose
import Crypto.JOSE.Header qualified as JoseHeader
import Crypto.JWT qualified as JWT
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64.URL qualified as GhcBase64Url
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


-- | Pre-computed test keys for signing tokens.
-- Keys are parsed from JSON rather than generated at runtime,
-- eliminating expensive RSA key generation (~300ms per call).
-- These are TEST-ONLY keys (1024-bit RSA) — NOT suitable for production.
testKeys :: Task Text TestKeys
testKeys = do
  es256 <- parseTestKey es256KeyJson "ES256"
  rs256 <- parseTestKey rs256KeyJson "RS256"
  hs256 <- parseTestKey hs256KeyJson "HS256"
  Task.yield
    TestKeys
      { es256Key = es256,
        rs256Key = rs256,
        hs256Key = hs256
      }


-- | Default test configuration
testConfig :: AuthConfig
testConfig =
  mkAuthConfig
    "https://auth.example.com"
    "https://auth.example.com/.well-known/jwks.json"
    Nothing
    "permissions"
    Nothing
    60
    900
    60
    86400
    defaultAllowedAlgorithms
    Array.empty


-- | Test configuration with audience validation
testConfigWithAudience :: Text -> AuthConfig
testConfigWithAudience aud =
  mkAuthConfig
    "https://auth.example.com"
    "https://auth.example.com/.well-known/jwks.json"
    (Just aud)
    "permissions"
    Nothing
    60
    900
    60
    86400
    defaultAllowedAlgorithms
    Array.empty


-- | Create a token with alg=none (MUST be rejected)
-- Constructed programmatically at runtime to avoid triggering secret scanners.
-- Header: {"alg":"none","typ":"JWT"}
-- Payload: {"sub":"user","iss":"https://auth.example.com"}
makeAlgNoneToken :: Text
makeAlgNoneToken = do
  let header = base64UrlEncodeJson (Aeson.object [("alg", "none"), ("typ", "JWT")])
  let payload = base64UrlEncodeJson (Aeson.object [("sub", "user"), ("iss", "https://auth.example.com")])
  -- alg=none tokens have empty signature
  [fmt|#{header}.#{payload}.|]


-- | Create a token signed with HS256 (symmetric, should be rejected for public key validation)
-- Constructed programmatically at runtime to avoid triggering secret scanners.
-- Header: {"alg":"HS256","typ":"JWT"}
-- Payload: {"sub":"user","iss":"https://auth.example.com"}
-- Note: Signature is a placeholder since we only need to test algorithm rejection.
makeHS256Token :: Text
makeHS256Token = do
  let header = base64UrlEncodeJson (Aeson.object [("alg", "HS256"), ("typ", "JWT")])
  let payload = base64UrlEncodeJson (Aeson.object [("sub", "user"), ("iss", "https://auth.example.com")])
  -- Placeholder signature - we're testing algorithm rejection, not signature validation
  [fmt|#{header}.#{payload}.placeholder|]


-- | Base64url encode a JSON value (no padding, as per JWT spec)
base64UrlEncodeJson :: Aeson.Value -> Text
base64UrlEncodeJson value = do
  let jsonBytes = Aeson.encode value |> GhcLBS.toStrict
  let encoded = GhcBase64Url.encodeUnpadded jsonBytes
  GhcTextEncoding.decodeUtf8 encoded


-- | Create a malformed token for testing format validation
makeMalformedToken :: Text
makeMalformedToken = "not.a.valid.jwt.token"


-- | Sign a valid token with ES256
signValidToken :: TestKeys -> Task Text Text
signValidToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now -- 1 hour from now
  let claims = buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
  signWithES256 keys.es256Key claims


-- | Sign a valid token with RS256
signValidTokenRS256 :: TestKeys -> Task Text Text
signValidTokenRS256 keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now -- 1 hour from now
  let claims = buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
  signWithRS256 keys.rs256Key claims


-- | Sign an expired token
signExpiredToken :: TestKeys -> Task Text Text
signExpiredToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime (-3600) now -- 1 hour ago
  let iatTime = GhcTime.addUTCTime (-7200) now -- 2 hours ago
  let claims = buildBaseClaims "test-user-123" "https://auth.example.com" iatTime expTime
  signWithES256 keys.es256Key claims


-- | Sign a token not yet valid (nbf in future)
signNotYetValidToken :: TestKeys -> Task Text Text
signNotYetValidToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let nbfTime = GhcTime.addUTCTime 3600 now -- 1 hour from now
  let expTime = GhcTime.addUTCTime 7200 now -- 2 hours from now
  let claims =
        buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
          & JWT.claimNbf ?~ JWT.NumericDate nbfTime
  signWithES256 keys.es256Key claims


-- | Sign a token that expired recently (within clock skew tolerance)
signRecentlyExpiredToken :: TestKeys -> Task Text Text
signRecentlyExpiredToken keys = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime (-30) now -- 30 seconds ago (within 60s skew)
  let iatTime = GhcTime.addUTCTime (-3630) now -- 1 hour + 30 seconds ago
  let claims = buildBaseClaims "test-user-123" "https://auth.example.com" iatTime expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with specific issuer
signTokenWithIssuer :: TestKeys -> Text -> Task Text Text
signTokenWithIssuer keys iss = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims = buildBaseClaims "test-user-123" iss now expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with specific audience
signTokenWithAudience :: TestKeys -> Text -> Task Text Text
signTokenWithAudience keys aud = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
          & JWT.claimAud ?~ JWT.Audience [textToStringOrUri aud]
  signWithES256 keys.es256Key claims


-- | Sign a token with specific sub claim
signTokenWithSub :: TestKeys -> Text -> Task Text Text
signTokenWithSub keys sub = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims = buildBaseClaims sub "https://auth.example.com" now expTime
  signWithES256 keys.es256Key claims


-- | Sign a token with email claim
signTokenWithEmail :: TestKeys -> Text -> Task Text Text
signTokenWithEmail keys email = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
          & JWT.addClaim "email" (Aeson.String email)
  signWithES256 keys.es256Key claims


-- | Sign a token with permissions claim
signTokenWithPermissions :: TestKeys -> Array Text -> Task Text Text
signTokenWithPermissions keys perms = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let permsList = perms |> Array.toLinkedList |> Prelude.map Aeson.String
  let claims =
        buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
          & JWT.addClaim "permissions" (Aeson.toJSON permsList)
  signWithES256 keys.es256Key claims


-- | Sign a token with tenant ID
signTokenWithTenantId :: TestKeys -> Text -> Task Text Text
signTokenWithTenantId keys tenantId = do
  now <- Task.fromIO GhcTime.getCurrentTime
  let expTime = GhcTime.addUTCTime 3600 now
  let claims =
        buildBaseClaims "test-user-123" "https://auth.example.com" now expTime
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
        buildBaseClaims sub iss now expTime
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


-- | Build base claims for a valid token.
-- This is the shared foundation used by all token signing helpers.
-- Individual helpers extend this with additional claims as needed.
buildBaseClaims ::
  -- | Subject claim (sub)
  Text ->
  -- | Issuer claim (iss)
  Text ->
  -- | Issued at time (iat)
  GhcTime.UTCTime ->
  -- | Expiration time (exp)
  GhcTime.UTCTime ->
  JWT.ClaimsSet
buildBaseClaims sub iss iatTime expTime =
  JWT.emptyClaimsSet
    & JWT.claimSub ?~ textToStringOrUri sub
    & JWT.claimIss ?~ textToStringOrUri iss
    & JWT.claimIat ?~ JWT.NumericDate iatTime
    & JWT.claimExp ?~ JWT.NumericDate expTime


-- | Internal: Sign claims with ES256
-- Returns a Task error instead of crashing if signing fails.
signWithES256 :: Jose.JWK -> JWT.ClaimsSet -> Task Text Text
signWithES256 key claims = do
  let header = Jose.newJWSHeader (JoseHeader.RequiredProtection, Jose.ES256)
  result <- Task.fromIO (JWT.runJOSE @JWT.JWTError (JWT.signClaims key header claims))
  case result of
    Prelude.Left err -> Task.throw [fmt|Failed to sign token: #{show err}|]
    Prelude.Right jwt -> do
      let compactBytes = Jose.encodeCompact jwt
      Task.yield (compactBytes |> GhcLBS.toStrict |> GhcTextEncoding.decodeUtf8)


-- | Internal: Sign claims with RS256
-- Returns a Task error instead of crashing if signing fails.
signWithRS256 :: Jose.JWK -> JWT.ClaimsSet -> Task Text Text
signWithRS256 key claims = do
  let header = Jose.newJWSHeader (JoseHeader.RequiredProtection, Jose.RS256)
  result <- Task.fromIO (JWT.runJOSE @JWT.JWTError (JWT.signClaims key header claims))
  case result of
    Prelude.Left err -> Task.throw [fmt|Failed to sign token: #{show err}|]
    Prelude.Right jwt -> do
      let compactBytes = Jose.encodeCompact jwt
      Task.yield (compactBytes |> GhcLBS.toStrict |> GhcTextEncoding.decodeUtf8)


-- | Validate a JWT token format against the auth configuration.
-- Wrapper that uses empty key set - only validates token format, NOT signatures.
-- For signature validation, use validateTokenWithKeys instead.
validateTokenFormat ::
  forall err.
  AuthConfig ->
  Text ->
  Task err (Result AuthError UserClaims)
validateTokenFormat config token = do
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

  -- Build the keys array from the map
  let keysArray = keyMap |> Map.values
  let jwkSet = Jose.JWKSet (keysArray |> Array.toLinkedList)

  -- Build per-kid JWKSets (avoids per-request allocation)
  let jwkSetsByKid = keyMap |> Map.mapValues (\key -> Jose.JWKSet [key])

  let snapshot =
        KeySnapshot
          { keysByKid = keyMap,
            jwkSetsByKid = jwkSetsByKid,
            cachedJwkSet = jwkSet,
            allKeysArray = keysArray,
            fetchedAt = now,
            snapshotVersion = 1,
            isStale = False
          }

  -- Create AtomicVars for the manager
  snapshotVar <- AtomicVar.containing snapshot
  -- Initialize refresh state (not refreshing, last attempt = now)
  let initialRefreshState = RefreshState {lastAttemptAt = now, refreshInProgress = False}
  refreshStateVar <- AtomicVar.containing initialRefreshState
  refreshTaskVar <- AtomicVar.containing Nothing
  runningVar <- AtomicVar.containing False -- Not running background refresh

  Task.yield
    JwksManager
      { config = testConfig,
        keySnapshot = snapshotVar,
        refreshState = refreshStateVar,
        refreshTask = refreshTaskVar,
        isRunning = runningVar
      }


-- | Get current Unix timestamp in seconds.
getCurrentSeconds :: Task err GhcInt.Int64
getCurrentSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: GhcInt.Int64 = Prelude.floor (Prelude.realToFrac posixTime :: Prelude.Double)
  Task.yield seconds


-- ---------------------------------------------------------------------------
-- Pre-computed test key material (JSON)
-- Generated once offline; parsed at test time for near-zero startup cost.
-- These are TEST-ONLY keys (1024-bit RSA) — NOT suitable for production.
-- ---------------------------------------------------------------------------


-- | Parse a JWK from a pre-serialized JSON blob.
parseTestKey :: GhcLBS.ByteString -> Text -> Task Text Jose.JWK
parseTestKey json label =
  case Aeson.eitherDecode json of
    Prelude.Right key -> Task.yield key
    Prelude.Left err -> Task.throw [fmt|Failed to parse #{label} test key: #{err}|]


-- | Pre-computed EC P-256 key for ES256 signing.
es256KeyJson :: GhcLBS.ByteString
es256KeyJson = "{\"crv\":\"P-256\",\"d\":\"By7lfJqDiQXl0Rj4OEYuH9SFCByJseTfQpqepdb08BU\",\"kty\":\"EC\",\"x\":\"s10hmNsAYBPI0BsaxdiyKkwsZQDXSEf_TPJ8n_2bJ2o\",\"y\":\"8RGhi-Yn2M4G4v6563vzX-gqJEczy2N-dJsikSUed90\"}"


-- | Pre-computed RSA 1024-bit key for RS256 signing (test-only size).
rs256KeyJson :: GhcLBS.ByteString
rs256KeyJson = "{\"d\":\"c8Qw7Xs6L02nbVnRIkWhj0vVVx3IQgKmJ-dbGXLFeveq6dUBjC9vVzyOmwfZxy2JpZnVuZnSd7xXgwe3HbDhKJrOdps6tSiMuAeb6I43pqoWEm_5TDmT6yAHfmHKLuRvHsZIH2st3VQl_S-Hdn6Lxt44mr9WnqgxJ2bPQtxV09vvehP6-CC2tS2gWVR6wITVIAxqwYOyGaeg5biyWcanMsWrdrfDdUf-0A-9UO3AV_WPoy0vtATWfUSHj4vimhlff8LltA3s_jMFfUt1VAVYKaqG1F4ta8u5yWfFSVzK0VVRcTdGPVZ5U9GHfVpF0XJIOfQM1JnAtdIbVHbwO9hOKM_QC0gJz2OMPu-9ebZwNewacNOl8PTUpw-q2BHkbPLFGcb9V1h1DKD2xLFixbEvdSL7LkeY5YF1-Zid_m-E--WQt2rmHZie7JPfuZ3TsWAIpMO1bSRKvU0FU4OZz0xpRWOnZeNQ7vcew0ZnYVM3D8xeddZUKqB7NKXqDGDy_PgQhA99YXyctPNfqGIG_yCl9q6DkP-XP4E54B9Sh24xw8Kloz0JlGaO7YvtRc5FpVzz0Y-uMeqY10i4T3KKqFVgcCljgkxLOZiaN75fM2fsIruuvq4P9_pUYvBTM7K1o4sY_8Fl0D1BR0TDhP6tr1Qr3YuDSO05wnyWRR6TWezZVQ9piZh-4eVBYkbKKnmHVpAsEK_wSBPqvQR3NILEINArCCNywbOO7tpxdzueZ_qA7hwvwETqadKIn3IXDd4NT9dCDyd7-Z2Yh7wACUbbvtDOeC4xrrbSD20HhcsIn39AEil5rm2VeYW3J9ZnMhrW0afEimif3ePYpidUW2AqT-k5PopRvqVTPo1k-lo8CSggeIQzfGYyQTjF6ob8Bpvf0QZyN0Meq7IMzuuDDiYEVAKyBvymI5M-9H-4VJ4uOrE-vNtr_Uv7iocFYE8XvHn9Y1yDSRxqGNljG910Hh2pf_YehrUYPo45IW3avPcg0Cmk8qO87QZm3AF0VSMr5prGj1YHjM_dfY7N9zyovGpMr_FzqFmiYHoX2XGFYfkv-mIOkKbKG0fe9_g_m4cCK3ZxvAW9kH7gNDIO5vKTAN8tT0EdTZrQZSj79-mV00rOGFCvtUbYeHXFLLuxqD8javdtNGBPClKfMQyFnfXRhb2F1bMYHBRUS1RUnO4PrZEuyoYXtzZtOD5on4coLbyF8BIgMOOQOqdRc2OEZxtOsxjK6Xnf99xovlkVNu-aI3zd6TG5W8LkpTVGE1E6lduMDDz1ylYmY-kE4gB70P5UuaAfrbyGNsR3-E2WEtgsjiLz5jUFxS-8e25Rj0l4rRn3EewY4U3vbgEQkXyFHw_xtrUfhxQT-Q\",\"dp\":\"rzDjDHz66IApUqTZHZQOHO5iOLgQ7RJArDWzWEEfjRXsGUDxtXNMWui1c4I_3ByJfj-yuCIknOtXFhKJHJ4jDA4WCqVoleVlR-_xVWl7pm2mCHDj1uHnJUtTczdXFud63x-iLcWi94Jk04gsIGLvUcd0r6vVSWqiPiEkCWemqkREhPWfPBaCHIu7hsSrW0fdJuIri1z47c7VJBLRoGjMgPqwaXppKLay7sr-VVxmn8iwdbsFud-SXE_KzirT-ECFXZuY_w1WAVe1GtugpEFZso-275sbPbSfeHNcM4VDFXC-29czs4ldHTIoOpHD03GitUTmwJ8H0KXjzjVAupJ3MR2LTUxGHo0nPRWET6WxDaU-7RcqoxdziFlB3rXKNNAkiIltEddpL_YoF3sktIStqGBzB1uL6PyEnf6y_GRnaH1ksVImKemao59KPPsqosYruu4g3UzFdrrFtC8C9Q2zuWlgbKEZJ_kQ3XaOoH59Ll2cdATqtKZTFkRTGm9gOi0LASgqmGJG-F1dKF1LQWPLz2r9SWqA9Q75bo95G2jN-5sKiqn5Ccgtkty6uI42JLTkZFTpWBA-L3TMjH1M5vMivOPmw1rzC9JSNOz1BfiPDbGnMuyIhU1-1CS3o1GrBq5AIfZSeJ9mzy0b8XY6gRSr2bzIGNxjbt4nGRvmUrXbzrU\",\"dq\":\"cUQzs3ABrphAKLm_nZib9-HVNioFWy6Z7tzG6tAmsxdd6s6w0raRhA1lCmw4Uo4Zy1dUkDATfZJufBZaLefU2Hyd8grJHaiGUltxnhQfkVOf4LnTeJrBXdCxnm0H1Akfckh0-MR0xxvYsPFbE5U90HLbC5E5G8_kIOc_t_SSvQJ13rSBmJ-ftrUZ-mpn02hCoREPxyj7EJF74lNmxs_HtDMDp9DSQ3XaJ2HOp6vhxkLq6nM4rt6p8RQn0nn7URcXvJOt76GB5vuYHhmz-Co1F1ZenBHICOkXdrHcEYiSl8Z0f9fkeZUXyzhZdXMEG9fq9TMRHkM_4uRrZRvHwyhi7vDByVl1su0jOZcKbKCs2SloI9oG8MmofUHmj5bTZk6gGiXRrDnUPwMNtNQAlKoa0OlTAc8E1wzYuMZmsT0Mngn2DWmcDqUXShqCrfsvr7txMJFcJ840bT13NcaDIDGYBsRXMDQQRIRhzueQlNkNfKQqiiyrpCcidqPa3kOMlDq6puVr5aX62axHc7P6SmwFbxY1NuknbdQZC_aKRSNKRpkjRxgImjvG01jdmCC8SZdyQfWaS3RbTsFhnrdTSx9nhZ1JjujjXUZCSK1xIpSdT99d4VGMjvdCjxW3XCtbvUet6wCDivQiyr0Qe7g8cOQijg2D5m3M7tyN9rcqeg-HBy0\",\"e\":\"AQAB\",\"kty\":\"RSA\",\"n\":\"p6_FtWS00vSAUdh2zDF5LFD7aqq5NeX8LaKltQp9W-km6GZROQ5XLv1kBuTljeUsjXL8RKQs44QhBFumhQtN0vM8WfkdquMMAQr-SVoj3a00KjLAYyVZjfGtXyXfbLNwxzHy8H-hiRb8JKCxeLoBYJPsBWinbDRByjbL8CezdWMNZIv2224VLum-fBvtw5SkOac9k5f6x5IFc2IMoasfeQDGyDCgZrmen-xXHZp9Z3UgLoOcrGqLanCibsVVCQZFxUV2FXy_7EPCa1ELVIeoxPY4UHaxr7IV-JjCx8DhIsfwPVZyYD9_sAiS4KWvxzGDFQhchlCcy14SeKHB2yVzbPI9HQ4CivtF_7eOZNZ_XZHLPOvMoMXtyqswwRSjSSYYnaJ9US_LyxjUdYfUoZg51riaOU85uW806is20NkzARs_RMKxKEueGWuuQqwdcK3ffkA1gfV8klypCz9zmvtYvA6rkrlLqsoguoUJXNMA4fsLg-8a4CpmlmMLBJgnUr5-UrW8B4CpBvFjfBXtXmoYYe7NAyKuBraSUHneFBFSDDCZewQm6cJEOcvnrhdMAML2LMCq7a2OABC-4c7ArIyS5H69PQ4l7mE3VRV9NNvHruJ6lCrKQ_GnNY7LOiiwbopdqE1MXXddqUXr4OSaDAoifXV1-qxH_eiRiRsWgUnRGvXB2Ika9BDYxj7UzMpmQYNK9GO08MHsQqE4j7lVfeOpZ2rGkJCypyKn-GAlH8x5DVbyuw7Jiqdwj2bfxsnGmx48-wKdygcqeYzxszkheRJadTq--ttNg5-bCMk3o2BXSJhGGJgDlwJ8vuD4gn3rsiTdaiU2YhZ-O9pdMbaGcCrlvrv6CFdkyZDSu-soVD8dSYx0Dgf7WkhqTOozEyz7fxuApImJtHlcWaKRgzfjFst5o38N-5PAD3nTLkoGkkmeVzLg_o2PTzqgDhpiRUEtePhgiheS1IKEDlHIA7MKJlDrKo2DfWYHg422BWywWfm41eNjty8UfE4a2oQLmd2c3nUEhMJQogMgXGnjGJmHL9w3zsSizthpMnJ5QyybLwE5K6b5W6gzAtXjivGLHLNDvnj-gintqi51G3m3faCDiU_6si10XsVMhdwtVQ8oi7sjfIisqI6QiCrq4V3lIf1AOucJa1sdQGEn7-BxG4l9DFN_Mp2X5g3Bcl9sOE9AgsaTtHYucjlN8HAYG7ZyxqO8PiDatKTcfGYj0QXzZRBPLggezEV9-I2ey1IQ_zEiZ0sFGdpkNVR8q6fM69SV_NFe_9B7axNs_yqCka80Y2cIo0OroWVJNQpLsOhR0uAlgxCDbVg7vz_EfHFMQmn1CLMarIHjoVtU8akt3s9TLGjtZUPGKw\",\"p\":\"0hhcNwhvEM7fmfupPJ4SAJLmbGY44kEv_ofkvucLievox7F3FcOR3qlAIr0IcDjqAp6XcnpLpjonFY-Kn2hyiRTXbnYSiaqPsmAL3gqoSvUGycXGEjY-9ujiwfCGJ3bip6FhYc46AhOvgvezxwOZ2FX2L8sIZ7HEq6KQnuBDY9Wg08jDEXBgQF9zVB6SPRdAO1uplq_TwSI5arAdZVv266bVYIbZa2ZGE6yfSpgCkWR35UI_3zI94YOuIAo0Dv4VD9VPD_YhUaTOLwHR5SrgX_s_ZC-zcPlZUsEpScEGdCn5V2qoayD3q0OxMT9b4bBr4wxfKFFmHVeKTv52lMPC_OXNR-PfNBLSM4qwfLDBsGwQOBeIieHaltCImR-XawIg3B8mUeoU5iciUjoigQZ9IFi4dCFGW60vxQdwVVQzrtTunpAZsJQzFbsU9GwFewMzq3uPfuxXzYWFy_pbHNheW_XySLNFxTh-XE8S4028nRd8p-pe4tDqWyDbwCwL1K8qh-3hIdIxSIff6Qn7Ez5xzNS24ekt3QHEsVtvHUgyzMyWxaOTGFAx0bgZ65u6ZyMzCIlWP_M2b7zmbJ9CPmzPCJ4gWOZkXPIZ9SgCcxqnkq_sdY85Y2iZeE7w8I-_lMnbOmcueCooRSkjqlKHwt-hVFF2LpzKI7ZVE0pfSM0_G1U\",\"q\":\"zFNL8eBwgkomFhVIa2P5EdcjTZOmPF_P8_vQFK6YXlp0yku0hxA5Ar2PHQ38TIrRoUZIAxOoUyJfkYtwOYOFgW05XQFOAhGzYeN5u3a3nUIs3-wS5twQA4J0iw-BDrSrSFbAYOntVncVsdq8906M4so0DG6UJz33dxiKVOYEb7W4yDG47DU3uegwBpHvYgu9YKI-ty_G5-s65LmVudWup6RIEERIBHRHZ22zUO0OGBNmeugFiglkRSe8D9pwy0uAkYVEceyXMYwtVcwRUcTf3VsDMCid6tcXLnlcSoAUEijCTYOzoEqInr62NRscpKKRnfus-SOoC95VcVgvrvAai-nZ0p14CHnOoyD2tdmmLxYtcd92-qm3o4KZUIX2FiPtEH0BA9gJFPOPl9YY5Jujy-KLg9ORuPRtkhiouCNGGz70SMYoj8HOlYowD4suNBTrqfANQXNaZyVrkRmV0F7gOQXj_KP9aTWHE6QqiSMCa_4T3Ads_LxizgAJ77m5VgFCr4PjYnP9PD3ZIZudga-yZb_LU_9BZgtZ3ykKYmFg9ZTD5RPTpq5WnBBpMoo6OHumt2fhreJskQWuoKObKuA9JIyHqTRCR9uUmoOdESneoZE9t94paLIlj3R3zdkTC1AHWObj2jp4X_HCs2jPoGWbXOp3K5oL5jEpjiLf4_9GW38\",\"qi\":\"M83tkvd6RLkkxZk5-PbAtM_fQRoRmqUma-PNP4LpYhsZXMWeuG5A2Dwg626CG4libnx1Eo0tcP0g9nkHmEjOb8s_BesGU8OJJastbn0NpYLQsX3t87jk3S7D0xbLe3rzHQFjpKFn9_SACiz3snxR5uXQpP8qoejrPqzZ3O20CCjHUKfKxBrkMJmCDihXIsh_7b8r2pLQCee6oEaA_G1fvuhMM8JAws7tOg1EbRL3N67imI9HkxDqBAkjcdVHGxmk3RCXKB8wUmCmR0tp9XTTx1-HhDRx1CdI-17rtg65dftv80JP-1qwM0ng7y_Dxs_N4gIBxbJO6OFYJ0GxMnEwr4bnhMIaETcyb_NhSM2Z5Wffk-_pxYa3d3y6oi31smnDnEG_DkPCijp_gueH9QRr4-qbH0hGMGT1vZkjaplWNILc1xYZHF70L3jcCba1A5NurXV5su-kTWi-rwZP2FW0YE82PUxr1PdZoPdKtIDiR02IFFDH6Fkip1G1dVFBoqwQrO-Seh3PVMxirohckIIZceYItZhexRJvnS9RhsPJsz4hrQy_rjr6Dtn1knorJotweIHCIINVhpkBhLYJ6YUXW4CPaLdVRFhbK51d-08q2W2XaBHd6w5h7ggY86sa51EnLGEV4mq07-Bddl7ulEFWt4GbwvH1kw6jtLmK0Ljw2iQ\"}"


-- | Pre-computed symmetric key for HS256 signing.
hs256KeyJson :: GhcLBS.ByteString
hs256KeyJson = "{\"k\":\"adb31ov8KxCx_nXcgpYFk637HQe2MToonIunQBaIbR4UHz4HF1MfSjD0MxLgFPPtwUsEFb4HFj9HQuqxrCt_4fyYP-6Vb5_VhoLce9kdvKUK8AKUBneNlwoN87in7jzFV8pdVu_NMPRqhg6eL1yXZYgJGTR9BXylJnwhNJScThdCIWHb-f_e_ShroKWpotHXtNNgxX9IWJR4IvdjP7gRIYNW-V3rHnmqAn2S3P3hULC_BtqXiWfLChYMvZs8kgeUo_SAP3C_YyPWlIakchxEdgkxbnJCw_r7w62rDc7rEuOBNOPjMvaG6SXMz0i2S6kRjVSXY5HiFRFGSyxAG_-1pg\",\"kty\":\"oct\"}"
