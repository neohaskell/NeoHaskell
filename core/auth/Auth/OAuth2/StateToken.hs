-- | # Auth.OAuth2.StateToken
--
-- HMAC-signed state tokens for OAuth2 CSRF protection.
--
-- State tokens are used in the OAuth2 Authorization Code flow to:
--
-- 1. Prevent CSRF attacks (random nonce)
-- 2. Carry userId through the OAuth2 callback (no session required)
-- 3. Prevent replay attacks (one-time use enforced by TransactionStore)
-- 4. Prevent mix-up attacks (provider name in payload)
--
-- = Security Properties
--
-- * HMAC-SHA256 signature prevents tampering
-- * Constant-time comparison prevents timing attacks
-- * TTL enforcement prevents stale tokens
-- * Clock skew tolerance (60s) handles minor time drift
--
-- = Usage
--
-- @
-- -- At /connect/{provider}
-- key <- getHmacKey  -- From environment/config
-- let payload = StatePayload
--       { provider = "oura"
--       , userId = "user-123"
--       , nonce = generatedNonce
--       , issuedAt = now
--       , expiresAt = now + 300  -- 5 minutes
--       }
-- stateToken <- StateToken.encodeStateToken key payload
--
-- -- At /callback/{provider}
-- payload <- StateToken.decodeStateToken key currentTime stateToken
-- -- Verify payload.provider matches route
-- -- Use payload.userId to store tokens
-- @
module Auth.OAuth2.StateToken (
  -- * Types
  HmacKey,
  StatePayload (..),
  StateTokenError (..),

  -- * Key Management
  mkHmacKey,
  generateKey,

  -- * Encoding/Decoding
  encodeStateToken,
  decodeStateToken,
) where

import Basics
import Bytes qualified
import IO qualified

import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.Random qualified as Random
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Data.Either qualified as GhcEither

import Json qualified
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | HMAC key for signing state tokens.
--
-- Must be at least 32 bytes for HMAC-SHA256 security.
-- Show instance is redacted to prevent key leakage.
newtype HmacKey = HmacKey BS.ByteString
  deriving (Eq)


-- | Redacted Show instance - NEVER reveals the actual key
instance Show HmacKey where
  show _ = "HmacKey <REDACTED>"


-- | Create an HMAC key from a secret.
--
-- The secret must be at least 32 bytes (256 bits) for security.
-- In production, load this from an environment variable or secrets manager.
--
-- @
-- key <- case mkHmacKey (Environment.get "OAUTH2_STATE_KEY") of
--   Err err -> panic err
--   Ok k -> pure k
-- @
mkHmacKey :: Text -> Result Text HmacKey
mkHmacKey secret = do
  let secretBytes = Text.toBytes secret |> Bytes.unwrap
  let len = BS.length secretBytes
  case len >= 32 of
    False -> Err [fmt|HMAC key must be at least 32 bytes, got #{len}|]
    True -> Ok (HmacKey secretBytes)


-- | Generate a cryptographically secure random HMAC key.
--
-- Creates a 32-byte (256-bit) key suitable for HMAC-SHA256.
-- Use this when you need to generate a new key at runtime.
--
-- @
-- key <- StateToken.generateKey
-- @
generateKey :: Task err HmacKey
generateKey = Task.fromIO do
  randomBytes <- Random.getRandomBytes 32
  IO.yield (HmacKey randomBytes)


-- | Payload carried in the state token.
--
-- All fields are validated on decode to ensure token integrity.
data StatePayload = StatePayload
  { -- | OAuth2 provider name (e.g., "oura", "github")
    -- Used to prevent mix-up attacks
    provider :: Text
  , -- | User ID from JWT (sub claim)
    -- Allows callback to store tokens without requiring session
    userId :: Text
  , -- | Random nonce for CSRF protection
    -- Should be cryptographically random, at least 16 bytes
    nonce :: Text
  , -- | Unix timestamp when token was issued
    issuedAt :: Int
  , -- | Unix timestamp when token expires
    expiresAt :: Int
  }
  deriving (Generic, Show, Eq)


instance Json.FromJSON StatePayload


instance Json.ToJSON StatePayload


-- | Errors that can occur during state token operations.
data StateTokenError
  = -- | HMAC signature does not match (tampering detected)
    SignatureInvalid
  | -- | Token has expired (expiresAt < currentTime)
    TokenExpired
  | -- | Token issuedAt is too far in the future (clock skew > 60s)
    TokenNotYetValid
  | -- | Token format is invalid (not base64, too short, bad JSON)
    MalformedToken
  | -- | Token timestamps are invalid (expiresAt < issuedAt)
    TokenInvalidTimestamps
  deriving (Generic, Show, Eq)


-- | Clock skew tolerance in seconds.
-- Allows for minor time differences between servers.
clockSkewTolerance :: Int
clockSkewTolerance = 60


-- | Size of HMAC-SHA256 signature in bytes.
signatureSize :: Int
signatureSize = 32


-- | Encode a state payload into a signed token.
--
-- Format: base64url(json_payload || hmac_sha256(json_payload))
--
-- The token is URL-safe and can be passed as a query parameter.
encodeStateToken ::
  forall error.
  HmacKey ->
  StatePayload ->
  Task error Text
encodeStateToken (HmacKey keyBytes) payload = do
  -- Serialize payload to JSON
  let payloadJson = Json.encodeText payload
  let payloadBytes = Text.toBytes payloadJson |> Bytes.unwrap

  -- Compute HMAC-SHA256 signature
  let hmacResult = HMAC.hmac keyBytes payloadBytes :: HMAC.HMAC Hash.SHA256
  let signatureBytes = BA.convert hmacResult :: BS.ByteString

  -- Concatenate payload and signature
  let tokenBytes = BS.append payloadBytes signatureBytes

  -- Base64url encode (URL-safe, no padding)
  let encoded = Encoding.convertToBase Encoding.Base64URLUnpadded tokenBytes
  let tokenText = Bytes.fromLegacy encoded |> Text.fromBytes

  Task.yield tokenText


-- | Decode and verify a signed state token.
--
-- Performs the following validations:
--
-- 1. Base64url decoding
-- 2. HMAC-SHA256 signature verification (constant-time)
-- 3. JSON payload parsing
-- 4. TTL validation (expiresAt >= currentTime)
-- 5. Clock skew validation (issuedAt <= currentTime + 60s)
--
-- Returns the decoded payload if all validations pass.
decodeStateToken ::
  HmacKey ->
  Int ->
  -- | Current unix timestamp
  Text ->
  -- | State token to decode
  Task StateTokenError StatePayload
decodeStateToken (HmacKey keyBytes) currentTime tokenText = do
  -- Base64url decode
  let tokenBytes = Text.toBytes tokenText |> Bytes.unwrap
  decodedBytes <- case Encoding.convertFromBase Encoding.Base64URLUnpadded tokenBytes of
    GhcEither.Left _ -> Task.throw MalformedToken
    GhcEither.Right bytes -> Task.yield bytes

  -- Check minimum length (at least signature size + 1 byte payload)
  let totalLen = BS.length decodedBytes
  case totalLen > signatureSize of
    False -> Task.throw MalformedToken
    True -> do
      -- Split into payload and signature
      let payloadLen = totalLen - signatureSize
      let (payloadBytes, signatureBytes) = BS.splitAt payloadLen decodedBytes

      -- Recompute HMAC and verify (constant-time comparison)
      let expectedHmac = HMAC.hmac keyBytes payloadBytes :: HMAC.HMAC Hash.SHA256
      let expectedSigBytes = BA.convert expectedHmac :: BS.ByteString
      case BA.constEq signatureBytes expectedSigBytes of
        False -> Task.throw SignatureInvalid
        True -> do
          -- Parse JSON payload
          let payloadText = Bytes.fromLegacy payloadBytes |> Text.fromBytes
          case Json.decodeText payloadText of
            Err _ -> Task.throw MalformedToken
            Ok payload -> do
              -- Validate timestamp consistency (expiresAt must be >= issuedAt)
              case payload.expiresAt >= payload.issuedAt of
                False -> Task.throw TokenInvalidTimestamps
                True -> do
                  -- Validate TTL
                  case currentTime <= payload.expiresAt of
                    False -> Task.throw TokenExpired
                    True -> do
                      -- Validate clock skew (issuedAt not too far in future)
                      case currentTime >= payload.issuedAt - clockSkewTolerance of
                        False -> Task.throw TokenNotYetValid
                        True -> Task.yield payload

