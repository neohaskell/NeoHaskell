-- | # Crypto
--
-- General-purpose cryptographic signing for application code.
--
-- The first primitive is HMAC-SHA256 message signing with constant-time
-- verification — the standard scheme for authenticating webhook bodies
-- (the sender signs the raw request body, the receiver recomputes the
-- signature and compares in constant time).
--
-- = Security Properties
--
-- * HMAC-SHA256 signatures prevent tampering
-- * Keys must be at least 32 bytes (256 bits)
-- * 'verifyWith' compares in constant time to prevent timing attacks
-- * The key's Show instance is redacted to prevent key leakage
--
-- = Usage
--
-- @
-- -- Sender: sign the outgoing request body
-- key <- case Crypto.hmacKeyFromText secret of
--   Err err -> Task.throw err
--   Ok k -> Task.yield k
-- let signature =
--       requestBody
--         |> Crypto.signWith key
-- -- e.g. put the signature in an X-Signature header
--
-- -- Receiver: verify the incoming request body
-- let isAuthentic =
--       requestBody
--         |> Crypto.verifyWith key incomingSignature
-- @
module Crypto (
  -- * Types
  HmacKey,

  -- * Key Management
  hmacKeyFromText,
  hmacKeyFromBytes,
  generateHmacKey,

  -- * Signing
  signWith,

  -- * Verification
  verifyWith,

  -- * Primitives
  sha256,
  hmacSha256,
  toHex,
) where

import Basics
import Bytes (Bytes)
import Bytes qualified

import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS

import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Key for HMAC-SHA256 signing.
--
-- Must be at least 32 bytes for HMAC-SHA256 security.
-- Show instance is redacted to prevent key leakage.
newtype HmacKey = HmacKey BS.ByteString
  deriving (Eq)


-- | Redacted Show instance - NEVER reveals the actual key
instance Show HmacKey where
  show _ = "HmacKey <REDACTED>"


-- | Create an HMAC key from a text secret.
--
-- The secret must be at least 32 bytes (256 bits) once UTF-8 encoded.
-- In production, load it from an environment variable or secrets manager.
--
-- @
-- key <- case Crypto.hmacKeyFromText secret of
--   Err err -> Task.throw err
--   Ok k -> Task.yield k
-- @
hmacKeyFromText :: Text -> Result Text HmacKey
hmacKeyFromText secret =
  hmacKeyFromBytes (Text.toBytes secret)


-- | Create an HMAC key from raw secret bytes.
--
-- The secret must be at least 32 bytes (256 bits) for security.
hmacKeyFromBytes :: Bytes -> Result Text HmacKey
hmacKeyFromBytes secret = do
  let len = Bytes.length secret
  if len >= 32
    then Ok (HmacKey (Bytes.unwrap secret))
    else Err [fmt|HMAC key must be at least 32 bytes, got #{len}|]


-- | Generate a cryptographically secure random HMAC key.
--
-- Creates a 32-byte (256-bit) key suitable for HMAC-SHA256.
--
-- WARNING: In production, load a persistent key from environment/config
-- using 'hmacKeyFromText' instead. A key generated at runtime cannot
-- verify signatures issued before a restart, and no other party can
-- verify signatures made with it.
generateHmacKey :: Task err HmacKey
generateHmacKey = do
  randomBytes <- Bytes.getRandom 32
  Task.yield (HmacKey (Bytes.unwrap randomBytes))


-- | Sign a message with HMAC-SHA256.
--
-- Returns the signature as lowercase hexadecimal text (64 characters),
-- the conventional wire format for webhook signature headers.
--
-- @
-- requestBody
--   |> Crypto.signWith key
-- @
signWith :: HmacKey -> Bytes -> Text
signWith (HmacKey keyBytes) message =
  message
    |> hmacSha256 (Bytes.fromLegacy keyBytes)
    |> toHex


-- | Verify an HMAC-SHA256 signature in constant time.
--
-- The signature is hexadecimal text as produced by 'signWith'
-- (uppercase hex is accepted too). Comparison runs in constant time
-- to prevent timing attacks — never compare signatures with (==).
--
-- @
-- requestBody
--   |> Crypto.verifyWith key incomingSignature
-- @
verifyWith :: HmacKey -> Text -> Bytes -> Bool
verifyWith key signature message = do
  let expected = signWith key message
  let expectedBytes = Text.toBytes expected |> Bytes.unwrap
  let providedBytes = Text.toBytes (Text.toLower signature) |> Bytes.unwrap
  BA.constEq providedBytes expectedBytes


-- | Raw SHA-256 digest (32 bytes) of the given bytes.
--
-- This is the unkeyed hash. For message authentication use 'signWith'
-- or 'hmacSha256' instead.
--
-- @
-- payload
--   |> Crypto.sha256
--   |> Crypto.toHex
-- @
sha256 :: Bytes -> Bytes
sha256 message = do
  let digest = Hash.hash (Bytes.unwrap message) :: Hash.Digest Hash.SHA256
  Bytes.fromLegacy (BA.convert digest :: BS.ByteString)


-- | Raw HMAC-SHA256 (32 bytes) of a message under an arbitrary key.
--
-- Unlike 'signWith' this takes any key bytes and returns the raw MAC, so
-- it can be chained — protocols such as AWS Signature V4 derive a signing
-- key by feeding one HMAC result into the next. For a single webhook
-- signature with a fixed secret prefer 'signWith' and 'verifyWith', which
-- also enforce the 32-byte minimum key length and compare in constant
-- time.
--
-- @
-- message
--   |> Crypto.hmacSha256 key
-- @
hmacSha256 :: Bytes -> Bytes -> Bytes
hmacSha256 key message = do
  let mac = HMAC.hmac (Bytes.unwrap key) (Bytes.unwrap message) :: HMAC.HMAC Hash.SHA256
  Bytes.fromLegacy (BA.convert mac :: BS.ByteString)


-- | Lowercase hexadecimal text of the given bytes (two characters per byte).
--
-- >>> Bytes.pack [0, 15, 255] |> Crypto.toHex
-- "000fff"
--
-- >>> Text.toBytes "abc" |> Crypto.sha256 |> Crypto.toHex
-- "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
toHex :: Bytes -> Text
toHex bytes = do
  let encoded = Encoding.convertToBase Encoding.Base16 (Bytes.unwrap bytes) :: BS.ByteString
  Bytes.fromLegacy encoded
    |> Text.fromBytes
