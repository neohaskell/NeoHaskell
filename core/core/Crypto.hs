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
) where

import Basics
import Bytes (Bytes)
import Bytes qualified
import IO qualified

import Crypto.Hash qualified as Hash
import Crypto.MAC.HMAC qualified as HMAC
import Crypto.Random qualified as Random
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
  let secretBytes = Bytes.unwrap secret
  let len = BS.length secretBytes
  if len >= 32
    then Ok (HmacKey secretBytes)
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
generateHmacKey = Task.fromIO do
  randomBytes <- Random.getRandomBytes 32
  IO.yield (HmacKey randomBytes)


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
signWith (HmacKey keyBytes) message = do
  let messageBytes = Bytes.unwrap message
  let hmacResult = HMAC.hmac keyBytes messageBytes :: HMAC.HMAC Hash.SHA256
  let signatureBytes = BA.convert hmacResult :: BS.ByteString
  let encoded = Encoding.convertToBase Encoding.Base16 signatureBytes :: BS.ByteString
  Bytes.fromLegacy encoded
    |> Text.fromBytes


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
