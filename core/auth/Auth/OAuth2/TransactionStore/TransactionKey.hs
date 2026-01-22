-- | # Auth.OAuth2.TransactionStore.TransactionKey
--
-- Type-safe key for OAuth2 transaction storage.
--
-- This newtype wraps Text to prevent accidental use of raw state tokens
-- as storage keys. Keys are normalized via SHA256 hashing to ensure:
--
-- * Consistent key format regardless of input encoding
-- * Fixed-size keys for storage efficiency
-- * No raw state token values stored in the database
--
-- = Usage
--
-- @
-- -- Convert a state token to a storage key
-- let key = TransactionKey.fromText stateToken
--
-- -- Use with TransactionStore
-- store.put key transaction
-- maybeTx <- store.get key
-- @
module Auth.OAuth2.TransactionStore.TransactionKey (
  -- * Type
  TransactionKey,

  -- * Construction
  fromText,

  -- * Conversion
  toText,
) where

import Basics
import Bytes qualified
import Crypto.Hash qualified as Hash
import Data.ByteArray qualified as BA
import Data.ByteArray.Encoding qualified as Encoding
import Data.ByteString qualified as BS
import Text (Text)
import Text qualified


-- | A type-safe key for transaction storage.
--
-- Keys are derived from state tokens via SHA256 hashing.
-- The Show instance is redacted to prevent key leakage in logs.
newtype TransactionKey = TransactionKey Text
  deriving (Eq, Ord)


-- | Redacted Show instance - prevents key leakage in logs
instance Show TransactionKey where
  show _ = "TransactionKey <REDACTED>"


-- | Create a TransactionKey from a state token.
--
-- The input is hashed using SHA256 and base64url-encoded to produce
-- a consistent, fixed-size key suitable for storage.
--
-- @
-- let key = TransactionKey.fromText stateToken
-- @
fromText :: Text -> TransactionKey
fromText input = do
  let inputBytes = Text.toBytes input |> Bytes.unwrap
  let hashResult = Hash.hash inputBytes :: Hash.Digest Hash.SHA256
  let hashBytes = BA.convert hashResult :: BS.ByteString
  let encoded = Encoding.convertToBase Encoding.Base64URLUnpadded hashBytes
  let keyText = Bytes.fromLegacy encoded |> Text.fromBytes
  TransactionKey keyText


-- | Get the underlying Text representation of the key.
--
-- This returns the hashed value, not the original input.
-- Useful for storage backends that require Text keys.
toText :: TransactionKey -> Text
toText (TransactionKey t) = t
