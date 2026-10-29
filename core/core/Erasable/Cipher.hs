{-| Encrypt protected values before they enter event or query storage.
Call 'seal' with a fresh storage context for each persisted copy, and call 'open' with the same context after loading the current key.
-}
module Erasable.Cipher (
  Key,
  Envelope (..),
  generateKey,
  keyFromBytes,
  keyToBytes,
  seal,
  open,
) where

import Basics
import Bytes (Bytes)
import Bytes qualified
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (AEADMode (AEAD_GCM), AuthTag (..))
import Crypto.Cipher.Types qualified as Cipher
import Crypto.Error (CryptoFailable (..))
import Data.ByteArray qualified as ByteArray
import Data.ByteString qualified as ByteString
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Result qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


newtype Key = Key ByteString.ByteString


instance Show Key where
  show _ = "ErasableKey <REDACTED>"


data Envelope = Envelope
  { version :: Int,
    nonce :: Text,
    ciphertext :: Text,
    tag :: Text
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON Envelope


instance Json.FromJSON Envelope


generateKey :: Task err Key
generateKey = do
  bytes <- Bytes.getRandom 32
  Task.yield (Key (Bytes.unwrap bytes))


keyFromBytes :: Bytes -> Result Text Key
keyFromBytes bytes =
  if Bytes.length bytes == 32
    then Ok (Key (Bytes.unwrap bytes))
    else Err "Invalid erasable key length"


keyToBytes :: Key -> Bytes
keyToBytes (Key bytes) = Bytes.fromLegacy bytes


seal :: Key -> Bytes -> Bytes -> Task Text Envelope
seal key context plaintext = do
  nonceBytes <- Bytes.getRandom 12
  case initialize key nonceBytes of
    Err err -> Task.throw err
    Ok cipher -> do
      let (authTag, encrypted) =
            Cipher.aeadSimpleEncrypt cipher (Bytes.unwrap context) (Bytes.unwrap plaintext) 16
      Task.yield
        Envelope
          { version = 1,
            nonce = encodeBase64 nonceBytes,
            ciphertext = encodeBase64 (Bytes.fromLegacy encrypted),
            tag = encodeBase64 (Bytes.fromLegacy (ByteArray.convert authTag))
          }


open :: Key -> Bytes -> Envelope -> Result Text Bytes
open key context envelope = do
  if envelope.version != 1
    then Err "Unsupported erasable envelope version"
    else do
      nonceBytes <- decodeBase64 envelope.nonce
      encrypted <- decodeBase64 envelope.ciphertext
      tagBytes <- decodeBase64 envelope.tag
      if Bytes.length nonceBytes != 12 || Bytes.length tagBytes != 16
        then Err "Invalid erasable envelope length"
        else do
          cipher <- initialize key nonceBytes
          let authTag = AuthTag (ByteArray.convert (Bytes.unwrap tagBytes))
          case Cipher.aeadSimpleDecrypt cipher (Bytes.unwrap context) (Bytes.unwrap encrypted) authTag of
            Nothing -> Err "Erasable envelope authentication failed"
            Just plaintext -> Ok (Bytes.fromLegacy plaintext)


initialize :: Key -> Bytes -> Result Text (Cipher.AEAD AES256)
initialize (Key keyBytes) nonceBytes = do
  case Cipher.cipherInit keyBytes of
    CryptoFailed _ -> Err "Invalid erasable cipher key"
    CryptoPassed cipher ->
      case Cipher.aeadInit AEAD_GCM cipher (Bytes.unwrap nonceBytes) of
        CryptoFailed _ -> Err "Invalid erasable cipher nonce"
        CryptoPassed aead -> Ok aead


encodeBase64 :: Bytes -> Text
encodeBase64 bytes =
  bytes |> Bytes.toBase64 |> Text.fromBytes


decodeBase64 :: Text -> Result Text Bytes
decodeBase64 encoded =
  encoded |> Text.toBytes |> Bytes.fromBase64 |> Result.mapError (\_ -> "Invalid erasable envelope encoding")
