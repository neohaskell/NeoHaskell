-- | Newtype wrapper around the fixture request hash.
--
-- Fixture files are only ever read and written by Haskell test binaries in
-- this repository, so we deliberately do NOT use RFC 8785 canonical JSON or
-- any other cross-language serialisation scheme. A plain 'Aeson.encode' of
-- the request — with object keys sorted for order-stability — SHA-256ed
-- gives us the only property we actually need: same request → same hash on
-- repeated runs of the same binary.
module Service.Integration.FixtureKey
  ( FixtureKey (..),
    fromRequest,
    fromHash,
    toText,
  )
where

import Basics
import Crypto.Hash qualified as Hash
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as AesonKeyMap
import Data.ByteArray.Encoding qualified as ByteArrayEncoding
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.List qualified as GhcList
import Data.Text.Encoding qualified as GhcTextEncoding
import Data.Vector qualified as Vector
import Text (Text)


newtype FixtureKey = FixtureKey Text
  deriving (Eq, Ord, Show, Generic)


-- | Derive a 'FixtureKey' from a 'Aeson.ToJSON' request as lowercase hex
-- SHA-256 of its sorted-key JSON encoding.
fromRequest :: forall request. (Aeson.ToJSON request) => request -> FixtureKey
fromRequest request = do
  let sorted = sortKeys (Aeson.toJSON request)
  let bytes = LBS.toStrict (Aeson.encode sorted)
  FixtureKey (hashBytes bytes)


-- | Build a 'FixtureKey' from an already-computed hash string. Internal use
-- only; callers are responsible for ensuring validity.
fromHash :: Text -> FixtureKey
fromHash = FixtureKey


toText :: FixtureKey -> Text
toText (FixtureKey hex) = hex


sortKeys :: Aeson.Value -> Aeson.Value
sortKeys value = case value of
  Aeson.Object obj ->
    obj
      |> AesonKeyMap.toList
      |> GhcList.sortOn (\(k, _) -> k)
      |> GhcList.map (\(k, v) -> (k, sortKeys v))
      |> AesonKeyMap.fromList
      |> Aeson.Object
  Aeson.Array vec -> Aeson.Array (Vector.map sortKeys vec)
  other -> other


hashBytes :: ByteString -> Text
hashBytes bytes = do
  let digest = Hash.hash bytes :: Hash.Digest Hash.SHA256
  let hex = ByteArrayEncoding.convertToBase ByteArrayEncoding.Base16 digest :: ByteString
  GhcTextEncoding.decodeUtf8 hex
