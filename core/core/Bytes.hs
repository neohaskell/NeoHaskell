module Bytes (
  Bytes (..),
  unwrap,
  fromLegacy,
  toLazyLegacy,
  fromLazyLegacy,
  length,
  empty,
  replicate,
  pack,
) where

import Basics
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)
import Data.Word (Word8)
import Prelude qualified as GhcPrelude


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString
  deriving (Eq, Show, Ord, Generic, IsString)


unwrap :: Bytes -> ByteString.ByteString
unwrap (INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = bytes


fromLegacy :: ByteString.ByteString -> Bytes
fromLegacy = INTERNAL_CORE_BYTES_CONSTRUCTOR


fromLazyLegacy :: LazyByteString -> Bytes
fromLazyLegacy lbs =
  lbs
    |> ByteString.toStrict
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


toLazyLegacy :: Bytes -> LazyByteString
toLazyLegacy bytes =
  bytes
    |> unwrap
    |> ByteString.fromStrict


length :: Bytes -> GhcPrelude.Int
length (INTERNAL_CORE_BYTES_CONSTRUCTOR bs) =
  ByteString.length bs


empty :: Bytes
empty = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.empty


replicate :: GhcPrelude.Int -> Word8 -> Bytes
replicate n byte =
  ByteString.replicate n byte
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR


pack :: [Word8] -> Bytes
pack ws =
  ByteString.pack ws
    |> INTERNAL_CORE_BYTES_CONSTRUCTOR