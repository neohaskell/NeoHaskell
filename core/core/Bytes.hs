module Bytes (Bytes (..), unwrap, fromLegacy, toLazyLegacy, fromLazyLegacy) where

import Basics
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy (LazyByteString)


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