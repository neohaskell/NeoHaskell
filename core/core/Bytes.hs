module Bytes (Bytes (..), unwrap) where

import Data.ByteString qualified as ByteString


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString


unwrap :: Bytes -> ByteString.ByteString
unwrap (INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = bytes