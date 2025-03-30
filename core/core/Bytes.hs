module Bytes (Bytes (..), unwrap) where

import Data.ByteString qualified as ByteString
import Prelude (Show)


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString
  deriving (Show)


unwrap :: Bytes -> ByteString.ByteString
unwrap (INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = bytes
