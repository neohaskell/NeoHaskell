module Bytes (Bytes (..), unwrap) where

import Basics (IsString)
import Data.ByteString qualified as ByteString
import GHC.Generics (Generic)
import Prelude (Eq, Ord, Show)


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString
  deriving (Eq, Show, Ord, Generic, IsString)


unwrap :: Bytes -> ByteString.ByteString
unwrap (INTERNAL_CORE_BYTES_CONSTRUCTOR bytes) = bytes
