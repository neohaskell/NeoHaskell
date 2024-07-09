module Bytes (Bytes (..)) where

import Data.ByteString qualified as ByteString


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString