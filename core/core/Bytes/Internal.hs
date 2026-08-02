-- | Internal home of the 'Bytes' newtype.
--
-- Modules that sit below 'Task' in the dependency graph (like 'Text')
-- import the type from here. That keeps the public 'Bytes' module free
-- to depend on 'Task' for effectful operations such as
-- 'Bytes.getRandom' without creating an import cycle.
--
-- Application code should import 'Bytes' instead of this module.
module Bytes.Internal (
  Bytes (..),
) where

import Basics
import Data.ByteString qualified as ByteString


newtype Bytes = INTERNAL_CORE_BYTES_CONSTRUCTOR ByteString.ByteString
  deriving (Eq, Show, Ord, Generic, IsString)
