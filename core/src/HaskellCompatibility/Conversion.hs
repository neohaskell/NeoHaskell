module HaskellCompatibility.Conversion (
  LegacyConvertible (..),
) where

import Array qualified
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.IO qualified as GHC
import HaskellCompatibility.List qualified as HaskellList
import HaskellCompatibility.String qualified as HaskellString
import Promise qualified
import String qualified
import Types


-- | # LegacyConvertible
--
-- This trait is used to convert between the legacy Haskell types and the
-- NeoHaskell types. Useful for wrapping functions from the Haskell ecosystem.
class LegacyConvertible haskellValue value where
  toLegacy :: value -> haskellValue
  fromLegacy :: haskellValue -> value


instance LegacyConvertible () Void where
  toLegacy _ = ()
  fromLegacy _ = void


instance LegacyConvertible [Char] String where
  toLegacy = HaskellString.stringToCharList
  fromLegacy = HaskellString.fromString


instance LegacyConvertible Text String where
  toLegacy = HaskellString.stringToDataText
  fromLegacy = String.INTERNAL_CORE_STRING_CONSTRUCTOR


instance LegacyConvertible [item] (Array item) where
  toLegacy = HaskellList.toList
  fromLegacy = HaskellList.fromList


instance LegacyConvertible (Vector item) (Array item) where
  toLegacy (Array.INTERNAL_CORE_ARRAY_CONSTRUCTOR v) = v
  fromLegacy v = Array.INTERNAL_CORE_ARRAY_CONSTRUCTOR v


instance LegacyConvertible (GHC.IO a) (Promise a) where
  toLegacy (Promise.INTERNAL_CORE_PROMISE_CONSTRUCTOR io) = io
  fromLegacy = Promise.fromIO
