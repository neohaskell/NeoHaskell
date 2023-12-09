module String (
  String (INTERNAL_CORE_STRING_CONSTRUCTOR),
) where

import Data.Text qualified as Text


-- | 'String' is the primitive type for representing strings in
-- NeoHaskell.
newtype String
  = -- | Internal constructor for 'String', you shouldn't use it, instead use the helper functions to operate with the internal values if needed.
    INTERNAL_CORE_STRING_CONSTRUCTOR
      Text.Text
