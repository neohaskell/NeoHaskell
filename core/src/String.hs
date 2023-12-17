module String (
  String (INTERNAL_CORE_STRING_CONSTRUCTOR),
  append,
) where

import Data.Text qualified as Text


-- | 'String' is the primitive type for representing strings in
-- NeoHaskell.
newtype String
  = -- | Internal constructor for 'String', you shouldn't use it, instead use the helper functions to operate with the internal values if needed.
    INTERNAL_CORE_STRING_CONSTRUCTOR
      Text.Text


-- | Append two strings.
append :: String -> String -> String
append (INTERNAL_CORE_STRING_CONSTRUCTOR s1) (INTERNAL_CORE_STRING_CONSTRUCTOR s2) =
  INTERNAL_CORE_STRING_CONSTRUCTOR (Text.append s1 s2)