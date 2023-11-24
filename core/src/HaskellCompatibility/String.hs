module HaskellCompatibility.String (
  stringToCharList,
) where

import Char (Char)
import Data.Text qualified as Text
import HaskellCompatibility.List (HaskellList)
import Pipe ((|>))
import Str (Str)
import Str.Internal qualified as StrInternal


-- | Converts a Str into a Haskell list of Chars
-- (called String in Haskell).
--
-- >>> stringToCharList "Hello"
-- ['H', 'e', 'l', 'l', 'o']
stringToCharList :: Str -> HaskellList Char
stringToCharList s =
  StrInternal.extractText s
    |> Text.unpack
{-# INLINE stringToCharList #-}