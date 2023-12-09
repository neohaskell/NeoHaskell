module HaskellCompatibility.String (
  stringToCharList,
  stringToDataText,
  fromString,
  mapText,
  applyToText,
) where

import Char (Char)
import Data.Text qualified as Text
import HaskellCompatibility.List (HaskellList)
import Pipe ((|>))
import String


-- | INTERNAL DO NOT USE
-- Converts a String into a Haskell list of Chars.
-- This function is only exposed for compatibility purposes.
stringToCharList :: String -> HaskellList Char
stringToCharList s =
  stringToDataText s
    |> Text.unpack
{-# INLINE stringToCharList #-}


-- | INTERNAL DO NOT USE
-- Converts a String into a Data.Text.Text.
-- This function is only exposed for compatibility purposes.
stringToDataText :: String -> Text.Text
stringToDataText (INTERNAL_CORE_STRING_CONSTRUCTOR text) =
  text
{-# INLINE stringToDataText #-}


-- | INTERNAL DO NOT USE
-- Required for compatibility in order to construct literals from a List of Chars.
-- Which is what the GHC compiler gives us when we write a string literal.
fromString :: HaskellList Char -> String
fromString charList =
  Text.pack charList
    |> INTERNAL_CORE_STRING_CONSTRUCTOR
{-# INLINE fromString #-}


-- | INTERNAL DO NOT USE
-- Applies the transformation function to the underlying 'Text.Text' of a 'String'.
--
-- This function is only exposed for compatibility purposes.
--
-- The only reason one would want to use this function is to
-- pass a 'String' to a function that expects a 'Text.Text',
-- which is probably for wrapping a function from a library
-- from the Haskell ecosystem.
--
-- prop> \someText -> applyToText (\x -> x) (String someText) == someText
mapText :: (Text.Text -> Text.Text) -> String -> String
mapText f (INTERNAL_CORE_STRING_CONSTRUCTOR text) =
  INTERNAL_CORE_STRING_CONSTRUCTOR (f text)
{-# INLINE mapText #-}


-- | INTERNAL DO NOT USE
-- Applies the function to the underlying 'Text.Text' of a 'String'.
--
-- This function is only exposed for compatibility purposes.
applyToText :: (Text.Text -> a) -> String -> a
applyToText f (INTERNAL_CORE_STRING_CONSTRUCTOR text) =
  f text
{-# INLINE applyToText #-}