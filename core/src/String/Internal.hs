module String.Internal (
  String (InternalStrConstructor),
  extractText,
  apply,
  -- Required for the IsString instance
  -- that is used to convert string literals
  -- into String.
  GhcExts.fromString,
) where

import Data.Text qualified as Text
import GHC.Exts qualified as GhcExts
import Pipe


-- | 'String' is the primitive type for representing strings in
-- NeoHaskell.
newtype String
  = -- | Internal constructor for 'String', you shouldn't use it, instead use the helper functions to operate with the internal values if needed.
    InternalStrConstructor
      Text.Text


-- | This instance is required for NeoHaskell to be able to
-- automatically convert all string literals into 'String'.
instance GhcExts.IsString String where
  fromString charList =
    Text.pack charList
      |> InternalStrConstructor
  {-# INLINE fromString #-}


-- | Extracts the underlying 'Text.Text' from a 'String'.
--
-- This function is only exposed for compatibility purposes.
--
-- The only reason one would want to use this function is to
-- pass a 'String' to a function that expects a 'Text.Text',
-- which is probably for wrapping a function from a library
-- from the Haskell ecosystem.
--
-- prop> extractText (String someText) == someText
extractText :: String -> Text.Text
extractText (InternalStrConstructor text) =
  text
{-# INLINE extractText #-}


-- | Applies the transformation function to the underlying 'Text.Text' of a 'String'.
--
-- This function is only exposed for compatibility purposes.
--
-- The only reason one would want to use this function is to
-- pass a 'String' to a function that expects a 'Text.Text',
-- which is probably for wrapping a function from a library
-- from the Haskell ecosystem.
--
-- prop> \someText -> apply (\x -> x) (String someText) == someText
apply :: (Text.Text -> Text.Text) -> String -> String
apply f (InternalStrConstructor text) =
  InternalStrConstructor (f text)
{-# INLINE apply #-}