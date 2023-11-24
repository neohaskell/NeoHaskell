module Str.Internal (
  Str (InternalStrConstructor),
  extractText,
  apply,
) where

import Data.Text qualified as Text
import GHC.Exts qualified as GhcExts
import Pipe


-- | 'Str' is the primitive type for representing strings in
-- NeoHaskell.
newtype Str
  = -- | Internal constructor for 'Str', you shouldn't use it, instead use the helper functions to operate with the internal values if needed.
    InternalStrConstructor
      Text.Text


-- | This instance is required for NeoHaskell to be able to
-- automatically convert all string literals into 'Str'.
instance GhcExts.IsString Str where
  fromString charList =
    Text.pack charList
      |> InternalStrConstructor
  {-# INLINE fromString #-}


-- | Extracts the underlying 'Text.Text' from a 'Str'.
--
-- This function is only exposed for compatibility purposes.
--
-- The only reason one would want to use this function is to
-- pass a 'Str' to a function that expects a 'Text.Text',
-- which is probably for wrapping a function from a library
-- from the Haskell ecosystem.
--
-- prop> extractText (Str someText) == someText
extractText :: Str -> Text.Text
extractText (InternalStrConstructor text) =
  text
{-# INLINE extractText #-}


-- | Applies the transformation function to the underlying 'Text.Text' of a 'Str'.
--
-- This function is only exposed for compatibility purposes.
--
-- The only reason one would want to use this function is to
-- pass a 'Str' to a function that expects a 'Text.Text',
-- which is probably for wrapping a function from a library
-- from the Haskell ecosystem.
--
-- prop> \someText -> apply (\x -> x) (Str someText) == someText
apply :: (Text.Text -> Text.Text) -> Str -> Str
apply f (InternalStrConstructor text) =
  InternalStrConstructor (f text)
{-# INLINE apply #-}