{-# OPTIONS_GHC -Wno-orphans #-}

module Cli.Codec (
  Decoder,
  Options (..),
  text,
  int,
  bool,
  applyTo,
  map,
) where

import Array qualified
import Bool
import Control.Applicative qualified as Applicative
import Data.Functor qualified as Functor
import Data.Monoid qualified as Monoid
import HaskellCompatibility.Conversion qualified as Convert
import HaskellCompatibility.List qualified as Compat
import HaskellCompatibility.Syntax
import Operators
import Optional qualified
import Options.Applicative qualified as OptParse
import Options.Applicative.Builder.Internal qualified as OptParserInternal
import Record
import String qualified
import Traits.Defaultable
import Types


data Options a = PropertyOptions
  { name :: String,
    description :: String,
    shorthand :: Char,
    defaultsTo :: Optional a,
    isHidden :: Bool,
    placeholder :: String
  }


instance Defaultable (Options a) where
  defaultValue =
    PropertyOptions
      { name = "",
        description = "",
        shorthand = ' ',
        defaultsTo = Optional.None,
        isHidden = False,
        placeholder = ""
      }


newtype Decoder a = INTERNAL_CORE_CODEC_CONSTRUCTOR (OptParse.Parser a)


text :: Options String -> Decoder String
text = codec OptParse.strOption


int :: Options Int -> Decoder Int
int = codec (OptParse.option OptParse.auto)


bool :: Options Bool -> Decoder Bool
bool = codec (OptParse.switch)


yield :: a -> Decoder a
yield value =
  INTERNAL_CORE_CODEC_CONSTRUCTOR (Applicative.pure value)


map :: (a -> b) -> Decoder a -> Decoder b
map f (INTERNAL_CORE_CODEC_CONSTRUCTOR parser) =
  INTERNAL_CORE_CODEC_CONSTRUCTOR (f Functor.<$> parser)


applyTo :: Decoder input -> Decoder (input -> output) -> Decoder output
applyTo (INTERNAL_CORE_CODEC_CONSTRUCTOR parser) (INTERNAL_CORE_CODEC_CONSTRUCTOR selfParser) =
  INTERNAL_CORE_CODEC_CONSTRUCTOR ((selfParser Applicative.<*> parser))


andThen :: (a -> Decoder b) -> Decoder a -> Decoder b
andThen f parser =
  -- x >>= f = f <$> x <*> pure ()
  -- but in terms of map, applyTo, and yield
  map f parser
    |> applyTo _


-- Private

codec ::
  forall value someFields.
  (OptParse.HasName someFields, OptParse.HasValue someFields, OptParse.HasMetavar someFields) =>
  (OptParse.Mod someFields value -> OptParse.Parser value) ->
  Options value ->
  Decoder value
codec parser props = do
  let nameOption =
        if String.isEmpty props.name
          then Optional.None
          else
            props.name
              |> Convert.toLegacy
              |> OptParse.long @someFields @value
              |> Optional.Some
  let descriptionOption =
        if String.isEmpty props.description
          then Optional.None
          else
            props.description
              |> Convert.toLegacy
              |> OptParse.help @someFields @value
              |> Optional.Some
  let shorthandOption =
        if props.shorthand == ' '
          then Optional.None
          else
            props.shorthand
              |> OptParse.short @someFields @value
              |> Optional.Some
  let placeholderOption =
        if String.isEmpty props.placeholder
          then Optional.None
          else
            props.placeholder
              |> Convert.toLegacy
              |> OptParse.metavar @someFields @value
              |> Optional.Some
  let defaultsToOption =
        props.defaultsTo
          |> Optional.applyToContents (OptParse.value @someFields @value)
  let hiddenOption =
        if not props.isHidden
          then Optional.None
          else
            OptParse.hidden @someFields @value
              |> Optional.Some
  let optionalOptions =
        [nameOption, descriptionOption, shorthandOption, placeholderOption, defaultsToOption, hiddenOption]
  let options = Array.dropNones optionalOptions

  let mod =
        Compat.toList options
          |> Monoid.mconcat
  INTERNAL_CORE_CODEC_CONSTRUCTOR (parser mod)


instance OptParserInternal.HasValue OptParse.FlagFields where
  hasValueDummy _ = ()


instance OptParserInternal.HasMetavar OptParse.FlagFields where
  hasMetavarDummy _ = ()
