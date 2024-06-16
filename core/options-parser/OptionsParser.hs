module OptionsParser (
  OptionsParser,
  text,
  parseWith,
  json,
  flag,
  yield,
  run,
) where

import Basics
import Combinable qualified
import Control.Applicative qualified as Applicative
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Reader qualified as Reader
import Core
import Data.Either qualified as GHC
import Data.Functor qualified as Functor
import Json qualified
import Options.Applicative qualified as Opt
import Options.Applicative qualified as OptParse
import Options.Applicative.Types qualified as OptParseTypes
import Result (Result (..))
import Text qualified


newtype OptionsParser value = OptionsParser (OptParse.Parser value)
  deriving (Functor.Functor, Applicative.Applicative)


type OptionParserInfo =
  [ "description" := Text,
    "header" := Text
  ]


defaultOptionParserInfo :: Record OptionParserInfo
defaultOptionParserInfo =
  ANON
    { description = defaultValue,
      header = defaultValue
    }


{-# INLINE yield #-}
yield :: value -> OptionsParser value
yield value = OptionsParser (OptParse.pure value)


run ::
  (ExtendsRecord OptionParserInfo info) =>
  OptionsParser value ->
  Record info ->
  IO value
run (OptionsParser parser) config = do
  let info = mergeRecords config defaultOptionParserInfo
  let programDescription =
        if info.description == ""
          then Combinable.empty
          else
            info.description
              |> Text.toLinkedList
              |> Opt.progDesc
  let programHeader =
        if info.header == ""
          then Combinable.empty
          else
            info.header
              |> Text.toLinkedList
              |> Opt.header
  let fullParser = OptParse.info (OptParse.helper OptParse.<*> parser) (programDescription ++ programHeader)
  OptParse.execParser fullParser


type OptionConfig value =
  [ "long" := Text,
    "short" := Char,
    "metavar" := Text,
    "help" := Text,
    "showDefault" := Bool,
    "value" := value
  ]


type FlagConfig value =
  [ "long" := Text,
    "short" := Char,
    "help" := Text,
    "showDefault" := Bool,
    "value" := value
  ]


text :: (ExtendsRecord (OptionConfig Text) config) => Record config -> OptionsParser Text
text config =
  makeOptionFields config
    |> OptParse.strOption
    |> OptionsParser


json ::
  ( Default value,
    ToText value,
    Eq value,
    Json.Decodable value,
    ExtendsRecord (OptionConfig value) config
  ) =>
  Record config ->
  OptionsParser value
json config = parseWith Json.decodeText config


flag :: (ExtendsRecord (FlagConfig Bool) config) => Record config -> OptionsParser Bool
flag config =
  makeFlagFields config
    |> OptParse.switch
    |> OptionsParser


-- TODO: Move me to a compatibility layer
resultToEither :: Result Text value -> GHC.Either OptParse.ParseError value
resultToEither (Ok val) = GHC.Right val
resultToEither (Err err) = GHC.Left (OptParse.ErrorMsg (Text.toLinkedList err))


-- | Converts a simple parser function to what optparse-applicative expects
parsingConverter :: (Text -> Result Text value) -> OptParse.ReadM value
parsingConverter parseFunc =
  ( \inputStr ->
      Text.fromLinkedList inputStr
        |> parseFunc
        |> resultToEither
        |> OptParse.pure
        |> Except.ExceptT
  )
    |> Reader.ReaderT
    |> OptParseTypes.ReadM


parseWith ::
  (ExtendsRecord (OptionConfig value) config, Default value, ToText value, Eq value) =>
  (Text -> Result Text value) ->
  Record config ->
  OptionsParser value
parseWith parseFunction config = do
  let parser = parsingConverter parseFunction
  makeOptionFields config
    |> OptParse.option parser
    |> OptionsParser


makeLongMod :: (OptParse.HasName f) => Text -> OptParse.Mod f a
makeLongMod "" = Combinable.empty
makeLongMod longName = OptParse.long (Text.toLinkedList longName)


makeShortMod :: (OptParse.HasName f) => Char -> OptParse.Mod f a
makeShortMod '\0' = Combinable.empty
makeShortMod shortName = OptParse.short shortName


makeMetavarMod :: (OptParse.HasMetavar f) => Text -> OptParse.Mod f a
makeMetavarMod "" = Combinable.empty
makeMetavarMod metavarName = OptParse.metavar (Text.toLinkedList metavarName)


makeHelpMod :: Text -> OptParse.Mod f a
makeHelpMod "" = Combinable.empty
makeHelpMod helpText = OptParse.help (Text.toLinkedList helpText)


makeShowDefaultMod :: (ToText a) => Bool -> OptParse.Mod f a
makeShowDefaultMod False = Combinable.empty
makeShowDefaultMod True = OptParse.showDefault


makeValueMod ::
  (Eq value, Default value) =>
  value ->
  OptParse.Mod OptParse.OptionFields value
makeValueMod value
  | value == defaultValue = Combinable.empty
  | otherwise = OptParse.value value


defaultFieldOptions :: (Default value) => Record (OptionConfig value)
defaultFieldOptions =
  ANON
    { long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      help = defaultValue,
      showDefault = defaultValue,
      value = defaultValue
    }


defaultFlagOptions :: Record (FlagConfig Bool)
defaultFlagOptions =
  ANON
    { long = defaultValue,
      short = defaultValue,
      help = defaultValue,
      showDefault = defaultValue,
      value = defaultValue
    }


makeOptionFields ::
  (ExtendsRecord (OptionConfig value) config, Default value, ToText value, Eq value) =>
  Record config ->
  OptParse.Mod OptParse.OptionFields value
makeOptionFields cfg = do
  let config = mergeRecords cfg defaultFieldOptions
  let longMod = makeLongMod config.long
  let shortMod = makeShortMod config.short
  let metavarMod = makeMetavarMod config.metavar
  let helpMod = makeHelpMod config.help
  let showDefaultMod = makeShowDefaultMod config.showDefault
  let valueMod = makeValueMod config.value
  longMod ++ shortMod ++ metavarMod ++ helpMod ++ showDefaultMod ++ valueMod


makeFlagFields ::
  (ExtendsRecord (FlagConfig Bool) config) =>
  Record config ->
  OptParse.Mod OptParse.FlagFields Bool
makeFlagFields cfg = do
  let config = mergeRecords cfg defaultFlagOptions
  let longMod = makeLongMod config.long
  let shortMod = makeShortMod config.short
  let helpMod = makeHelpMod config.help
  let showDefaultMod = makeShowDefaultMod config.showDefault
  longMod ++ shortMod ++ helpMod ++ showDefaultMod