module OptionsParser (
  OptionsParser,
  text,
  parseWith,
) where

import Combinable qualified
import Control.Monad.Trans.Except qualified as Except
import Control.Monad.Trans.Reader qualified as Reader
import Core
import Data.Either qualified as GHC
import Data.Record.Anon
import Data.Record.Anon.Simple (Record)
import Data.Record.Anon.Simple qualified as Simple
import Options.Applicative qualified as OptParse
import Options.Applicative.Types qualified as OptParseTypes
import Result (Result (..))
import Text qualified


newtype OptionsParser value = OptionsParser (OptParse.Parser value)


type Config value =
  [ "long" := Text,
    "short" := Char,
    "metavar" := Text,
    "help" := Text,
    "showDefault" := Bool,
    "value" := value
  ]


text :: (SubRow (Config Text) config) => Record config -> OptionsParser Text
text config =
  makeModFields config
    |> OptParse.strOption
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
  (SubRow (Config value) config, Default value, ToText value, Eq value) =>
  (Text -> Result Text value) ->
  Record config ->
  OptionsParser value
parseWith parseFunction config = do
  let parser = parsingConverter parseFunction
  makeModFields config
    |> OptParse.option parser
    |> OptionsParser


makeLongMod :: Text -> OptParse.Mod OptParse.OptionFields a
makeLongMod "" = Combinable.empty
makeLongMod longName = OptParse.long (Text.toLinkedList longName)


makeShortMod :: Char -> OptParse.Mod OptParse.OptionFields a
makeShortMod '\0' = Combinable.empty
makeShortMod shortName = OptParse.short shortName


makeMetavarMod :: Text -> OptParse.Mod OptParse.OptionFields a
makeMetavarMod "" = Combinable.empty
makeMetavarMod metavarName = OptParse.metavar (Text.toLinkedList metavarName)


makeHelpMod :: Text -> OptParse.Mod OptParse.OptionFields a
makeHelpMod "" = Combinable.empty
makeHelpMod helpText = OptParse.help (Text.toLinkedList helpText)


makeShowDefaultMod :: (ToText a) => Bool -> OptParse.Mod OptParse.OptionFields a
makeShowDefaultMod False = Combinable.empty
makeShowDefaultMod True = OptParse.showDefault


makeValueMod ::
  (Eq value, Default value) =>
  value ->
  OptParse.Mod OptParse.OptionFields value
makeValueMod value
  | value == defaultValue = Combinable.empty
  | otherwise = OptParse.value value


defaultFieldOptions :: (Default value) => Record (Config value)
defaultFieldOptions =
  ANON
    { long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      help = defaultValue,
      showDefault = defaultValue,
      value = defaultValue
    }


makeModFields ::
  (SubRow (Config value) config, Default value, ToText value, Eq value) =>
  Record config ->
  OptParse.Mod OptParse.OptionFields value
makeModFields cfg = do
  let config = Simple.inject cfg defaultFieldOptions
  let longMod = makeLongMod config.long
  let shortMod = makeShortMod config.short
  let metavarMod = makeMetavarMod config.metavar
  let helpMod = makeHelpMod config.help
  let showDefaultMod = makeShowDefaultMod config.showDefault
  let valueMod = makeValueMod config.value
  longMod ++ shortMod ++ metavarMod ++ helpMod ++ showDefaultMod ++ valueMod
