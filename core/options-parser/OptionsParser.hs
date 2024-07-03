module OptionsParser (
  OptionsParser,
  text,
  parseWith,
  json,
  flag,
  run,
) where

import Appendable ((++))
import Basics
import Char (Char)
import Control.Applicative qualified as Applicative
import Data.Aeson qualified as Json
import Data.Either qualified as GHC
import Data.Functor qualified as Functor
import Default (Default, defaultValue)
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import OptEnvConf qualified
import Result (Result (..))
import Text (Text, fromLinkedList, toLinkedList)
import ToText (ToText)
import Version qualified


newtype OptionsParser value = OptionsParser (OptEnvConf.Parser value)
  deriving (Functor.Functor, Applicative.Applicative)


run :: OptionsParser value -> IO value
run (OptionsParser parser) = do
  let versionText = "0.0.0"
  let ver = case Version.parse versionText of
        Just v -> v
        Nothing -> dieWith ("Could not parse version " ++ versionText)
  OptEnvConf.runParser ver parser


type TextConfig =
  [ "help" := Text,
    "long" := Text,
    "short" := Char,
    "metavar" := Text,
    "value" := Maybe Text
  ]


defaultTextConfig :: Record TextConfig
defaultTextConfig =
  ANON
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      value = Nothing
    }


text :: (ExtendsRecord TextConfig config) => Record config -> OptionsParser Text
text cfg = do
  let config = mergeRecords cfg defaultTextConfig
  let textValue = case config.value of
        Just val -> [OptEnvConf.value val]
        Nothing -> []
  let options =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.metavar (config.metavar |> Text.toLinkedList),
          OptEnvConf.reader OptEnvConf.str,
          OptEnvConf.option
        ]
  OptEnvConf.setting (textValue ++ options)
    |> OptionsParser


type JsonConfig value =
  [ "help" := Text,
    "long" := Text,
    "short" := Char,
    "metavar" := Text,
    "value" := Maybe value
  ]


defaultJsonConfig :: (Default value) => Record (JsonConfig value)
defaultJsonConfig =
  ANON
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      value = Nothing
    }


json ::
  ( Default value,
    ToText value,
    Eq value,
    Json.FromJSON value,
    ExtendsRecord (JsonConfig value) config
  ) =>
  Record config ->
  OptionsParser value
json cfg = do
  let config = mergeRecords cfg defaultJsonConfig
  let parseFunction textToParse = do
        let either = Json.eitherDecodeStrictText textToParse
        case either of
          GHC.Left err -> Err (Text.fromLinkedList err)
          GHC.Right val -> Ok val
  parseWith (parseFunction) config


type FlagConfig =
  [ "help" := Text,
    "long" := Text,
    "short" := Char,
    "value" := Maybe Bool
  ]


defaultFlagConfig :: Record FlagConfig
defaultFlagConfig =
  ANON
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      value = defaultValue
    }


flag :: (ExtendsRecord FlagConfig config) => Record config -> OptionsParser Bool
flag cfg = do
  let config = mergeRecords cfg defaultFlagConfig
  OptEnvConf.setting
    [ OptEnvConf.help (config.help |> Text.toLinkedList),
      OptEnvConf.long (config.long |> Text.toLinkedList),
      OptEnvConf.short config.short,
      OptEnvConf.switch True
    ]
    |> OptionsParser


parseWith ::
  (ToText value) =>
  (Text -> Result Text value) ->
  Record (JsonConfig value) ->
  OptionsParser value
parseWith parseFunc config = do
  let wrappedParseFunction charList = do
        let textToParse = Text.fromLinkedList charList
        let result = parseFunc textToParse
        resultToEither result

  let reader = OptEnvConf.eitherReader wrappedParseFunction

  let defaultValueConfig = case config.value of
        Just val -> [OptEnvConf.value val]
        Nothing -> []

  let options =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.metavar (config.metavar |> Text.toLinkedList),
          OptEnvConf.reader reader,
          OptEnvConf.option
        ]

  OptEnvConf.setting (defaultValueConfig ++ options)
    |> OptionsParser


resultToEither :: Result Text value -> GHC.Either (LinkedList Char) value
resultToEither (Ok val) = GHC.Right val
resultToEither (Err err) = GHC.Left (Text.toLinkedList err)