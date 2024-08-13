module Command(
  OptionsParser,
  text,
  parseWith,
  json,
  flag,
  run,
  commands,
) where

import Appendable ((++))
import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Control.Applicative qualified as Applicative
import Data.Aeson qualified as Json
import Data.Either qualified as GHC
import Data.Functor qualified as Functor
import Data.Version (Version)
import Default (Default, defaultValue)
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Maybe qualified
import OptEnvConf qualified
import Record qualified
import Result (Result (..))
import Text (Text, fromLinkedList, toLinkedList)
import ToText (ToText)
import Version (version)


newtype OptionsParser value = OptionsParser (OptEnvConf.Parser value)
  deriving (Functor.Functor, Applicative.Applicative)


type CommandOptions value =
  Record
    [ "name" := Text,
      "description" := Text,
      "version" := Maybe Version,
      "decoder" := OptionsParser value
    ]


run :: CommandOptions value -> IO value
run options = do
  let (OptionsParser parser) = options.decoder
  let programDescription = options.description |> Text.toLinkedList
  let ver =
        options.version
          |> Maybe.withDefault [version|0.0.0|]
  OptEnvConf.runParser ver programDescription parser


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


text :: (Record.Extends TextConfig config) => Record config -> OptionsParser Text
text cfg = do
  let config = cfg |> Record.overrideWith defaultTextConfig
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
    Record.Extends (JsonConfig value) config
  ) =>
  Record config ->
  OptionsParser value
json cfg = do
  let config = cfg |> Record.overrideWith defaultJsonConfig
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


flag :: (Record.Extends FlagConfig config) => Record config -> OptionsParser Bool
flag cfg = do
  let config = cfg |> Record.overrideWith defaultFlagConfig
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


commands :: Array (CommandOptions value) -> OptionsParser value
commands commandConfigs = do
  let cmds =
        commandConfigs
          |> Array.map
            ( \config -> do
                let (OptionsParser handler) = config.decoder
                OptEnvConf.command (config.name |> Text.toLinkedList) (config.description |> Text.toLinkedList) (handler)
            )
          |> Array.toLinkedList

  OptionsParser (OptEnvConf.commands cmds)