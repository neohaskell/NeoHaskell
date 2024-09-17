module Command (
  OptionsParser,
  CommandOptions (..),
  PathConfig (..),
  text,
  path,
  parseWith,
  json,
  flag,
  parse,
  parseHandler,
  commands,
  map,
) where

import Action (Action)
import Action qualified
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
import Default (Default (..), defaultValue)
import IO (IO)
import LinkedList (LinkedList)
import Maybe (Maybe (..))
import Maybe qualified
import OptEnvConf qualified
import Path (Path)
import Result (Result (..))
import Text (Text, fromLinkedList, toLinkedList)
import ToText (Show (..), ToText)
import Unknown qualified
import Version (version)


newtype OptionsParser value = OptionsParser (OptEnvConf.Parser value)
  deriving (Functor.Functor, Applicative.Applicative)


map :: (a -> b) -> OptionsParser a -> OptionsParser b
map f (OptionsParser parser) = OptionsParser (OptEnvConf.fmap f parser)


instance (Unknown.Convertible value) => Show (OptionsParser value) where
  show _ = do
    let typeName = Unknown.getTypeName @value
    "[OptionsParser " ++ Text.toLinkedList typeName ++ "]"


data CommandOptions value = CommandOptions
  { name :: Text,
    description :: Text,
    version :: Maybe Version,
    decoder :: OptionsParser value
  }
  deriving (Show)


newtype Error = Error Text
  deriving (Show)


parse ::
  (Unknown.Convertible event) =>
  CommandOptions event ->
  Action event
parse options = Action.named "Command.parse" options


parseHandler :: CommandOptions event -> IO event
parseHandler options = do
  let (OptionsParser parser) = options.decoder
  let programDescription = options.description |> Text.toLinkedList
  let ver =
        options.version
          |> Maybe.withDefault [Version.version|0.0.0|]
  OptEnvConf.runParser ver programDescription parser


data TextConfig = TextConfig
  { help :: Text,
    long :: Text,
    short :: Char,
    metavar :: Text,
    value :: Maybe Text
  }


defaultTextConfig :: TextConfig
defaultTextConfig =
  TextConfig
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      value = Nothing
    }


instance Default TextConfig where
  def = defaultTextConfig


text :: TextConfig -> OptionsParser Text
text config = do
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


data PathConfig = PathConfig
  { help :: Text,
    long :: Text,
    short :: Char,
    metavar :: Text,
    value :: Maybe Path
  }


defaultPathConfig :: PathConfig
defaultPathConfig =
  PathConfig
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      value = Nothing
    }


instance Default PathConfig where
  def = defaultPathConfig


path :: PathConfig -> OptionsParser Path
path config = do
  let pathValue = case config.value of
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
  OptEnvConf.setting (pathValue ++ options)
    |> OptionsParser


data JsonConfig value = JsonConfig
  { help :: Text,
    long :: Text,
    short :: Char,
    metavar :: Text,
    value :: Maybe value
  }


defaultJsonConfig :: (Default value) => (JsonConfig value)
defaultJsonConfig =
  JsonConfig
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      metavar = defaultValue,
      value = Nothing
    }


instance (Default value) => Default (JsonConfig value) where
  def = defaultJsonConfig


json ::
  ( Default value,
    ToText value,
    Eq value,
    Json.FromJSON value
  ) =>
  JsonConfig value ->
  OptionsParser value
json config = do
  let parseFunction textToParse = do
        let either = Json.eitherDecodeStrictText textToParse
        case either of
          GHC.Left err -> Err (Text.fromLinkedList err)
          GHC.Right val -> Ok val
  parseWith (parseFunction) config


data FlagConfig = FlagConfig
  { help :: Text,
    long :: Text,
    short :: Char,
    value :: Maybe Bool
  }


defaultFlagConfig :: FlagConfig
defaultFlagConfig =
  FlagConfig
    { help = defaultValue,
      long = defaultValue,
      short = defaultValue,
      value = defaultValue
    }


instance Default FlagConfig where
  def = defaultFlagConfig


flag :: FlagConfig -> OptionsParser Bool
flag config = do
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
  JsonConfig value ->
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