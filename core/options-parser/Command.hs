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
import Combinable (Combinable)
import Combinable qualified
import Control.Applicative qualified as Applicative
import Data.Aeson qualified as Json
import Data.Either qualified as GHC
import Data.Functor qualified as Functor
import Data.Version (Version)
import Default (Default (..), defaultValue)
import LinkedList (LinkedList)
import Mappable qualified
import Maybe (Maybe (..))
import Maybe qualified
import Options.Applicative qualified as OptParse
import Path (Path)
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text, fromLinkedList, toLinkedList)
import Text qualified
import ToText (Show (..), ToText)
import Unknown qualified
import Version qualified


newtype OptionsParser value = OptionsParser (OptParse.Parser value)
  deriving (Functor.Functor, Applicative.Applicative)


map :: (a -> b) -> OptionsParser a -> OptionsParser b
map f (OptionsParser parser) = OptionsParser (Mappable.fmap f parser)


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


parseHandler :: CommandOptions event -> Task _ event
parseHandler options = do
  let (OptionsParser parser) = options.decoder
  let ver =
        options.version
          |> Maybe.withDefault [Version.version|0.0.0|]
          |> Version.toText
  let programDescription =
        [fmt|{options.description} - Version {ver}|]
          |> Text.toLinkedList
  let foo = OptParse.info (parser OptParse.<**> OptParse.helper) (OptParse.progDesc programDescription)
  OptParse.execParser foo
    |> Task.fromIO


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
text config =
  do
    let textValue = case config.value of
          Just val -> [OptParse.value val]
          Nothing -> []
    let options =
          [ OptParse.help (config.help |> Text.toLinkedList),
            OptParse.long (config.long |> Text.toLinkedList),
            OptParse.short config.short,
            OptParse.metavar (config.metavar |> Text.toLinkedList)
          ]
            ++ textValue
              |> setting

    OptParse.option OptParse.str options
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
        Just val -> [OptParse.value val]
        Nothing -> []
  let options =
        [ OptParse.help (config.help |> Text.toLinkedList),
          OptParse.long (config.long |> Text.toLinkedList),
          OptParse.short config.short,
          OptParse.metavar (config.metavar |> Text.toLinkedList)
        ]
  setting (pathValue ++ options)
    |> OptParse.option OptParse.str
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
        let either = Json.eitherDecodeStrict (Text.convert textToParse)
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
  setting
    [ OptParse.help (config.help |> Text.toLinkedList),
      OptParse.long (config.long |> Text.toLinkedList),
      OptParse.short config.short
    ]
    |> OptParse.switch
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

  let reader = OptParse.eitherReader wrappedParseFunction

  let defaultValueConfig = case config.value of
        Just val -> [OptParse.value val]
        Nothing -> []

  let options =
        [ OptParse.help (config.help |> Text.toLinkedList),
          OptParse.long (config.long |> Text.toLinkedList),
          OptParse.short config.short,
          OptParse.metavar (config.metavar |> Text.toLinkedList)
        ]

  setting (defaultValueConfig ++ options)
    |> OptParse.option reader
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
                let name = (config.name |> Text.toLinkedList)
                let description = OptParse.info handler (config.description |> Text.toLinkedList |> OptParse.progDesc)
                OptParse.command name description
            )
          |> Array.toLinkedList
          |> Combinable.mconcat

  OptionsParser (OptParse.subparser cmds)


-- Private

-- Helper function to make porting to opt-env-conf in the future easily
setting :: (Combinable m) => [m] -> m
setting = Combinable.mconcat
