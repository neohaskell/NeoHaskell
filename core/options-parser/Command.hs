module Command (
  OptionsParser,
  CommandOptions (..),
  PathConfig (..),
  TextConfig (..),
  text,
  path,
  parseWith,
  json,
  flag,
  parseHandler,
  commands,
  map,
  fromSchema,
  fromFieldSchema,
) where

import Appendable ((++))
import Array (Array)
import Array qualified
import Basics
import Char (Char)
import Control.Applicative qualified as Applicative
import Data.Aeson qualified as Json
import Data.Aeson.Key qualified as GhcAesonKey
import Data.Either qualified as GHC
import Data.Functor qualified as Functor
import Data.Version (Version)
import Default (Default (..), defaultValue)
import LinkedList (LinkedList)
import LinkedList qualified
import Mappable qualified
import Maybe (Maybe (..))
import OptEnvConf qualified
import OptEnvConf.Args qualified as Args
import OptEnvConf.EnvMap qualified as EnvMap
import OptEnvConf.Error qualified as GhcOptError
import Text.Colour qualified as GhcColour
import Path (Path)
import Result (Result (..))
import Schema (FieldSchema (..), Schema (..))
import System.Environment qualified as GhcEnv
import Task (Task)
import Task qualified
import Text (Text, fromLinkedList, toLinkedList)
import Text qualified
import ToText (ToText)
import Unknown qualified


newtype OptionsParser value = OptionsParser (OptEnvConf.Parser value)
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


parseHandler :: CommandOptions event -> Task _ event
parseHandler options = do
  let (OptionsParser parser) = options.decoder
  rawArgs <- Task.fromIO GhcEnv.getArgs
  rawEnv <- Task.fromIO GhcEnv.getEnvironment
  let args = Args.parseArgs rawArgs
  let envMap = EnvMap.parse rawEnv
  parseResult <- Task.fromIO (OptEnvConf.runParserOn Nothing parser args envMap Nothing)
  case parseResult of
    GHC.Right result -> Task.yield result
    GHC.Left errs -> do
        let errorChunks = GhcOptError.renderErrors errs
        let errorText = GhcColour.renderChunksText GhcColour.WithoutColours errorChunks
        Task.throw (Error [fmt|Failed to parse CLI arguments:\n#{errorText}|])


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
  let defaultValueMods = case config.value of
        Just val -> [OptEnvConf.value val]
        Nothing -> []
  let mods =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.metavar (config.metavar |> Text.toLinkedList),
          OptEnvConf.option,
          OptEnvConf.reader OptEnvConf.str
        ]
          ++ defaultValueMods
  OptionsParser (OptEnvConf.setting mods)


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
  let defaultValueMods = case config.value of
        Just val -> [OptEnvConf.value val]
        Nothing -> []
  let mods =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.metavar (config.metavar |> Text.toLinkedList),
          OptEnvConf.option,
          OptEnvConf.reader OptEnvConf.str
        ]
          ++ defaultValueMods
  OptionsParser (OptEnvConf.setting mods)


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
  let mods =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.switch True,
          OptEnvConf.value False
        ]
  OptionsParser (OptEnvConf.setting mods)


parseWith ::
  (ToText value) =>
  (Text -> Result Text value) ->
  JsonConfig value ->
  OptionsParser value
parseWith parseFunc config = do
  let mods =
        [ OptEnvConf.help (config.help |> Text.toLinkedList),
          OptEnvConf.long (config.long |> Text.toLinkedList),
          OptEnvConf.short config.short,
          OptEnvConf.metavar (config.metavar |> Text.toLinkedList),
          OptEnvConf.option,
          OptEnvConf.reader OptEnvConf.str
        ]
  let stringParser = OptEnvConf.setting mods :: OptEnvConf.Parser (LinkedList Char)
  let checkedParser =
        OptEnvConf.checkMapEither
          ( \charList -> do
              let textToParse = Text.fromLinkedList charList
              let result = parseFunc textToParse
              resultToEither result
          )
          stringParser
  let finalParser = case config.value of
        Just val -> checkedParser Applicative.<|> Applicative.pure val
        Nothing -> checkedParser
  OptionsParser finalParser


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
                let name = config.name |> Text.toLinkedList
                let description = config.description |> Text.toLinkedList
                OptEnvConf.command name description handler
            )
          |> Array.toLinkedList
  OptionsParser (OptEnvConf.commands cmds)


-- | Generate a parser from a Schema that produces a JSON Value.
-- This allows schema-driven CLI argument parsing where each field
-- in the schema becomes a CLI option.
fromSchema :: Schema -> OptionsParser Json.Value
fromSchema schema =
  case schema of
    SObject fields -> do
      let parsers = fields |> Array.map fromFieldSchema
      let initial = OptionsParser (Applicative.pure [])
      -- NOTE: Array.foldl has non-standard argument order: (element -> accumulator -> accumulator)
      -- instead of the standard (accumulator -> element -> accumulator)
      let combine parser acc = do
            let (OptionsParser accP) = acc
            let (OptionsParser fieldP) = parser
            OptionsParser (Applicative.liftA2 (\pairs pair -> pairs ++ [pair]) accP fieldP)
      let combined = Array.foldl combine initial parsers
      map
        ( \pairs -> do
            let jsonPairs = Functor.fmap (\(k, v) -> (GhcAesonKey.fromText k, v)) pairs
            Json.object jsonPairs
        )
        combined
    SText ->
      map Json.String (OptionsParser (OptEnvConf.setting [OptEnvConf.reader OptEnvConf.str, OptEnvConf.argument]))
    SInt ->
      map (\n -> Json.toJSON (n :: Int)) (OptionsParser (OptEnvConf.setting [OptEnvConf.reader OptEnvConf.auto, OptEnvConf.argument]))
    SNumber ->
      map (\n -> Json.toJSON (n :: Float)) (OptionsParser (OptEnvConf.setting [OptEnvConf.reader OptEnvConf.auto, OptEnvConf.argument]))
    SBool ->
      map Json.Bool (OptionsParser (OptEnvConf.setting [OptEnvConf.reader OptEnvConf.auto, OptEnvConf.argument]) :: OptionsParser Bool)
    _ ->
      map Json.String (OptionsParser (OptEnvConf.setting [OptEnvConf.reader OptEnvConf.str, OptEnvConf.argument]))


-- | Generate a parser for a single field producing a (fieldName, value) pair.
-- The field name is converted to kebab-case for the CLI long option.
fromFieldSchema :: FieldSchema -> OptionsParser (Text, Json.Value)
fromFieldSchema field = do
  let kebabName = field.fieldName |> Text.toKebabCase |> Text.toLinkedList
  let helpText = field.fieldDescription |> Text.toLinkedList
  case field.fieldSchema of
    SText -> do
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help helpText, OptEnvConf.option, OptEnvConf.reader OptEnvConf.str]
      map (\val -> (field.fieldName, Json.String val)) (OptionsParser parser)
    SInt -> do
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help helpText, OptEnvConf.option, OptEnvConf.reader OptEnvConf.auto] :: OptEnvConf.Parser Int
      map (\n -> (field.fieldName, Json.toJSON n)) (OptionsParser parser)
    SNumber -> do
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help helpText, OptEnvConf.option, OptEnvConf.reader OptEnvConf.auto] :: OptEnvConf.Parser Float
      map (\n -> (field.fieldName, Json.toJSON n)) (OptionsParser parser)
    SBool -> do
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help helpText, OptEnvConf.switch True, OptEnvConf.value False]
      map (\b -> (field.fieldName, Json.Bool b)) (OptionsParser parser)
    SOptional innerSchema -> do
      let innerField = FieldSchema {fieldName = field.fieldName, fieldSchema = innerSchema, fieldRequired = False, fieldDescription = field.fieldDescription}
      let (OptionsParser innerParser) = fromFieldSchema innerField
      let optionalParser = Applicative.optional innerParser
      map
        ( \result ->
            case result of
              Just (_, val) -> (field.fieldName, val)
              Nothing -> (field.fieldName, Json.Null)
        )
        (OptionsParser optionalParser)
    SEnum variants -> do
      let variantList = variants |> Array.toLinkedList |> LinkedList.map Text.toLinkedList |> LinkedList.intersperse ", " |> LinkedList.concat
      let enumHelp = helpText ++ " (one of: " ++ variantList ++ ")"
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help enumHelp, OptEnvConf.option, OptEnvConf.reader OptEnvConf.str] :: OptEnvConf.Parser Text
      let checkedParser =
            OptEnvConf.checkMapEither
              ( \val ->
                  case Array.find (\v -> v == val) variants of
                    Just _ -> GHC.Right val
                    Nothing -> GHC.Left (Text.toLinkedList [fmt|Invalid value: expected one of #{variantList}|])
              )
              parser
      map (\val -> (field.fieldName, Json.String val)) (OptionsParser checkedParser)
    SArray innerSchema -> do
      let innerField = FieldSchema {fieldName = field.fieldName, fieldSchema = innerSchema, fieldRequired = field.fieldRequired, fieldDescription = field.fieldDescription}
      let (OptionsParser innerParser) = fromFieldSchema innerField
      let manyParser = map (\pairs -> pairs |> Mappable.fmap (\(_, v) -> v)) (OptionsParser (Applicative.many innerParser))
      map
        ( \vals ->
            (field.fieldName, Json.toJSON vals)
        )
        manyParser
    _ -> do
      -- Fallback: parse as JSON string for complex types
      let parser = OptEnvConf.setting [OptEnvConf.long kebabName, OptEnvConf.help helpText, OptEnvConf.option, OptEnvConf.reader OptEnvConf.str] :: OptEnvConf.Parser Text
      map (\val -> (field.fieldName, Json.String val)) (OptionsParser parser)
