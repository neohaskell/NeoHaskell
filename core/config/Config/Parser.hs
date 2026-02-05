-- | Runtime parsing for configuration fields.
--
-- This module bridges the 'FieldSpec' DSL to actual parsing of environment
-- variables and CLI arguments. It provides functions to parse configuration
-- values from their configured sources and handle errors gracefully.
--
-- ==== __Example Usage__
--
-- @
-- import Config.Builder qualified as Config
-- import Config.Parser qualified as Parser
--
-- portSpec :: FieldSpec Int
-- portSpec =
--   Config.field \@Int "port"
--     |> Config.doc "HTTP port to listen on"
--     |> Config.defaultsTo 8080
--     |> Config.envVar "PORT"
--
-- main :: IO ()
-- main = do
--   result <- Parser.parseField portSpec
--   case result of
--     Ok port -> print port
--     Err err -> putStrLn (Parser.formatError err)
-- @
module Config.Parser (
  -- * Error Types
  ConfigError (..),

  -- * Parsing Functions
  parseField,
  parseFieldPure,

  -- * Error Handling
  runParser,
  formatError,
  formatErrors,
) where

import Array qualified
import Config.Core (FieldSource (..), FieldSpec (..), Optionality (..))
import Config.Naming qualified as Naming
import Core
import Data.Text.IO qualified as TextIO
import System.Environment qualified
import System.Exit qualified
import System.IO qualified
import Text qualified
import Prelude qualified


-- | Errors that can occur during configuration parsing.
--
-- Each error variant captures enough context to produce helpful error
-- messages that guide users toward fixing the problem.
data ConfigError
  = -- | A required field was not provided via any source.
    --
    -- Contains: field name, field documentation (if available)
    MissingRequired Text (Maybe Text)
  | -- | A value was provided but could not be parsed to the expected type.
    --
    -- Contains: field name, expected type, actual raw value, isSecret flag
    InvalidValue Text Text Text Bool
  | -- | A general parse error occurred.
    --
    -- Contains: field name, error message
    ParseError Text Text
  deriving (Show, Eq, Generic)


-- | Attempt to parse a configuration field from its configured sources.
--
-- This function tries each source in order (environment variables first,
-- then CLI args if configured), and falls back to the default value if
-- one is specified.
--
-- ==== __Source Priority__
--
-- 1. Environment variables (tried in order they were added)
-- 2. CLI long flags (not yet implemented)
-- 3. CLI short flags (not yet implemented)
-- 4. Default value (if specified)
--
-- ==== __Example__
--
-- @
-- spec <- Config.field \@Int "port" |> Config.defaultsTo 8080 |> Config.envVar "PORT"
-- result <- parseField spec
-- -- If PORT=3000 is set: Ok 3000
-- -- If PORT is not set: Ok 8080 (default)
-- -- If PORT="abc": Err (InvalidValue "port" "Int" "abc")
-- @
parseField ::
  forall value.
  (Prelude.Read value) =>
  FieldSpec value ->
  IO (Result ConfigError value)
parseField spec = do
  -- Try each env var source in order
  maybeEnvValue <- tryEnvVarSources spec.fieldSources

  case maybeEnvValue of
    Just rawValue ->
      -- Found a value, try to parse it
      rawValue
        |> parseValue spec
        |> pure
    Nothing ->
      -- No env var found, check for default
      case spec.fieldOptionality of
        HasDefault defaultValue ->
          Ok defaultValue
            |> pure
        Required ->
          MissingRequired spec.fieldName spec.fieldDoc
            |> Err
            |> pure


-- | Pure version of 'parseField' for testing.
--
-- Takes an environment lookup function instead of reading from the actual
-- environment. This makes it easy to test parsing logic without setting
-- real environment variables.
--
-- ==== __Example__
--
-- @
-- let mockEnv "PORT" = Just "8080"
--     mockEnv _ = Nothing
--
-- parseFieldPure mockEnv portSpec  -- Ok 8080
-- @
parseFieldPure ::
  forall value.
  (Prelude.Read value) =>
  (Text -> Maybe Text) ->
  FieldSpec value ->
  Result ConfigError value
parseFieldPure lookupEnv spec =
  let maybeEnvValue = tryEnvVarSourcesPure lookupEnv spec.fieldSources
   in case maybeEnvValue of
        Just rawValue ->
          parseValue spec rawValue
        Nothing ->
          case spec.fieldOptionality of
            HasDefault defaultValue ->
              Ok defaultValue
            Required ->
              Err (MissingRequired spec.fieldName spec.fieldDoc)


-- | Try to find a value from environment variable sources.
--
-- Iterates through all EnvVarSource entries and returns the first
-- one that has a value set.
tryEnvVarSources :: Array FieldSource -> IO (Maybe Text)
tryEnvVarSources sources = do
  let envVarNames = extractEnvVarNames sources
  findFirstEnvVar envVarNames


-- | Pure version of 'tryEnvVarSources' for testing.
tryEnvVarSourcesPure :: (Text -> Maybe Text) -> Array FieldSource -> Maybe Text
tryEnvVarSourcesPure lookupEnv sources =
  let envVarNames = extractEnvVarNames sources
   in envVarNames
        |> Array.map lookupEnv
        |> Array.getJusts
        |> Array.first


-- | Extract environment variable names from field sources.
extractEnvVarNames :: Array FieldSource -> Array Text
extractEnvVarNames sources =
  sources
    |> Array.map extractEnvVarName
    |> Array.getJusts


-- | Extract the environment variable name from a single source (if applicable).
extractEnvVarName :: FieldSource -> Maybe Text
extractEnvVarName source =
  case source of
    EnvVarSource name -> Just name
    CliLongSource _ -> Nothing
    CliShortSource _ -> Nothing


-- | Look up the first environment variable that has a value.
findFirstEnvVar :: Array Text -> IO (Maybe Text)
findFirstEnvVar names =
  Array.foldM tryLookup Nothing names
 where
  tryLookup :: Maybe Text -> Text -> IO (Maybe Text)
  tryLookup (Just found) _ = pure (Just found)
  tryLookup Nothing name = do
    maybeValue <- System.Environment.lookupEnv (Text.toLinkedList name)
    case maybeValue of
      Just value ->
        value
          |> Text.fromLinkedList
          |> Text.trim
          |> Just
          |> pure
      Nothing ->
        pure Nothing


-- | Parse a raw string value into the target type.
parseValue ::
  forall value.
  (Prelude.Read value) =>
  FieldSpec value ->
  Text ->
  Result ConfigError value
parseValue spec rawValue =
  let parsed = Text.toLinkedList rawValue |> readMaybe
   in case parsed of
        Just value -> Ok value
        Nothing ->
          Err
            ( InvalidValue
                spec.fieldName
                spec.fieldType
                rawValue
                spec.fieldIsSecret
            )


-- | Safe read that returns Maybe instead of failing.
readMaybe :: (Prelude.Read a) => [Char] -> Maybe a
readMaybe str =
  case Prelude.reads str of
    [(x, "")] -> Just x
    _ -> Nothing


-- | Run a parser and exit with formatted errors on failure.
--
-- This is a convenience function for main functions that want to
-- parse configuration and exit immediately if there are errors.
--
-- The function:
--
--   1. Collects ALL parse errors (doesn't stop at first failure)
--   2. Formats all errors in a user-friendly way
--   3. Prints errors to stderr
--   4. Exits with code 1 on failure
--
-- ==== __Example__
--
-- @
-- main :: IO ()
-- main = do
--   config <- runParser do
--     port <- parseField portSpec
--     host <- parseField hostSpec
--     dbUrl <- parseField dbUrlSpec
--     pure (AppConfig port host dbUrl)
--   -- If we get here, all fields parsed successfully
--   startApp config
-- @
runParser :: IO (Result (Array ConfigError) config) -> IO config
runParser parser = do
  result <- parser
  case result of
    Ok config ->
      pure config
    Err errors -> do
      TextIO.hPutStrLn System.IO.stderr (formatErrors errors)
      System.Exit.exitFailure
 where
  -- Note: We use System.IO.stderr from base for error output


-- | Format a single configuration error for display.
--
-- The formatted error includes:
--
--   * The field name
--   * What went wrong
--   * Documentation (if available)
--   * Hints for fixing the problem
--
-- ==== __Example Output__
--
-- @
-- Configuration error for field 'databaseUrl':
--   Missing required value
--   Documentation: PostgreSQL connection URL
--   Set the DATABASE_URL environment variable
-- @
formatError :: ConfigError -> Text
formatError err =
  case err of
    MissingRequired fieldName maybeDoc ->
      let docLine = case maybeDoc of
            Just doc -> "\n  Documentation: " ++ doc
            Nothing -> ""
          envVarHint = "\n  Set the " ++ toEnvVarHint fieldName ++ " environment variable"
       in "Configuration error for field '"
            ++ fieldName
            ++ "':\n  Missing required value"
            ++ docLine
            ++ envVarHint
    InvalidValue fieldName expectedType actualValue isSecret ->
      let displayValue = if isSecret then "[REDACTED]" else actualValue
       in "Configuration error for field '"
            ++ fieldName
            ++ "':\n  Invalid value: expected "
            ++ expectedType
            ++ ", got '"
            ++ displayValue
            ++ "'"
    ParseError fieldName message ->
      "Configuration error for field '"
        ++ fieldName
        ++ "':\n  "
        ++ message


-- | Format multiple configuration errors for display.
--
-- Joins all errors with blank lines between them for readability.
formatErrors :: Array ConfigError -> Text
formatErrors errors =
  let header = "Configuration failed with " ++ Text.fromInt (Array.length errors) ++ " error(s):\n\n"
      body =
        errors
          |> Array.map formatError
          |> Text.joinWith "\n\n"
   in header ++ body


-- | Convert a field name to an environment variable hint.
--
-- Delegates to the canonical naming function.
toEnvVarHint :: Text -> Text
toEnvVarHint = Naming.toEnvVarName
