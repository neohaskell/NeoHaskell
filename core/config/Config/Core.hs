-- | Core types for the Config DSL.
--
-- This module defines the fundamental data structures that represent
-- configuration field specifications. These types are used by the builder
-- DSL and Template Haskell macro to generate configuration parsers.
module Config.Core (
  -- * Field Source Types
  FieldSource (..),

  -- * Optionality
  Optionality (..),

  -- * Field Specification
  FieldSpec (..),
) where

import Array (Array)
import Basics
import Char (Char)
import Maybe (Maybe)
import Text (Text)


-- | How the field value can be sourced at runtime.
--
-- Configuration values can come from multiple sources, and the parser
-- will try each source in order until one provides a value.
data FieldSource
  = -- | Environment variable name (e.g., @EnvVarSource "DATABASE_URL"@)
    EnvVarSource Text
  | -- | CLI long flag (e.g., @CliLongSource "database-url"@)
    CliLongSource Text
  | -- | CLI short flag (e.g., @CliShortSource 'd'@)
    CliShortSource Char
  deriving (Show, Eq, Generic)


-- | Whether a field is required or has a default value.
--
-- This determines whether the parser will fail if no value is provided,
-- or fall back to a default.
data Optionality value
  = -- | Field must be provided via one of its sources
    Required
  | -- | Field has a default value if not provided
    HasDefault value
  deriving (Show, Eq, Generic)


-- | Specification for a single configuration field.
--
-- This captures all the metadata needed to generate a parser for one
-- field of a configuration type.
--
-- Example usage with the builder DSL:
--
-- @
-- Config.field @Int "port"
--   |> Config.doc "HTTP port to listen on"
--   |> Config.defaultsTo 8080
--   |> Config.envVar "PORT"
--   |> Config.cliLong "port"
--   |> Config.cliShort 'p'
-- @
data FieldSpec value = FieldSpec
  { -- | The field name (used as record field name in generated type)
    fieldName :: Text
  , -- | Type name for error messages (e.g., "Int", "Text")
    fieldType :: Text
  , -- | Documentation string (required by the TH macro for --help output)
    fieldDoc :: Maybe Text
  , -- | Whether the field is required or has a default
    fieldOptionality :: Optionality value
  , -- | Sources to try when parsing this field (env vars, CLI flags)
    fieldSources :: Array FieldSource
  , -- | Whether to redact this field in logs and --help output
    fieldIsSecret :: Bool
  }
  deriving (Show, Eq, Generic)
