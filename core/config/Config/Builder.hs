{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Builder DSL for configuration field specifications.
--
-- This module provides a fluent API for constructing 'FieldSpec' values
-- using the pipe operator @|>@. Fields are configured by chaining
-- modifier functions together.
--
-- ==== __Example__
--
-- @
-- import Config.Builder qualified as Config
--
-- portSpec :: FieldSpec Int
-- portSpec =
--   Config.field \@Int "port"
--     |> Config.doc "HTTP port to listen on"
--     |> Config.defaultsTo 8080
--     |> Config.envVar "PORT"
--     |> Config.cliLong "port"
--     |> Config.cliShort 'p'
-- @
module Config.Builder (
  -- * Field Constructor
  field,

  -- * Documentation
  doc,

  -- * Default Values
  defaultsTo,
  required,

  -- * Sensitivity
  secret,

  -- * Sources
  envVar,
  cliLong,
  cliShort,
) where

import Array qualified
import Config.Core (FieldSource (..), FieldSpec (..), Optionality (..))
import Config.Naming qualified as Naming
import Core
import TypeName qualified


-- | Create a new field specification with the given name.
--
-- The type parameter must be provided explicitly using @TypeApplications@
-- so that the field type can be recorded for error messages.
--
-- By default, the field:
--
--   * Is required (no default value)
--   * Is not secret
--   * Has an auto-generated environment variable name (e.g., @"databaseUrl"@ becomes @"DATABASE_URL"@)
--
-- ==== __Examples__
--
-- >>> :set -XTypeApplications
-- >>> field @Int "port"
-- FieldSpec {fieldName = "port", fieldType = "Int", ...}
--
-- >>> field @Text "databaseUrl"
-- FieldSpec {fieldName = "databaseUrl", fieldType = "Text", ...}
field :: forall value. (Typeable value) => Text -> FieldSpec value
field name =
  FieldSpec
    { fieldName = name
    , fieldType = TypeName.reflect @value
    , fieldDoc = Nothing
    , fieldOptionality = Required
    , fieldSources = Array.wrap (EnvVarSource (Naming.toEnvVarName name))
    , fieldIsSecret = False
    }


-- | Set the documentation string for a field.
--
-- This documentation is used in @--help@ output and error messages.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> doc "HTTP port to listen on"
-- FieldSpec {fieldDoc = Just "HTTP port to listen on", ...}
doc :: Text -> FieldSpec value -> FieldSpec value
doc documentation spec =
  spec {fieldDoc = Just documentation}


-- | Set a default value for a field.
--
-- If no value is provided via environment variable or CLI flag,
-- this default will be used. Changes the field from 'Required' to 'HasDefault'.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> defaultsTo 8080
-- FieldSpec {fieldOptionality = HasDefault 8080, ...}
defaultsTo :: value -> FieldSpec value -> FieldSpec value
defaultsTo value spec =
  spec {fieldOptionality = HasDefault value}


-- | Mark a field as required (no default value).
--
-- This is the default behavior, but can be used to override a previous
-- 'defaultsTo' call if needed.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> defaultsTo 8080 |> required
-- FieldSpec {fieldOptionality = Required, ...}
required :: FieldSpec value -> FieldSpec value
required spec =
  spec {fieldOptionality = Required}


-- | Mark a field as containing sensitive data.
--
-- Secret fields will have their values redacted in logs and @--help@ output.
-- Use this for passwords, API keys, and other sensitive information.
--
-- ==== __Example__
--
-- >>> field @Text "apiKey" |> secret
-- FieldSpec {fieldIsSecret = True, ...}
secret :: FieldSpec value -> FieldSpec value
secret spec =
  spec {fieldIsSecret = True}


-- | Add an environment variable source with higher precedence.
--
-- Note: By default, 'field' already adds an auto-generated environment
-- variable source based on the field name. Using this function adds an
-- explicit env var at higher precedence (checked first), which also
-- effectively overrides the auto-generated one.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> envVar "HTTP_PORT"
-- -- HTTP_PORT is checked first, then PORT (auto-generated)
envVar :: Text -> FieldSpec value -> FieldSpec value
envVar name spec =
  spec {fieldSources = Array.prepend (Array.wrap (EnvVarSource name)) spec.fieldSources}


-- | Add a long CLI flag source with higher precedence.
--
-- Long flags are specified with two dashes (e.g., @--port@).
--
-- __Note:__ CLI argument parsing is not yet implemented. This modifier
-- is a placeholder that records the source for future implementation.
-- Currently only environment variables are parsed.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> cliLong "port"
-- -- Will be settable via: --port 8080 (when CLI parsing is implemented)
cliLong :: Text -> FieldSpec value -> FieldSpec value
cliLong name spec =
  spec {fieldSources = Array.prepend (Array.wrap (CliLongSource name)) spec.fieldSources}


-- | Add a short CLI flag source with higher precedence.
--
-- Short flags are specified with a single dash and a single character
-- (e.g., @-p@).
--
-- __Note:__ CLI argument parsing is not yet implemented. This modifier
-- is a placeholder that records the source for future implementation.
-- Currently only environment variables are parsed.
--
-- ==== __Example__
--
-- >>> field @Int "port" |> cliShort 'p'
-- -- Will be settable via: -p 8080 (when CLI parsing is implemented)
cliShort :: Char -> FieldSpec value -> FieldSpec value
cliShort char spec =
  spec {fieldSources = Array.prepend (Array.wrap (CliShortSource char)) spec.fieldSources}
