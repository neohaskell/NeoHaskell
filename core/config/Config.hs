-- | Declarative configuration DSL with fail-fast validation.
--
-- This module provides a type-safe way to define application configuration
-- that is validated at startup, with values sourced from environment
-- variables, CLI arguments, and defaults.
--
-- = Quick Start
--
-- @
-- import Config (defineConfig)
-- import Config qualified
--
-- defineConfig "AppConfig"
--   [ Config.field \@Int "port"
--       |> Config.doc "HTTP port to listen on"
--       |> Config.defaultsTo 8080
--       |> Config.envVar "PORT"
--   , Config.field \@Text "databaseUrl"
--       |> Config.doc "Database connection string"
--       |> Config.required
--   ]
-- @
--
-- = Using Config in Functions
--
-- @
-- handleRequest :: (HasAppConfig) => Request -> Task Error Response
-- handleRequest req = do
--   let port = ?config.port  -- Direct field access!
--   ...
-- @
module Config (
  -- * Template Haskell Macro
  defineConfig,
  mkConfigField,
  ConfigField (..),

  -- * Builder DSL
  field,
  doc,
  defaultsTo,
  required,
  secret,
  envVar,
  cliLong,
  cliShort,

  -- * Config Access
  HasConfig,
  getConfig,
  withConfig,

  -- * Parsing
  ConfigError (..),
  parseField,
  parseFieldPure,
  runParser,
  formatError,
  formatErrors,

  -- * Core Types (for advanced usage)
  FieldSpec (..),
  FieldSource (..),
  Optionality (..),

  -- * Naming Utilities
  toEnvVarName,
  toScreamingSnake,
) where

import Config.Builder (cliLong, cliShort, defaultsTo, doc, envVar, field, required, secret)
import Config.Core (FieldSource (..), FieldSpec (..), Optionality (..))
import Config.HasConfig (HasConfig, getConfig, withConfig)
import Config.Naming (toEnvVarName, toScreamingSnake)
import Config.Parser (ConfigError (..), formatError, formatErrors, parseField, parseFieldPure, runParser)
import Config.TH (ConfigField (..), defineConfig, mkConfigField)
