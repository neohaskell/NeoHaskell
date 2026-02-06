-- | Declarative configuration DSL for NeoHaskell applications.
--
-- This module provides a type-safe, fail-fast configuration system that
-- validates all configuration at startup before the application accepts requests.
--
-- = Quick Start
--
-- Define your config using 'defineConfig':
--
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
-- module MyApp.Config where
--
-- import Config (defineConfig)
-- import Config qualified
--
-- defineConfig "AppConfig"
--   [ Config.field \@Int "port"
--       |> Config.doc "HTTP port to listen on"
--       |> Config.defaultsTo 8080
--       |> Config.envVar "PORT"
--       |> Config.cliLong "port"
--       |> Config.cliShort 'p'
--
--   , Config.field \@Text "databaseUrl"
--       |> Config.doc "PostgreSQL connection string"
--       |> Config.required
--       |> Config.envVar "DATABASE_URL"
--   ]
-- @
--
-- This generates:
--
-- * @data AppConfig = AppConfig { port :: Int, databaseUrl :: Text }@
-- * @instance HasParser AppConfig@ (for opt-env-conf)
-- * @type HasAppConfig = (?config :: AppConfig)@ (for implicit parameter access)
--
-- = Loading Configuration
--
-- Use 'load' to parse config from CLI args, environment variables, and config files:
--
-- @
-- main :: IO ()
-- main = do
--   config <- Config.load \@AppConfig
--   let ?config = config
--   runApp
-- @
--
-- = Accessing Configuration
--
-- With the implicit parameter in scope, access config fields directly:
--
-- @
-- startServer :: (HasAppConfig) => Task Error ()
-- startServer = do
--   Console.printLine [fmt|Starting on port {?config.port}|]
--   ...
-- @
--
-- = Field Modifiers
--
-- | Modifier | Description |
-- |----------|-------------|
-- | 'doc' | Documentation (REQUIRED) |
-- | 'envVar' | Environment variable name |
-- | 'defaultsTo' | Default value |
-- | 'required' | Must be provided |
-- | 'secret' | Redacted in output |
-- | 'cliLong' | Long CLI flag |
-- | 'cliShort' | Short CLI flag |
-- | 'envPrefix' | Prefix for nested configs |
module Config (
  -- * TH Macro
  defineConfig,

  -- * Field Definition
  field,
  enum,
  nested,

  -- * Field Modifiers
  doc,
  envVar,
  defaultsTo,
  required,
  secret,
  cliLong,
  cliShort,
  envPrefix,

  -- * Loading Config
  load,
  loadWithVersion,

  -- * Testing Support
  testWithConfig,

  -- * Re-exports from opt-env-conf
  HasParser,
) where

import Config.Builder (cliLong, cliShort, defaultsTo, doc, enum, envPrefix, envVar, field, nested, required, secret)
import Config.TH (defineConfig)
import Core
import OptEnvConf (HasParser)
import OptEnvConf qualified
import Task qualified
import Text qualified


-- | Load configuration from CLI args, environment variables, and config files.
--
-- This parses configuration at startup and fails fast if any required
-- configuration is missing or invalid.
--
-- @
-- main :: IO ()
-- main = do
--   config <- Config.load \@AppConfig
--   let ?config = config
--   runApp
-- @
load ::
  forall config.
  (OptEnvConf.HasParser config) =>
  Task Text config
load = do
  loadWithVersion defaultVersion "NeoHaskell Application"


-- | Load configuration with explicit version and description.
--
-- The version and description are shown in @--help@ output.
--
-- @
-- main :: IO ()
-- main = do
--   config <- Config.loadWithVersion version "My Application" \@MyConfig
--   ...
-- @
loadWithVersion ::
  forall config.
  (OptEnvConf.HasParser config) =>
  Version ->
  Text ->
  Task Text config
loadWithVersion ver description = do
  Task.fromIO (OptEnvConf.runSettingsParser ver (Text.toLinkedList description))
    |> Task.mapError (\err -> "Configuration error:\n" ++ err)


-- | Default version for applications that don't specify one.
defaultVersion :: Version
defaultVersion = [version|0.1.0|]


-- | Run an action with a specific configuration for testing.
--
-- This allows testing code that uses the @HasXxxConfig@ constraint without
-- setting up environment variables or CLI arguments.
--
-- @
-- spec :: Spec
-- spec = describe "OpenRouter integration" do
--   it "sends requests with API key" do
--     let testConfig = def { openRouterKey = Redacted.wrap "test-key" }
--     result <- testWithConfig testConfig do
--       sendToOpenRouter testRequest
--     result.headers \`shouldContain\` ("Authorization", "Bearer test-key")
-- @
testWithConfig ::
  forall config result.
  config ->
  ((?config :: config) => result) ->
  result
testWithConfig config action = do
  let ?config = config
  action
