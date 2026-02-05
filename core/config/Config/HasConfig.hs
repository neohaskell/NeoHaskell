{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE RankNTypes #-}

-- | Implicit parameter infrastructure for zero-boilerplate config access.
--
-- This module provides the core mechanism for accessing configuration values
-- using Haskell's implicit parameter feature. By using the 'HasConfig' constraint,
-- functions can access configuration via @?config@ without any explicit passing.
--
-- = Overview
--
-- The implicit parameter approach provides several benefits:
--
-- * Zero runtime cost - compiles to direct field access
-- * No monad transformer overhead
-- * Clean syntax: @?config.fieldName@ reads naturally
-- * Easy testing - just bind a different config value
--
-- = Usage
--
-- Define your functions with the 'HasConfig' constraint:
--
-- @
-- sendRequest :: (HasConfig AppConfig) => Request -> Task Error Response
-- sendRequest request = do
--   let apiKey = ?config.openRouterKey  -- Direct field access!
--   Http.post "..."
--     |> Http.bearer (Secret.unwrap apiKey)
--     |> Http.send
-- @
--
-- Run them by binding the config value:
--
-- @
-- main :: IO ()
-- main = do
--   cfg <- loadConfig
--   withConfig cfg do
--     response <- sendRequest myRequest
--     -- ...
-- @
--
-- = Testing
--
-- Testing is straightforward - just provide a test config:
--
-- @
-- testMyFeature :: Task Error ()
-- testMyFeature =
--   withConfig testConfig do
--     result <- sendRequest testRequest
--     -- assert on result
-- @
module Config.HasConfig (
  -- * Config Constraint
  HasConfig,

  -- * Config Access
  getConfig,

  -- * Running with Config
  withConfig,
) where


-- | Constraint alias for functions that need access to a configuration value.
--
-- This type alias wraps the implicit parameter constraint, providing a cleaner
-- interface. Any function with this constraint can access the config via @?config@.
--
-- ==== __Examples__
--
-- @
-- -- A function that needs database config
-- connectDb :: (HasConfig DbConfig) => Task Error Connection
-- connectDb = do
--   let url = ?config.databaseUrl
--   Db.connect url
--
-- -- A function that needs API config
-- callApi :: (HasConfig ApiConfig) => Task Error Response
-- callApi = do
--   let key = ?config.apiKey
--   let timeout = ?config.requestTimeout
--   Http.get "..."
--     |> Http.header "Authorization" key
--     |> Http.timeout timeout
--     |> Http.send
-- @
type HasConfig config = (?config :: config)


-- | Retrieve the current configuration value.
--
-- This function simply returns the implicitly-bound config value.
-- It's useful when you need to pass the entire config object to another
-- function, rather than accessing individual fields.
--
-- ==== __Examples__
--
-- @
-- -- Pass the whole config to another function
-- initServices :: (HasConfig AppConfig) => Task Error Services
-- initServices = do
--   let cfg = getConfig
--   db <- initDb cfg.database
--   cache <- initCache cfg.redis
--   pure Services{db, cache}
-- @
getConfig :: (HasConfig config) => config
getConfig = ?config


-- | Run an action with a given configuration value bound.
--
-- This function establishes the implicit parameter binding, making
-- the config value available to all code within the action via @?config@.
--
-- ==== __Examples__
--
-- @
-- -- In production, load config and run the app
-- main :: IO ()
-- main = do
--   cfg <- loadAppConfig
--   withConfig cfg do
--     runServer
--
-- -- In tests, provide a test config
-- spec :: Test.Spec
-- spec = Test.describe "API" do
--   Test.it "handles requests" do
--     withConfig testConfig do
--       response <- handleRequest testRequest
--       response.status `Test.shouldBe` 200
--
-- -- Nest configs for different parts of the system
-- runApp :: AppConfig -> Task Error ()
-- runApp cfg =
--   withConfig cfg do
--     -- All code here can use ?config
--     initDb
--     startWorkers
--     runServer
-- @
withConfig :: config -> (HasConfig config => a) -> a
withConfig cfg action = let ?config = cfg in action
