-- | Environment variable operations.
--
-- This module provides functions for reading environment variables and
-- loading them from .env files.
--
-- = Loading .env Files
--
-- NeoHaskell applications automatically load @.env@ files when using
-- @Application.run@. You can also load them manually:
--
-- @
-- main :: IO ()
-- main = do
--   Environment.loadEnvFileIfPresent ".env"
--     |> Task.runOrPanic
--   ...
-- @
--
-- = .env File Format
--
-- @
-- # Comments start with #
-- DATABASE_URL=postgres://localhost/mydb
-- API_KEY="quoted values work too"
-- MULTILINE="line1\\nline2"
-- @
--
-- Existing environment variables are NOT overwritten.
module Environment (
  -- * Reading Variables
  getVariable,

  -- * Loading .env Files
  loadEnvFile,
  loadEnvFileIfPresent,
) where

import Basics
import Configuration.Dotenv qualified as Dotenv
import Control.Exception qualified as GhcException
import Data.Bool qualified as GhcBool
import Data.Either qualified as GhcEither
import Result qualified
import System.Directory qualified as GhcDir
import System.Environment qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Get an environment variable.
--
-- Returns an error if the variable is not set.
--
-- @
-- dbUrl <- Environment.getVariable "DATABASE_URL"
-- @
getVariable :: Text -> Task Text Text
getVariable key = Task.fromIOResult do
  let errorMsg = [fmt|Environment variable #{key} not found|]
  maybeRes <- System.Environment.lookupEnv (Text.toLinkedList key)
  maybeRes
    |> Result.fromMaybe errorMsg
    |> Result.map (\value -> value |> Text.fromLinkedList |> Text.trim)
    |> pure


-- | Load environment variables from a .env file.
--
-- Fails if the file does not exist or contains invalid syntax.
-- Existing environment variables are NOT overwritten.
--
-- @
-- Environment.loadEnvFile ".env"
--   |> Task.runOrPanic
-- @
loadEnvFile :: Text -> Task Text ()
loadEnvFile filePath = do
  let path = Text.toLinkedList filePath
  let config =
        Dotenv.defaultConfig
          { Dotenv.configPath = [path]
          , Dotenv.configOverride = False -- Don't overwrite existing vars
          }
  -- Catch any exceptions from dotenv (parse errors, IO errors, etc.)
  result <- Task.fromIO do
    GhcException.try @GhcException.SomeException (Dotenv.loadFile config)
  case result of
    GhcEither.Right _ -> Task.yield ()
    GhcEither.Left err -> Task.throw [fmt|Failed to load .env file '#{filePath}': #{GhcException.displayException err}|]


-- | Load environment variables from a .env file if it exists.
--
-- Does nothing if the file doesn't exist. Existing environment variables
-- are NOT overwritten.
--
-- This is called automatically by @Application.run@ to load @.env@ files.
--
-- @
-- Environment.loadEnvFileIfPresent ".env"
--   |> Task.runOrPanic
-- @
loadEnvFileIfPresent :: Text -> Task Text ()
loadEnvFileIfPresent filePath = do
  let path = Text.toLinkedList filePath
  exists <- Task.fromIO (GhcDir.doesFileExist path)
  case exists of
    GhcBool.True -> loadEnvFile filePath
    GhcBool.False -> Task.yield ()
