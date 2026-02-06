{-# LANGUAGE TemplateHaskell #-}

-- | Configuration for the NeoHaskell testbed application.
--
-- This module defines the testbed's configuration using the declarative
-- Config DSL. All configuration values can be set via environment variables
-- or CLI arguments.
--
-- @
-- -- Run with custom database:
-- DB_HOST=mydb.example.com DB_PASSWORD=secret cabal run nhtestbed
--
-- -- Or via CLI:
-- cabal run nhtestbed -- --port 3000
-- @
module Testbed.Config (
  TestbedConfig (..),
  HasTestbedConfig,
) where

import Config (defineConfig)
import Config qualified
import Core


defineConfig
  "TestbedConfig"
  [ Config.field @Text "dbHost"
      |> Config.doc "PostgreSQL host"
      |> Config.defaultsTo "localhost"
      |> Config.envVar "DB_HOST"
  , Config.field @Int "dbPort"
      |> Config.doc "PostgreSQL port"
      |> Config.defaultsTo 5432
      |> Config.envVar "DB_PORT"
  , Config.field @Text "dbUser"
      |> Config.doc "PostgreSQL username"
      |> Config.defaultsTo "neohaskell"
      |> Config.envVar "DB_USER"
  , Config.field @Text "dbPassword"
      |> Config.doc "PostgreSQL password"
      |> Config.defaultsTo "neohaskell"
      |> Config.envVar "DB_PASSWORD"
      |> Config.secret
  , Config.field @Text "dbName"
      |> Config.doc "PostgreSQL database name"
      |> Config.defaultsTo "neohaskell"
      |> Config.envVar "DB_NAME"
  , Config.field @Int "httpPort"
      |> Config.doc "HTTP server port"
      |> Config.defaultsTo 8080
      |> Config.envVar "HTTP_PORT"
      |> Config.cliLong "port"
      |> Config.cliShort 'p'
  , Config.field @Text "uploadDir"
      |> Config.doc "Directory for file uploads"
      |> Config.defaultsTo "./uploads"
      |> Config.envVar "UPLOAD_DIR"
  ]
