{-# LANGUAGE TemplateHaskell #-}

module Starter.Config (
  StarterConfig (..),
  HasStarterConfig,
) where

import Config (defineConfig)
import Config qualified
import Core


-- | Typed application configuration. Every field is populated from (in order):
--   1. CLI argument, 2. environment variable, 3. `.env` file, 4. default value.
--
-- Fields marked `|> Config.secret` are excluded from help output and logs.
defineConfig
  "StarterConfig"
  [ Config.field @Int "httpPort"
      |> Config.doc "HTTP server port"
      |> Config.defaultsTo (8080 :: Int)
      |> Config.envVar "PORT"
      |> Config.cliLong "port"
      |> Config.cliShort 'p'
  , Config.field @Text "uploadDir"
      |> Config.doc "Directory for file uploads"
      |> Config.defaultsTo ("./uploads" :: Text)
      |> Config.envVar "UPLOAD_DIR"
    -- SWITCH TO POSTGRES: uncomment the five fields below and wire them in src/App.hs.
    --
    -- , Config.field @Text "dbHost"
    --     |> Config.doc "PostgreSQL host"
    --     |> Config.defaultsTo ("localhost" :: Text)
    --     |> Config.envVar "DB_HOST"
    -- , Config.field @Int "dbPort"
    --     |> Config.doc "PostgreSQL port"
    --     |> Config.defaultsTo (5432 :: Int)
    --     |> Config.envVar "DB_PORT"
    -- , Config.field @Text "dbUser"
    --     |> Config.doc "PostgreSQL username"
    --     |> Config.defaultsTo ("neohaskell" :: Text)
    --     |> Config.envVar "DB_USER"
    -- , Config.field @Text "dbPassword"
    --     |> Config.doc "PostgreSQL password"
    --     |> Config.defaultsTo ("neohaskell" :: Text)
    --     |> Config.envVar "DB_PASSWORD"
    --     |> Config.secret
    -- , Config.field @Text "dbName"
    --     |> Config.doc "PostgreSQL database name"
    --     |> Config.defaultsTo ("neohaskell" :: Text)
    --     |> Config.envVar "DB_NAME"
  ]
