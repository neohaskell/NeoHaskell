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
  readPort,
) where

import Config (defineConfig)
import Config qualified
import Core
import Environment qualified
import Task qualified
import Text qualified


readPort :: Text -> Int -> Task Text Int
readPort name defaultPort = do
  configured <- Environment.getVariable name |> Task.asResult
  case configured of
    Err _ -> Task.yield defaultPort
    Ok value -> case Text.toInt value of
      Nothing -> Task.throw [fmt|Invalid #{name}|]
      Just port -> Task.yield port


defineConfig
  "TestbedConfig"
  [ Config.field @Text "dbHost"
      |> Config.doc "PostgreSQL host"
      |> Config.defaultsTo ("localhost" :: Text)
      |> Config.envVar "DB_HOST"
  , Config.field @Int "dbPort"
      |> Config.doc "PostgreSQL port"
      |> Config.defaultsTo (5432 :: Int)
      |> Config.envVar "DB_PORT"
  , Config.field @Text "dbUser"
      |> Config.doc "PostgreSQL username"
      |> Config.defaultsTo ("neohaskell" :: Text)
      |> Config.envVar "DB_USER"
  , Config.field @Text "dbPassword"
      |> Config.doc "PostgreSQL password"
      |> Config.defaultsTo ("neohaskell" :: Text)
      |> Config.envVar "DB_PASSWORD"
      |> Config.secret
  , Config.field @Text "dbName"
      |> Config.doc "PostgreSQL database name"
      |> Config.defaultsTo ("neohaskell" :: Text)
      |> Config.envVar "DB_NAME"
  , Config.field @Int "dbPoolSize"
      |> Config.doc "PostgreSQL connection-pool size for the EventStore (shared with FileUpload); must be > 0; see ADR-0060"
      |> Config.defaultsTo (6 :: Int)
      |> Config.envVar "DB_POOL_SIZE"
  , Config.field @Text "dbSslMode"
      |> Config.doc "Postgres TLS mode: unset|disable|allow|prefer|require|verify-ca|verify-full (default unset = libpq 'prefer'; localhost/dev needs nothing)"
      |> Config.defaultsTo ("unset" :: Text)
      |> Config.envVar "DB_SSL_MODE"
  , Config.field @Text "dbSslRootCert"
      |> Config.doc "Path to a root-CA bundle for verify-full (root CAs only; empty = none)"
      |> Config.defaultsTo ("" :: Text)
      |> Config.envVar "DB_SSL_ROOT_CERT"
  , Config.field @Int "httpPort"
      |> Config.doc "HTTP server port"
      |> Config.defaultsTo (8080 :: Int)
      |> Config.envVar "HTTP_PORT"
      |> Config.cliLong "port"
      |> Config.cliShort 'p'
  , Config.field @Text "uploadDir"
      |> Config.doc "Directory for file uploads"
      |> Config.defaultsTo ("./uploads" :: Text)
      |> Config.envVar "UPLOAD_DIR"
  , Config.field @Text "s3Endpoint"
      |> Config.doc "S3 endpoint URL when running the S3 file upload example"
      |> Config.defaultsTo ("" :: Text)
      |> Config.envVar "S3_ENDPOINT"
  , Config.field @Text "s3Bucket"
      |> Config.doc "S3 bucket name for file uploads"
      |> Config.defaultsTo ("testbed-s3-bucket" :: Text)
      |> Config.envVar "S3_BUCKET"
  , Config.field @Text "s3Region"
      |> Config.doc "S3 SigV4 signing region"
      |> Config.defaultsTo ("us-east-1" :: Text)
      |> Config.envVar "S3_REGION"
  , Config.field @Text "s3AccessKeyId"
      |> Config.doc "S3 access key identifier"
      |> Config.defaultsTo ("testbed-s3-access-key" :: Text)
      |> Config.envVar "S3_ACCESS_KEY_ID"
  , Config.field @Text "s3SecretAccessKey"
      |> Config.doc "S3 secret signing key"
      |> Config.defaultsTo ("testbed-s3-secret-key" :: Text)
      |> Config.envVar "S3_SECRET_ACCESS_KEY"
      |> Config.secret
  ]
