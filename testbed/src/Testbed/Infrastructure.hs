-- | Build the testbed's persistence and upload settings after config loads.
-- Pass these factories to the named application Event Model.
module Testbed.Infrastructure (makePostgresConfig, makeFileUploadConfig, makeS3FileUploadConfig) where

import Core
import Path qualified
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))
import Service.FileUpload.BlobStore.S3 (S3Config (..))
import Service.FileUpload.Core (FileStateStoreBackend (..), FileUploadConfig (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Testbed.Config (TestbedConfig (..))
import Text qualified


makePostgresConfig :: TestbedConfig -> PostgresEventStore
makePostgresConfig config =
  PostgresEventStore
    { user = config.dbUser,
      password = config.dbPassword,
      host = config.dbHost,
      databaseName = config.dbName,
      port = config.dbPort,
      poolSize = config.dbPoolSize,
      sslMode = case ConnectionConfig.textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err problem -> panic problem,
      erasableApplicationScope = "testbed",
      sslRootCert =
        if Text.isEmpty config.dbSslRootCert
          then Nothing
          else Just config.dbSslRootCert
    }


makeFileUploadConfig :: TestbedConfig -> FileUploadConfig LocalBlobStoreConfig
makeFileUploadConfig config = do
  let uploadDir = config.uploadDir
  let rootDir = case Path.fromText uploadDir of
        Just path -> path
        Nothing -> panic [fmt|Invalid upload directory path: #{uploadDir}|]
  FileUploadConfig
    { blobStore = LocalBlobStoreConfig {rootDir},
      stateStoreBackend = makePostgresFileStateStore config,
      maxFileSizeBytes = 10485760,
      pendingTtlSeconds = 21600,
      cleanupIntervalSeconds = 900,
      allowedContentTypes = Nothing,
      storeOriginalFilename = True
    }


makeS3FileUploadConfig :: TestbedConfig -> FileUploadConfig S3Config
makeS3FileUploadConfig config =
  FileUploadConfig
    { blobStore =
        S3Config
          { endpoint = config.s3Endpoint,
            bucket = config.s3Bucket,
            region = config.s3Region,
            accessKeyId = config.s3AccessKeyId,
            secretAccessKey = config.s3SecretAccessKey
          },
      stateStoreBackend = makePostgresFileStateStore config,
      maxFileSizeBytes = 10485760,
      pendingTtlSeconds = 21600,
      cleanupIntervalSeconds = 900,
      allowedContentTypes = Nothing,
      storeOriginalFilename = True
    }


makePostgresFileStateStore :: TestbedConfig -> FileStateStoreBackend
makePostgresFileStateStore config =
  PostgresStateStore
    { pgHost = config.dbHost,
      pgPort = config.dbPort,
      pgDatabase = config.dbName,
      pgUser = config.dbUser,
      pgPassword = config.dbPassword,
      pgSslMode = case ConnectionConfig.textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err problem -> panic problem,
      pgSslRootCert =
        if Text.isEmpty config.dbSslRootCert
          then Nothing
          else Just config.dbSslRootCert
    }
