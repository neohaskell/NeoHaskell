-- | Build the testbed's persistence and upload settings after config loads.
-- Pass these factories to the named application Event Model.
module Testbed.Infrastructure (makePostgresConfig, makeFileUploadConfig) where

import Core
import Service.EventStore.Postgres (PostgresEventStore (..))
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


makeFileUploadConfig :: TestbedConfig -> FileUploadConfig
makeFileUploadConfig config =
  FileUploadConfig
    { blobStoreDir = config.uploadDir,
      stateStoreBackend =
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
          },
      maxFileSizeBytes = 10485760,
      pendingTtlSeconds = 21600,
      cleanupIntervalSeconds = 900,
      allowedContentTypes = Nothing,
      storeOriginalFilename = True
    }
