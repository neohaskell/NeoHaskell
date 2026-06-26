module App (app) where

import Core
import Service.Application (Application, ApiInfo (..))
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.Infra.Postgres.ConnectionConfig (textToSslMode)
import Text qualified
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Integrations (periodicCartCreator)
import Testbed.Cart.Integrations.EventCounter qualified as EventCounter
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Config (TestbedConfig (..))
import Testbed.Document.Core ()
import Testbed.Service qualified
import Testbed.Stock.Queries.StockLevel (StockLevel)


-- | API metadata for the testbed application.
testbedApiInfo :: ApiInfo
testbedApiInfo = ApiInfo
  { apiTitle = "Testbed API"
  , apiVersion = "1.0.0"
  , apiDescription = "Example NeoHaskell application demonstrating event sourcing, CQRS, and integrations"
  }


-- | Complete application with file upload support (pure, declarative)
-- IO initialization is deferred to Application.run.
-- Config is loaded first, then used to configure PostgreSQL and file uploads.
--
-- This demonstrates the recommended pattern for config-dependent wiring:
-- Use 'withEventStore' and 'withFileUpload' with factory functions that take
-- the config type as a parameter. The factory is called AFTER Application.run
-- loads the config, eliminating the chicken-and-egg problem.
app :: Application
app =
  Application.new
    |> Application.withConfig @TestbedConfig
    |> Application.withEventStore makePostgresConfig
    |> Application.withTransport WebTransport.server
    |> Application.withApiInfo @() (\_ -> testbedApiInfo)
    |> Application.withService Testbed.Service.cartService
    |> Application.withService Testbed.Service.stockService
    |> Application.withService Testbed.Service.documentService
    |> Application.withQuery @CartSummary
    |> Application.withQuery @StockLevel
    -- Typed outbound handler: reserve stock when items are added to cart
    |> Application.withOutbound @ReserveStockOnItemAdded
    -- Outbound with lifecycle: count events per entity (demonstrates stateful integration)
    |> Application.withOutboundLifecycle @() @CartEntity (\_ -> EventCounter.eventCounterIntegration)
    -- Inbound: create a cart every 30 seconds
    |> Application.withInbound @() (\_ -> periodicCartCreator)
    -- File uploads with declarative config (initialized at runtime)
    |> Application.withFileUpload makeFileUploadConfig


-- | PostgreSQL event store configuration factory.
--
-- Takes the application config and produces a PostgresEventStore configuration.
-- This function is called by Application.run AFTER the config is loaded,
-- so it's safe to access config fields directly.
makePostgresConfig :: TestbedConfig -> PostgresEventStore
makePostgresConfig config =
  PostgresEventStore
    { user = config.dbUser,
      password = config.dbPassword,
      host = config.dbHost,
      databaseName = config.dbName,
      port = config.dbPort,
      poolSize = config.dbPoolSize,
      -- WI-5 (#684): parse DB_SSL_MODE; unknown tokens fail fast at startup
      -- (ADR-0064 §4: "unknown token is a single clean config error").
      -- makePostgresConfig is pure so we panic on Err — the error is caught
      -- by Application.run before any connection is attempted.
      sslMode = case textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err e -> panic e,
      -- WI-5 (#684): map "" (the Config default) to Nothing; any path -> Just path.
      sslRootCert = case Text.isEmpty config.dbSslRootCert of
        True -> Nothing
        False -> Just config.dbSslRootCert
    }


-- | File upload configuration factory.
--
-- Takes the application config and produces a FileUploadConfig.
-- Uses PostgreSQL for persistent file state (survives restarts).
-- This function is called by Application.run AFTER the config is loaded.
makeFileUploadConfig :: TestbedConfig -> FileUploadConfig
makeFileUploadConfig config = FileUploadConfig
  { blobStoreDir = config.uploadDir
  , stateStoreBackend = PostgresStateStore
      { pgHost = config.dbHost
      , pgPort = config.dbPort
      , pgDatabase = config.dbName
      , pgUser = config.dbUser
      , pgPassword = config.dbPassword
      -- WI-5 (#684): the FileUpload state-store pool MUST inherit the same
      -- DB_SSL_MODE / DB_SSL_ROOT_CERT the EventStore pool gets, else file
      -- operations silently downgrade TLS (ADR-0064). Parse identically to
      -- makePostgresConfig so the operator path is uniform.
      , pgSslMode = case textToSslMode config.dbSslMode of
          Ok mode -> mode
          Err e -> panic e
      , pgSslRootCert = case Text.isEmpty config.dbSslRootCert of
          True -> Nothing
          False -> Just config.dbSslRootCert
      }
  , maxFileSizeBytes = 10485760  -- 10 MB
  , pendingTtlSeconds = 21600    -- 6 hours
  , cleanupIntervalSeconds = 900 -- 15 minutes
  , allowedContentTypes = Nothing  -- All types allowed
  , storeOriginalFilename = True
  }
