module App (app) where

import Core
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Integrations (cartIntegrations, periodicCartCreator)
import Testbed.Cart.Integrations.EventCounter qualified as EventCounter
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Config (TestbedConfig (..))
import Testbed.Document.Core ()
import Testbed.Service qualified
import Testbed.Stock.Queries.StockLevel (StockLevel)


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
    |> Application.withApiInfo "Testbed API" "1.0.0" "Example NeoHaskell application demonstrating event sourcing, CQRS, and integrations"
    |> Application.withService Testbed.Service.cartService
    |> Application.withService Testbed.Service.stockService
    |> Application.withService Testbed.Service.documentService
    |> Application.withQuery @CartSummary
    |> Application.withQuery @StockLevel
    -- Outbound: reserve stock when items are added to cart (Process Manager)
    |> Application.withOutbound @CartEntity cartIntegrations
    -- Outbound with lifecycle: count events per entity (demonstrates stateful integration)
    |> Application.withOutboundLifecycle @CartEntity EventCounter.eventCounterIntegration
    -- Inbound: create a cart every 30 seconds
    |> Application.withInbound periodicCartCreator
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
      port = config.dbPort
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
      }
  , maxFileSizeBytes = 10485760  -- 10 MB
  , pendingTtlSeconds = 21600    -- 6 hours
  , cleanupIntervalSeconds = 900 -- 15 minutes
  , allowedContentTypes = Nothing  -- All types allowed
  , storeOriginalFilename = True
  }
