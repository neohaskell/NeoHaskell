{-# LANGUAGE NoStrict #-}
module App (app) where

import Config qualified
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


-- | Get config from global storage (lazily evaluated after Application.run loads config)
config :: TestbedConfig
config = Config.get @TestbedConfig


-- | File upload configuration (declarative, no IO)
-- Uses PostgreSQL for persistent file state (survives restarts).
-- Values come from TestbedConfig (loaded by Application.run).
fileUploadConfig :: FileUploadConfig
fileUploadConfig = FileUploadConfig
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


-- | Complete application with file upload support (pure, declarative)
-- IO initialization is deferred to Application.run.
-- Config is loaded first, then used to configure PostgreSQL and file uploads.
--
-- NOTE: This module uses {-# LANGUAGE NoStrict #-} to disable strict evaluation.
-- This is necessary because the config-dependent values (postgresConfig, fileUploadConfig)
-- must be evaluated lazily - only after Application.run has loaded the config via withConfig.
-- With Strict enabled, these values would be forced during module load, before the config
-- is available, causing a panic.
app :: Application
app =
  Application.new
    |> Application.withConfig @TestbedConfig
    |> Application.withEventStore postgresConfig
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
    |> Application.withFileUpload fileUploadConfig


-- | PostgreSQL event store configuration (declarative, no IO)
-- Values come from TestbedConfig (loaded by Application.run).
postgresConfig :: PostgresEventStore
postgresConfig =
  PostgresEventStore
    { user = config.dbUser,
      password = config.dbPassword,
      host = config.dbHost,
      databaseName = config.dbName,
      port = config.dbPort
    }
