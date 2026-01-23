module App (app, appTask) where

import Core
import Path qualified
import Service.Application (Application)
import Task qualified
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))
import Service.FileUpload.BlobStore.Local qualified as LocalBlobStore
import Service.FileUpload.FileStateStore.Postgres qualified as PostgresFileStore
import Service.FileUpload.Web qualified as FileUpload
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Integrations (cartIntegrations, periodicCartCreator)
import Testbed.Cart.Integrations.EventCounter qualified as EventCounter
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Document.Core ()
import Testbed.Service qualified
import Testbed.Stock.Queries.StockLevel (StockLevel)


-- | Base application builder (without file upload, which needs IO setup)
baseApp :: Application
baseApp =
  Application.new
    |> Application.withEventStore postgresConfig
    |> Application.withTransport WebTransport.server
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


-- | Pure application builder (no file uploads)
-- This is kept for backwards compatibility.
app :: Application
app = baseApp


-- | Task-based application builder with file upload support
-- Use this when you need file upload functionality.
-- Uses PostgreSQL for persistent file state (survives restarts).
appTask :: Task Text Application
appTask = do
  -- Create local blob store for file uploads
  let uploadDir = getUploadDir
  blobStore <- LocalBlobStore.createBlobStore LocalBlobStoreConfig
    { rootDir = uploadDir
    }

  -- Create PostgreSQL-backed state store (persistent across restarts)
  -- Uses the same database config as the event store
  stateStore <- PostgresFileStore.new postgresConfig

  -- Build file upload setup with defaults
  let fileUploadSetup = FileUpload.defaultFileUploadSetup blobStore stateStore

  -- Return application with file uploads enabled
  Task.yield (baseApp |> Application.withFileUpload fileUploadSetup)


-- | Get upload directory path with fallback
getUploadDir :: Path
getUploadDir = case Path.fromText "./uploads" of
  Just p -> p
  Nothing -> case Path.fromText "uploads" of
    Just p -> p
    Nothing -> panic "Failed to create upload directory path"


postgresConfig :: PostgresEventStore
postgresConfig =
  PostgresEventStore
    { user = "neohaskell",
      password = "neohaskell",
      host = "localhost",
      databaseName = "neohaskell",
      port = 5432
    }
