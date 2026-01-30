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
import Testbed.Document.Core ()
import Testbed.Service qualified
import Testbed.Stock.Queries.StockLevel (StockLevel)


-- | File upload configuration (declarative, no IO)
-- Uses PostgreSQL for persistent file state (survives restarts).
fileUploadConfig :: FileUploadConfig
fileUploadConfig = FileUploadConfig
  { blobStoreDir = "./uploads"
  , stateStoreBackend = PostgresStateStore
      { pgHost = postgresConfig.host
      , pgPort = postgresConfig.port
      , pgDatabase = postgresConfig.databaseName
      , pgUser = postgresConfig.user
      , pgPassword = postgresConfig.password
      }
  , maxFileSizeBytes = 10485760  -- 10 MB
  , pendingTtlSeconds = 21600    -- 6 hours
  , cleanupIntervalSeconds = 900 -- 15 minutes
  , allowedContentTypes = Nothing  -- All types allowed
  , storeOriginalFilename = True
  }


-- | Complete application with file upload support (pure, declarative)
-- IO initialization is deferred to Application.run.
app :: Application
app =
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
    -- File uploads with declarative config (initialized at runtime)
    |> Application.withFileUpload fileUploadConfig


postgresConfig :: PostgresEventStore
postgresConfig =
  PostgresEventStore
    { user = "neohaskell",
      password = "neohaskell",
      host = "localhost",
      databaseName = "neohaskell",
      port = 5432
    }
