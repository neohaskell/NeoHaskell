module App (app) where

import Core
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.Cart.Core (CartEntity)
import Testbed.Cart.Integrations (cartIntegrations, periodicCartCreator)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Service qualified
import Testbed.Stock.Queries.StockLevel (StockLevel)


app :: Application
app =
  Application.new
    |> Application.withEventStore postgresConfig
    |> Application.withTransport WebTransport.server
    |> Application.withService Testbed.Service.cartService
    |> Application.withService Testbed.Service.stockService
    |> Application.withQuery @CartSummary
    |> Application.withQuery @StockLevel
    -- Outbound: reserve stock when items are added to cart (Process Manager)
    |> Application.withOutbound @CartEntity cartIntegrations
    -- Inbound: create a cart every 30 seconds
    |> Application.withInbound periodicCartCreator


postgresConfig :: PostgresEventStore
postgresConfig =
  PostgresEventStore
    { user = "neohaskell",
      password = "neohaskell",
      host = "localhost",
      databaseName = "neohaskell",
      port = 5432
    }
