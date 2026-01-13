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


app :: Application
app =
  Application.new
    |> Application.withEventStore postgresConfig
    |> Application.withTransport WebTransport.server
    |> Application.withService Testbed.Service.service
    |> Application.withQuery @CartSummary
    -- Outbound: log when carts are created
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
