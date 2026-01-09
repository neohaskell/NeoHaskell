module Main where

import Core
import Service.Application qualified as Application
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.EventStore.Postgres qualified as Postgres
import Service.Query.Endpoint qualified as Endpoint
import Service.Query.Registry qualified as Registry
import Service.Query.Updater qualified as Updater
import Service.QueryObjectStore.InMemory qualified as InMemory
import Service.Transport.Web qualified as WebTransport
import Task qualified
import Testbed.Cart.Core (CartEntity, CartEvent)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Service qualified


main :: IO ()
main = do
  -- Create the shared EventStore
  let postgresConfig =
        PostgresEventStore
          { user = "neohaskell",
            password = "neohaskell",
            host = "localhost",
            databaseName = "neohaskell",
            port = 5432
          }

  -- Build and run the application
  let runApp = do
        -- Create the EventStore
        eventStore <- Postgres.new postgresConfig

        -- Create QueryObjectStore for CartSummary
        cartSummaryStore <- InMemory.new @CartSummary |> Task.mapError toText

        -- Create EntityFetcher for CartEntity (needed by query updater)
        let typedEventStore = eventStore |> EventStore.castEventStore @CartEvent
        cartEntityFetcher <-
          EntityFetcher.new
            typedEventStore
            (initialStateImpl @CartEntity)
            (updateImpl @CartEntity)
            |> Task.mapError toText

        -- Create QueryUpdater for CartEntity -> CartSummary
        let cartSummaryUpdater =
              Updater.createUpdater @CartEntity @CartSummary
                "cart-summary"
                cartEntityFetcher
                cartSummaryStore

        -- Create QueryRegistry with the updater
        let queryRegistry =
              Registry.empty
                |> Registry.register (EntityName "CartEntity") cartSummaryUpdater

        -- Create query endpoint handler
        let cartSummaryEndpoint = Endpoint.createQueryEndpoint cartSummaryStore

        -- Build the application
        let app =
              Application.new
                |> Application.withTransport WebTransport.server
                |> Application.withService Testbed.Service.service
                |> Application.withQueryRegistry queryRegistry
                |> Application.withQueryEndpoint "cart-summary" cartSummaryEndpoint

        Application.runWith eventStore app

  runApp |> Task.runOrPanic
