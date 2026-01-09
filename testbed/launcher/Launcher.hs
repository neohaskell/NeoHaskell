module Main where

import Core
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Task qualified
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Service qualified


main :: IO ()
main = do
  let postgresConfig =
        PostgresEventStore
          { user = "neohaskell",
            password = "neohaskell",
            host = "localhost",
            databaseName = "neohaskell",
            port = 5432
          }

  let app =
        Application.new
          |> Application.withEventStore postgresConfig
          |> Application.withTransport WebTransport.server
          |> Application.withService Testbed.Service.service
          |> Application.withQuery @CartSummary

  Application.run app |> Task.runOrPanic
