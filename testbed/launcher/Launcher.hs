module Main where

import Core
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.EventStore.Postgres qualified as Postgres
import Task qualified
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
        eventStore <- Postgres.new postgresConfig
        let app =
              Application.new
                |> Application.withService Testbed.Service.service
        Application.runWith eventStore app

  runApp |> Task.runOrPanic
