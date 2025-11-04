module Service.EventStore.PostgresSpec where

import Core
import Service.EventStore.Postgres qualified as Postgres
import Task qualified
import Test
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "PostgresEventStore" do
    let config =
          Postgres.Config
            { host = "neohaskell",
              databaseName = "neohaskell",
              user = "neohaskell",
              password = "neohaskell"
            }
    let newStore = Postgres.new config |> Task.mapError toText
    EventStore.spec newStore
