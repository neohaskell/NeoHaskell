module Service.EventStore.PostgresSpec where

import Core
import Service.EventStore.Postgres qualified as Postgres
import Service.EventStore.Postgres.Internal qualified as Internal
import Task qualified
import Test
import Test.Service.EventStore qualified as EventStore
import Var qualified


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
    describe "new method" do
      it "acquires the connection" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new ops config
          |> Task.mapError toText
          |> discard
        observe.acquireCalls
          |> varContents shouldBe 1

    let newStore = Postgres.new config |> Task.mapError toText
    EventStore.spec newStore


data NewObserve = NewObserve
  { acquireCalls :: Var Int
  }


mockNewOps :: Task Text (Internal.Ops, NewObserve)
mockNewOps = do
  acquireCalls <- Var.new 0

  let acquire :: Task Text Internal.Connection
      acquire = do
        Var.increment acquireCalls
        Task.yield Internal.MockConnection

  let newObserver = NewObserve {acquireCalls}

  let ops = Internal.Ops {acquire}
  Task.yield (ops, newObserver)
