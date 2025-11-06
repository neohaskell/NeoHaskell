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
            { host = "localhost",
              databaseName = "neohaskell",
              user = "neohaskell",
              password = "neohaskell",
              port = 5432
            }
    describe "new method" do
      it "acquires the connection" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new ops config
          |> Task.mapError toText
          |> discard
        observe.acquireCalls
          |> varContents shouldBe 1

      it "initializes the table" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new ops config
          |> Task.mapError toText
          |> discard
        observe.initializeTableCalls
          |> varContents shouldBe 1

    let newStore = Postgres.new config |> Task.mapError toText
    EventStore.spec newStore


data NewObserve = NewObserve
  { acquireCalls :: Var Int,
    initializeTableCalls :: Var Int
  }


mockNewOps :: Task Text (Internal.Ops, NewObserve)
mockNewOps = do
  acquireCalls <- Var.new 0
  initializeTableCalls <- Var.new 0

  let acquire :: Internal.Config -> Task Text Internal.Connection
      acquire _ = do
        Var.increment acquireCalls
        Task.yield Internal.MockConnection

  let initializeTable :: Internal.Connection -> Task Text Unit
      initializeTable _ = do
        Var.increment initializeTableCalls
        Task.yield unit

  let newObserver = NewObserve {acquireCalls, initializeTableCalls}

  let ops = Internal.Ops {acquire, initializeTable}
  Task.yield (ops, newObserver)
