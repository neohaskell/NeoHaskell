module Service.EventStore.PostgresSpec where

import Core
import Service.EventStore.Postgres qualified as Postgres
import Service.EventStore.Postgres.Internal qualified as Internal
import Service.EventStore.Postgres.Internal.Sessions qualified as Sessions
import Task qualified
import Test
import Test.Service.EventStore qualified as EventStore
import Test.Service.EventStore.Core (MyEvent)
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
        Internal.new @MyEvent ops config
          |> Task.mapError toText
          |> discard
        observe.acquireCalls
          |> varContents shouldBe 1

      it "initializes the table" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new @MyEvent ops config
          |> Task.mapError toText
          |> discard
        observe.initializeTableCalls
          |> varContents shouldBe 1

      it "initializes the subscriptions" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new @MyEvent ops config
          |> Task.mapError toText
          |> discard
        observe.initializeSubscriptionsCalls
          |> varContents shouldBe 1

    let newStore = do
          let ops = Internal.defaultOps
          dropPostgres ops config
          Postgres.new config |> Task.mapError toText
    EventStore.spec newStore


data NewObserve = NewObserve
  { acquireCalls :: Var Int,
    initializeTableCalls :: Var Int,
    initializeSubscriptionsCalls :: Var Int
  }


dropPostgres :: Internal.Ops eventType -> Postgres.Config -> Task Text Unit
dropPostgres ops config = do
  connection <- ops.acquire config
  res <-
    Sessions.dropEventsTableSession
      |> Sessions.run connection
      |> Task.mapError toText
      |> Task.asResult
  case res of
    Ok _ -> Task.yield unit
    Err err ->
      Task.throw err


mockNewOps :: Task Text (Internal.Ops eventType, NewObserve)
mockNewOps = do
  acquireCalls <- Var.new 0
  initializeTableCalls <- Var.new 0
  initializeSubscriptionsCalls <- Var.new 0

  let acquire :: Internal.Config -> Task Text Internal.Connection
      acquire _ = do
        Var.increment acquireCalls
        Task.yield Internal.MockConnection

  let initializeTable :: Internal.Connection -> Task Text Unit
      initializeTable _ = do
        Var.increment initializeTableCalls
        Task.yield unit

  let initializeSubscriptions :: Internal.SubscriptionStore eventType -> Internal.Connection -> Task Text Unit
      initializeSubscriptions _ _ = do
        Var.increment initializeSubscriptionsCalls
        Task.yield unit

  let newObserver = NewObserve {acquireCalls, initializeTableCalls, initializeSubscriptionsCalls}

  let ops = Internal.Ops {acquire, initializeTable, initializeSubscriptions}
  Task.yield (ops, newObserver)
