module Service.EventStore.PostgresSpec where

import Core
import Service.EventStore.Postgres qualified as Postgres
import Service.EventStore.Postgres.Internal qualified as Internal
import Service.EventStore.Postgres.Core qualified as PostgresCore
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Task qualified
import Test
import Test.Service.CommandHandler qualified as CommandHandler
import Test.Service.EntityFetcher qualified as EntityFetcherSpec
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStore
import Test.Service.EventStore.Core (CartEvent)
import Var qualified


spec :: Spec Unit
spec = do
  describe "PostgresEventStore" do
    let config =
          Postgres.PostgresEventStore
            { host = "localhost",
              databaseName = "neohaskell",
              user = "neohaskell",
              password = "neohaskell",
              port = 5432
            }
    describe "new method" do
      it "acquires the connection" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new @CartEvent ops config
          |> Task.mapError toText
          |> discard
        observe.acquireCalls
          |> varContents shouldBe 1

      it "initializes the table" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new @CartEvent ops config
          |> Task.mapError toText
          |> discard
        observe.initializeTableCalls
          |> varContents shouldBe 1

      it "initializes the subscriptions" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new @CartEvent ops config
          |> Task.mapError toText
          |> discard
        observe.initializeSubscriptionsCalls
          |> varContents shouldBe 1

    let newStore = do
          let ops = Internal.defaultOps @CartEvent
          dropPostgres ops config
          Postgres.new config |> Task.mapError toText
    EventStore.spec newStore

    let newStoreAndFetcher = do
          store <- newStore
          fetcher <- EntityFetcherCore.newFetcher store |> Task.mapError toText
          Task.yield (store, fetcher)
    EntityFetcherSpec.spec newStoreAndFetcher

    let newCartStore = do
          let ops = Internal.defaultOps @CartEvent
          dropPostgres ops config
          Postgres.new config |> Task.mapError toText
    CommandHandler.spec newCartStore


data NewObserve = NewObserve
  { acquireCalls :: Var Int,
    initializeTableCalls :: Var Int,
    initializeSubscriptionsCalls :: Var Int
  }


dropPostgres :: Internal.Ops eventType -> Postgres.PostgresEventStore -> Task Text Unit
dropPostgres ops config =
  ops
    |> Internal.withConnection
      config
      ( \connection -> do
          res <-
            Sessions.dropEventsTableSession
              |> Sessions.run connection
              |> Task.asResult
          case res of
            Ok _ -> Task.yield unit
            Err err ->
              Task.throw (err |> PostgresCore.SessionError)
      )
    |> Task.mapError (toText)


mockNewOps :: Task Text (Internal.Ops eventType, NewObserve)
mockNewOps = do
  acquireCalls <- Var.new 0
  initializeTableCalls <- Var.new 0
  initializeSubscriptionsCalls <- Var.new 0
  releaseCalls <- Var.new (0 :: Int)

  let acquire :: Internal.PostgresEventStore -> Task Text Internal.Connection
      acquire _ = do
        Var.increment acquireCalls
        Task.yield Internal.MockConnection

  let initializeTable :: Internal.Connection -> Task Text Unit
      initializeTable _ = do
        Var.increment initializeTableCalls
        Task.yield unit

  let initializeSubscriptions :: Internal.SubscriptionStore eventType -> Internal.PostgresEventStore -> Task Text Unit
      initializeSubscriptions _ _ = do
        Var.increment initializeSubscriptionsCalls
        Task.yield unit

  let release :: Internal.Connection -> Task Text Unit
      release _ = do
        Var.increment releaseCalls
        Task.yield unit

  let newObserver = NewObserve {acquireCalls, initializeTableCalls, initializeSubscriptionsCalls}

  let ops = Internal.Ops {acquire, initializeTable, initializeSubscriptions, release}
  Task.yield (ops, newObserver)
