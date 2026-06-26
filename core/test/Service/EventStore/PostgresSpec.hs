module Service.EventStore.PostgresSpec where

import Core
import Service.EventStore.Core qualified as EventStore
import AsyncTask qualified
import Service.EventStore.Postgres qualified as Postgres
import Service.EventStore.Postgres.Internal qualified as Internal
import Service.EventStore.Postgres.Core qualified as PostgresCore
import Service.EventStore.Postgres.Sessions qualified as Sessions
import Task qualified
import Test
import Test.Service.CommandHandler qualified as CommandHandler
import Test.Service.EntityFetcher qualified as EntityFetcherSpec
import Test.Service.EntityFetcher.Core qualified as EntityFetcherCore
import Test.Service.EventStore qualified as EventStoreSpec
import Test.Service.EventStore.Core (CartEvent)
import Var qualified
import Service.Event (EntityName (..), Event)
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Postgres.Internal (toConnectionSettings)
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Result qualified
import Text qualified
import Bytes qualified
import Hasql.Connection qualified as Hasql
import Hasql.Session qualified as Session
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Statement (Statement)
import Hasql.Statement qualified as HasqlStatement


spec :: Spec Unit
spec = do
  describe "PostgresEventStore" do
    let config =
          Internal.PostgresEventStore
            { Internal.host = "localhost",
              Internal.databaseName = "neohaskell",
              Internal.user = "neohaskell",
              Internal.password = "neohaskell",
              Internal.port = 5432,
              Internal.poolSize = 6,
              Internal.sslMode = ConnectionConfig.SslModeUnset,
              Internal.sslRootCert = Nothing
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

      it "initializes the subscriptions" \_ -> do
        (ops, observe) <- mockNewOps
        Internal.new ops config
          |> Task.mapError toText
          |> discard
        observe.initializeSubscriptionsCalls
          |> varContents shouldBe 1

    let newStore = do
          let ops = Internal.defaultOps
          prevStore <- Postgres.new config |> Task.mapError toText
          prevStore.close |> discard
          dropPostgres ops config
          Postgres.new config
            |> Task.map (EventStore.castEventStore @CartEvent)
            |> Task.mapError toText
    EventStoreSpec.spec newStore

    let newStoreAndFetcher = do
          store <- newStore
          fetcher <- EntityFetcherCore.newFetcher store |> Task.mapError toText
          Task.yield (store, fetcher)
    EntityFetcherSpec.spec newStoreAndFetcher

    let newCartStore = do
          let ops = Internal.defaultOps
          prevStore <- Postgres.new config |> Task.mapError toText
          prevStore.close |> discard
          dropPostgres ops config
          Postgres.new config
            |> Task.map (EventStore.castEventStore @CartEvent)
            |> Task.mapError toText
    CommandHandler.spec newCartStore

    describe "per-stream connection release (ADR-0063)" do
      whenEnvVar "POSTGRES_AVAILABLE" do
        it "releases the dedicated connection across N stream subscribe/unsubscribe cycles (no leak)" \_ -> do
          -- Regression for issue #683: each subscribeToStreamEvents opens a
          -- dedicated unpooled connection; unsubscribe must release it. With the
          -- leak, the backend connection count climbs by N across N cycles.
          store <-
            Postgres.new config
              |> Task.map (EventStore.castEventStore @CartEvent)
              |> Task.mapError toText

          -- WI-2 made 'toConnectionSettings' return a 'Result' (port validation).
          -- Surface any settings error into this spec's Text error channel before
          -- acquiring the admin connection.
          adminSettings <-
            case toConnectionSettings config of
              Ok settings -> Task.yield settings
              Err err -> Task.throw err
          adminConn <-
            Hasql.acquire adminSettings
              |> Task.fromIOEither
              |> Task.mapError toText

          -- Guaranteed teardown: release the admin connection and close the store
          -- whether the body succeeds, throws, or the final assertion fails.
          -- Otherwise a failure here leaks connections into later Postgres-gated
          -- specs and contaminates them.
          let cleanup = do
                Hasql.release adminConn |> Task.fromIO
                store.close |> Task.mapError toText |> Task.ignoreError

          Task.finally cleanup do
            let entityName = EntityName "ConnReleaseRegressionEntity"
            let callback _event = Task.yield unit

            -- Warm-up: one subscribe/unsubscribe cycle forces the Hasql pool and
            -- the store's listener connections to fully establish, so the baseline
            -- reflects steady state rather than a lazily-growing pool. (The pooled
            -- connection borrowed by withConnectionAndError is what would otherwise
            -- inflate the post-loop count and mask the per-stream release.)
            subscribeUnsubscribeCycles store entityName callback 1
            -- Poll until the backend count settles instead of a fixed sleep, so
            -- the baseline reflects steady state regardless of teardown timing.
            baseline <- pollSettledBackends adminConn Nothing

            -- Now run N more cycles. Each opens ONE dedicated per-stream connection
            -- that unsubscribe must release; with the leak the count would climb by
            -- N. With the fix the dedicated connection is gone each cycle, so the
            -- steady-state count does not grow.
            let cycleN = 5 :: Int
            subscribeUnsubscribeCycles store entityName callback cycleN

            -- Poll until the count settles to at-or-below the baseline (or a bounded
            -- timeout expires), so pg_stat_activity reflects the per-stream releases.
            afterCycles <- pollSettledBackends adminConn (Just baseline)

            -- No net growth across the N measured cycles: a leak would leave
            -- baseline + cycleN dedicated connections; the fix keeps it at baseline.
            (afterCycles <= baseline) |> shouldBe True



data NewObserve = NewObserve
  { acquireCalls :: Var Int,
    initializeTableCalls :: Var Int,
    initializeSubscriptionsCalls :: Var Int
  }


dropPostgres :: Internal.Ops -> Postgres.PostgresEventStore -> Task Text Unit
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


mockNewOps :: Task Text (Internal.Ops, NewObserve)
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

  let initializeSubscriptions :: Internal.Connection -> Internal.SubscriptionStore -> Internal.PostgresEventStore -> Task Text (Task Text Unit)
      initializeSubscriptions _ _ _ = do
        Var.increment initializeSubscriptionsCalls
        Task.yield (Task.yield unit)

  let release :: Internal.Connection -> Task Text Unit
      release _ = do
        Var.increment releaseCalls
        Task.yield unit

  let newObserver = NewObserve {acquireCalls, initializeTableCalls, initializeSubscriptionsCalls}

  let ops = Internal.Ops {acquire, initializeTable, initializeSubscriptions, release}
  Task.yield (ops, newObserver)


-- | Run one subscribe/unsubscribe cycle @n@ times against the same stream id.
-- Each subscribe opens a dedicated per-stream connection that unsubscribe must
-- release; after @n@ cycles no dedicated connection should survive.
subscribeUnsubscribeCycles ::
  EventStore.EventStore CartEvent -> EntityName -> (Event CartEvent -> Task Text Unit) -> Int -> Task Text Unit
subscribeUnsubscribeCycles store entityName callback n =
  case n <= 0 of
    True -> Task.yield unit
    False -> do
      streamId <- StreamId.new
      subId <- store.subscribeToStreamEvents entityName streamId callback |> Task.mapError toText
      store.unsubscribe subId |> Task.mapError toText
      subscribeUnsubscribeCycles store entityName callback (n - 1)


-- | Count the backends currently open for the test database user, scoped to the
-- current database. The dedicated per-stream connections are opened by the same
-- user against the same database, so a leak shows up as a higher count. Scoping
-- to @current_database()@ keeps unrelated sessions for the same user on other
-- databases from inflating the count.
countUserBackends :: Hasql.Connection -> Task Text Int64
countUserBackends conn = do
  let query :: Text = "SELECT count(*) :: int8 FROM pg_stat_activity WHERE usename = current_user AND datname = current_database()"
  let decoder = Decoders.singleRow (Decoders.column (Decoders.nonNullable Decoders.int8))
  let statement :: Statement Unit Int64 =
        HasqlStatement.Statement (query |> Text.toBytes |> Bytes.unwrap) Encoders.noParams decoder True
  result <- Session.run (Session.statement unit statement) conn |> Task.fromIO |> Task.map Result.fromEither
  case result of
    Err err -> Task.throw [fmt|pg_stat_activity count failed: #{err}|]
    Ok count -> Task.yield count


-- | Poll 'countUserBackends' until it settles, replacing a fixed sleep so the
-- test is deterministic regardless of how long libpq teardown takes.
--
-- "Settled" means two consecutive samples (50 ms apart) are equal AND, when a
-- @target@ baseline is supplied, the count is at or below it. Polling stops as
-- soon as it settles, or after a bounded number of attempts (≈5 s), returning
-- the last sample either way so the caller's assertion still runs.
pollSettledBackends :: Hasql.Connection -> Maybe Int64 -> Task Text Int64
pollSettledBackends conn target = do
  let maxAttempts = 100 :: Int
  let stepMs = 50
  let atOrBelowTarget count =
        case target of
          Nothing -> True
          Just baseline -> count <= baseline
  let loop attempts previous = do
        current <- countUserBackends conn
        let settled = case previous of
              Just prev -> current == prev && atOrBelowTarget current
              Nothing -> False
        case settled || attempts >= maxAttempts of
          True -> Task.yield current
          False -> do
            AsyncTask.sleep stepMs |> Task.mapError (\_ -> "poll settle sleep failed")
            loop (attempts + 1) (Just current)
  loop 0 Nothing
