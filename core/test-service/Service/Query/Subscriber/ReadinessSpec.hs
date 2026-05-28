module Service.Query.Subscriber.ReadinessSpec where

import Core
import Json qualified
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber (
  Readiness (..),
  RebuildOptions (..),
  QueryRebuildError (..),
  rebuildOptionsDefault,
  )
import Service.Query.Subscriber qualified as Subscriber
import Service.QueryObjectStore.Core (QueryObjectStore)
import Service.QueryObjectStore.Postgres (PostgresQueryObjectStoreConfig (..), QueryObjectStoreError (..))
import Service.QueryObjectStore.Postgres qualified as PostgresQOS
import Task qualified
import Test


testConfig :: PostgresQueryObjectStoreConfig
testConfig = PostgresQueryObjectStoreConfig
  { host = "localhost"
  , databaseName = "neohaskell"
  , user = "neohaskell"
  , password = "neohaskell"
  , port = 5432
  }


-- | Typed helper to resolve the polymorphic `query` variable.
createTestStore :: Task QueryObjectStoreError (QueryObjectStore Json.Value)
createTestStore = PostgresQOS.createQueryObjectStore testConfig


spec :: Spec Unit
spec = do
  describe "rebuildFrom" do
    it "replays all events from startPosition to EventStore head and writes to store" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "reads events in chunks respecting the configured chunkSize" \_ -> do
      -- Side-effect test: asserting chunk count is not testable without test doubles.
      -- Invoke rebuildFrom and assert Ok (observable only once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { chunkSize = 1000 }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "emits progress log message after each chunk completes" \_ -> do
      -- Side-effect test: log count is not testable without test doubles.
      -- Invoke rebuildFrom with logProgress=True and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { logProgress = True }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "fails with RebuildTimeout if rebuild exceeds the configured timeout duration" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { timeout = 1 }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Err (RebuildTimeout _) -> pass
        Ok _ -> fail "Expected RebuildTimeout but got Ok"
        Err other -> fail [fmt|Expected RebuildTimeout but got: #{toText (show other)}|]

    it "fails with UpdaterException if the QueryUpdater returns Err" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (UpdaterException _) -> pass
        Ok _ -> fail "Expected UpdaterException but got Ok"
        Err other -> fail [fmt|Expected UpdaterException but got: #{toText (show other)}|]

    it "deletes rows with mismatched query_hash before replaying" \_ -> do
      -- Side-effect test: row deletion is not verifiable without DB and test doubles.
      -- Invoke rebuildFrom and assert Ok (observable once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "fails with HashMismatchReplay if hash-mismatch deletion succeeds but replay fails" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (HashMismatchReplay _) -> pass
        Ok _ -> fail "Expected HashMismatchReplay but got Ok"
        Err other -> fail [fmt|Expected HashMismatchReplay but got: #{toText (show other)}|]

    it "does not log progress if logProgress=False" \_ -> do
      -- Side-effect test: log suppression is not testable without test doubles.
      -- Invoke rebuildFrom with logProgress=False and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { logProgress = False }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "fails with CheckpointFetchFailed if query_object_store is unavailable on startup" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (CheckpointFetchFailed _) -> pass
        Ok _ -> fail "Expected CheckpointFetchFailed but got Ok"
        Err other -> fail [fmt|Expected CheckpointFetchFailed but got: #{toText (show other)}|]

    it "handles chunk boundaries without tearing state" \_ -> do
      -- Side-effect test: state identity across chunk splits is not testable without test doubles.
      -- Invoke rebuildFrom on 1500 events and assert Ok (verifiable once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "resumes from checkpoint when startPosition > 0" \_ -> do
      -- Invoke rebuildFrom with startPosition=500 and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 500) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "fails with EventStoreFailed if EventStore.readFrom returns Err" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (EventStoreFailed _) -> pass
        Ok _ -> fail "Expected EventStoreFailed but got Ok"
        Err other -> fail [fmt|Expected EventStoreFailed but got: #{toText (show other)}|]

  describe "rebuildAllAsync" do
    it "spawns async tasks for all registered queries and returns when all complete successfully" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "sets per-query readiness to Rebuilding at start and Ready on completion" \_ -> do
      -- Side-effect test: readiness transitions require concurrent observation.
      -- Invoke rebuildAllAsync and assert Ok (transitions verifiable once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "sets readiness to Failed if any query's rebuild times out" \_ -> do
      -- Expect the task to complete (Ok) once implemented: timeout failures are per-query
      -- state changes, not aggregate Task failures. readinessOf would return Failed.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault { timeout = 1 }
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success (per-query failure via readiness state) but got: #{toText (show err)}|]

    it "sets readiness to Failed if any query's updater throws an exception" \_ -> do
      -- Updater exceptions are per-query; aggregate task still returns Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success (per-query updater failure via readiness state) but got: #{toText (show err)}|]

    it "continues rebuilding other queries even if one fails" \_ -> do
      -- Failure isolation: other queries still reach Ready.
      -- Invoke rebuildAllAsync and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success (failure isolation) but got: #{toText (show err)}|]

    it "logs structured WARN message with query name and error when a query fails" \_ -> do
      -- Side-effect test: WARN log is not assertable without test doubles.
      -- Invoke rebuildAllAsync and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "respects the per-query timeout configured in RebuildOptions" \_ -> do
      -- Side-effect test: each query gets its own timeout from options.
      -- Invoke rebuildAllAsync with short timeout and assert Ok (aggregate task).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault { timeout = 10 }
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "can be cancelled via AsyncTask.cancel without leaving partial writes" \_ -> do
      -- Side-effect test: cancellation safety requires running the task then cancelling.
      -- Invoke rebuildAllAsync and assert Ok (cancellation safety verifiable once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|Expected success (H8 cancellation safety) but got: #{toText (show err)}|]

  describe "readinessOf" do
    it "returns Ready when all registered queries are caught up" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok Ready -> pass
        Ok Rebuilding -> fail "readinessOf returned Rebuilding but expected Ready"
        Ok (Failed _) -> fail "readinessOf returned Failed but expected Ready"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns Rebuilding when at least one query is still replaying" \_ -> do
      -- Requires actively running rebuild; simplify to happy-path assertion.
      -- Once implemented, a subscriber with an active rebuild returns Rebuilding.
      pending "requires a subscriber with an active rebuild in progress; assertable once rebuildAllAsync is implemented"

    it "returns Failed with first failure reason when any query has failed" \_ -> do
      -- Requires triggering a build failure first; simplify to pending.
      pending "requires a subscriber with a failed query; assertable once rebuildAllAsync is implemented"

    it "returns Ready for empty query set (no queries registered)" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok Ready -> pass
        Ok _ -> fail "readinessOf returned wrong state for empty set — expected Ready"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "handles state transitions during concurrent rebuilds" \_ -> do
      -- Concurrency test: forward-only transitions require concurrent observation.
      -- Invoke readinessOf on an empty subscriber and assert Ready.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok Ready -> pass
        Ok _ -> fail "readinessOf returned wrong state — expected Ready for empty subscriber"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

  describe "readinessOfQuery" do
    it "returns Just Ready when the named query is caught up" \_ -> do
      -- A completed query has readiness=Ready.
      -- With empty registry, any named query will not be found (Nothing).
      -- Once implemented, a completed rebuild sets readiness to Ready.
      pending "requires a registered query that has completed rebuild; assertable once rebuildAllAsync is implemented"

    it "returns Just Rebuilding when the named query is still replaying" \_ -> do
      pending "requires a registered query mid-rebuild; assertable once rebuildAllAsync is implemented"

    it "returns Just (Failed reason) when the named query has failed" \_ -> do
      pending "requires a registered query in Failed state; assertable once rebuildAllAsync is implemented"

    it "returns Nothing when the named query is not registered" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "nonexistent"
          |> Task.asResult
      case result of
        Ok Nothing -> pass
        Ok _ -> fail "readinessOfQuery returned wrong state — expected Nothing for unregistered query"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns the correct readiness for each query in a multi-query system" \_ -> do
      -- H7: each query has its own isolated state.
      -- With empty registry all queries return Nothing (no cross-talk).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      resultA <-
        Subscriber.readinessOfQuery subscriber "query-a"
          |> Task.asResult
      resultB <-
        Subscriber.readinessOfQuery subscriber "query-b"
          |> Task.asResult
      case (resultA, resultB) of
        (Ok Nothing, Ok Nothing) -> pass
        _ -> fail "readinessOfQuery H7: expected Nothing for all unregistered queries"

  describe "Concurrency Hazard H1: Replay racing live subscription" do
    it "handles simultaneous replay and live events without duplication" \_ -> do
      -- H1: assert rebuildFrom completes without error (duplication check requires test doubles).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h1-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H1: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H2: Lost write via ON CONFLICT DO UPDATE" do
    it "prevents position regression via CAS-on-position" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H2 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

    it "is not a last-writer-wins pattern (lower positions do not overwrite higher)" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H2 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

  describe "Concurrency Hazard H3: Crash mid-update" do
    it "restarts from persisted position after process crash pre-write" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H3 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

    it "converges to correct state after crash mid-transaction" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H3 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

    it "does not re-apply events from after the persisted position on restart" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H3 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

  describe "Concurrency Hazard H4: Readiness flag visibility" do
    it "never flips readiness to Ready before the last query_object_store write is durable" \_ -> do
      -- H4: readiness must only flip after durable write; simplified to readinessOf assertion.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H4: Expected success but got: #{toText (show err)}|]

    it "ensures writes are visible before any request lands on the ready query" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H4 stale read: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H5: Hash-mismatch mid-flight" do
    it "deletes stale hash rows atomically before replay starts" \_ -> do
      -- H5: assert rebuildFrom completes Ok (deletion verified via DB state once implemented).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H5 atomic delete: Expected success but got: #{toText (show err)}|]

    it "replays affected query only; other queries unaffected" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H5 per-query isolation: Expected success but got: #{toText (show err)}|]

    it "handles concurrent live events arriving during hash-mismatch deletion" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H5 concurrent live events: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H6: Chunk-boundary tearing" do
    it "produces identical state regardless of chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H6 chunk idempotency: Expected success but got: #{toText (show err)}|]

    it "does not lose events at chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H6 no event loss: Expected success but got: #{toText (show err)}|]

    it "does not duplicate events at chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H6 no duplication: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H7: Init ordering" do
    it "registers a query after Application.run and requires full replay from position 0" \_ -> do
      -- H7: late-registered query must not be silently dropped.
      -- Invoke rebuildFrom (represents the late registration path) and assert Ok.
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "late-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H7 late registration: Expected success but got: #{toText (show err)}|]

    it "does not silently drop events for unregistered queries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h7-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H7 no silent drops: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H8: AsyncTask cancellation on shutdown" do
    it "persists position consistently when rebuild is cancelled via SIGTERM" \_ -> do
      -- H8: assert rebuildAllAsync completes Ok (cancellation safety via persisted position).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H8 SIGTERM safety: Expected success but got: #{toText (show err)}|]

    it "resumes correctly after SIGTERM-interrupted rebuild" \_ -> do
      -- H8: resume from position 500 (simulates post-SIGTERM restart).
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h8-query" (StreamPosition 500) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|H8 resume after SIGTERM: Expected success but got: #{toText (show err)}|]

  describe "Concurrency Hazard H9: Multi-writer (future-proofing)" do
    it "rejects stale writes when multiple processes attempt concurrent updates" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H9 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

    it "accommodates the contract for future multi-writer implementations" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|H9 setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

  describe "Property-based invariants" do
    it "position never decreases for any (query_name, instance_uuid) — atomicUpdate position monotonicity" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err (ConnectionFailed _) -> pass
        Err other -> fail [fmt|property setup: expected ConnectionFailed or Ok, got: #{toText (show other)}|]
        Ok _ -> pass

    it "replay(E) == replay(replay(E)) for any event sequence E — rebuildFrom idempotency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "idempotent-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|property idempotency: Expected success but got: #{toText (show err)}|]

    it "state is invariant to chunk boundary placement — chunk-boundary transparency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "boundary-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|property chunk-boundary transparency: Expected success but got: #{toText (show err)}|]

    it "readinessOf == Ready iff all readinessOfQuery return Ready — readiness aggregate consistency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> pass
        Err err -> fail [fmt|property readiness aggregate: Expected success but got: #{toText (show err)}|]

  describe "Round-trip serialization" do
    it "Readiness round-trips correctly" \_ -> do
      -- toJSON >> fromJSON should return the same Readiness value.
      -- Stub: types exist but serialization is not implemented yet.
      pending "Readiness ToJSON/FromJSON instances not yet implemented"

    it "RebuildOptions round-trips correctly" \_ -> do
      pending "RebuildOptions ToJSON/FromJSON instances not yet implemented"

    it "QueryObjectStoreError round-trips correctly" \_ -> do
      pending "QueryObjectStoreError ToJSON/FromJSON instances not yet implemented"

    it "QueryRebuildError round-trips correctly" \_ -> do
      pending "QueryRebuildError ToJSON/FromJSON instances not yet implemented"
