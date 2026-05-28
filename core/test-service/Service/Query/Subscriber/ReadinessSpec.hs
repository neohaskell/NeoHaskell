module Service.Query.Subscriber.ReadinessSpec where

import Core
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
import Service.QueryObjectStore.Postgres (PostgresQueryObjectStoreConfig (..), QueryObjectStoreError)
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
createTestStore :: Task QueryObjectStoreError (QueryObjectStore Unit)
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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { chunkSize = 1000 }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom chunk adherence: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom chunk adherence: not implemented — stub must fail"

    it "emits progress log message after each chunk completes" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { logProgress = True }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom progress log: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom progress log: not implemented — stub must fail"

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom hash mismatch delete: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom hash mismatch delete: not implemented — stub must fail"

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      let opts = rebuildOptionsDefault { logProgress = False }
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) opts
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom no-progress-log: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom no-progress-log: not implemented — stub must fail"

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom chunk-boundary tearing H6: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom chunk-boundary tearing H6: not implemented — stub must fail"

    it "resumes from checkpoint when startPosition > 0" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 500) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildFrom resume: not implemented — stub must fail"
        Err _ -> fail "rebuildFrom resume: not implemented — stub must fail"

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync readiness transitions: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync readiness transitions: not implemented — stub must fail"

    it "sets readiness to Failed if any query's rebuild times out" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault { timeout = 1 }
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync timeout -> Failed: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync timeout -> Failed: not implemented — stub must fail"

    it "sets readiness to Failed if any query's updater throws an exception" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync updater exception -> Failed: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync updater exception -> Failed: not implemented — stub must fail"

    it "continues rebuilding other queries even if one fails" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync failure isolation: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync failure isolation: not implemented — stub must fail"

    it "logs structured WARN message with query name and error when a query fails" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync WARN log: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync WARN log: not implemented — stub must fail"

    it "respects the per-query timeout configured in RebuildOptions" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault { timeout = 10 }
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync per-query timeout: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync per-query timeout: not implemented — stub must fail"

    it "can be cancelled via AsyncTask.cancel without leaving partial writes" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "rebuildAllAsync cancellation H8: not implemented — stub must fail"
        Err _ -> fail "rebuildAllAsync cancellation H8: not implemented — stub must fail"

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok Rebuilding -> pass
        Ok _ -> fail "readinessOf returned wrong state — expected Rebuilding"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns Failed with first failure reason when any query has failed" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok (Failed _) -> pass
        Ok _ -> fail "readinessOf returned wrong state — expected Failed"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

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
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> fail "readinessOf concurrent transitions: not implemented — stub must fail"
        Err _ -> fail "readinessOf concurrent transitions: not implemented — stub must fail"

  describe "readinessOfQuery" do
    it "returns Just Ready when the named query is caught up" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "orders"
          |> Task.asResult
      case result of
        Ok (Just Ready) -> pass
        Ok _ -> fail "readinessOfQuery returned wrong state — expected Just Ready"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns Just Rebuilding when the named query is still replaying" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "orders"
          |> Task.asResult
      case result of
        Ok (Just Rebuilding) -> pass
        Ok _ -> fail "readinessOfQuery returned wrong state — expected Just Rebuilding"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns Just (Failed reason) when the named query has failed" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "orders"
          |> Task.asResult
      case result of
        Ok (Just (Failed _)) -> pass
        Ok _ -> fail "readinessOfQuery returned wrong state — expected Just (Failed _)"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns Nothing when the named query is not registered" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "nonexistent"
          |> Task.asResult
      case result of
        Ok Nothing -> pass
        Ok _ -> fail "readinessOfQuery returned wrong state — expected Nothing"
        Err err -> fail [fmt|Expected success but got: #{toText (show err)}|]

    it "returns the correct readiness for each query in a multi-query system" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOfQuery subscriber "orders"
          |> Task.asResult
      case result of
        Ok _ -> fail "readinessOfQuery multi-query H7: not implemented — stub must fail"
        Err _ -> fail "readinessOfQuery multi-query H7: not implemented — stub must fail"

  describe "Concurrency Hazard H1: Replay racing live subscription" do
    it "handles simultaneous replay and live events without duplication" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h1-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H1 race test: not implemented — stub must fail"
        Err _ -> fail "H1 race test: not implemented — stub must fail"

  describe "Concurrency Hazard H2: Lost write via ON CONFLICT DO UPDATE" do
    it "prevents position regression via CAS-on-position" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H2 CAS property: not implemented — stub must fail"

    it "is not a last-writer-wins pattern (lower positions do not overwrite higher)" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H2 deterministic race: not implemented — stub must fail"

  describe "Concurrency Hazard H3: Crash mid-update" do
    it "restarts from persisted position after process crash pre-write" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H3 crash pre-write: not implemented — stub must fail"

    it "converges to correct state after crash mid-transaction" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H3 crash mid-txn: not implemented — stub must fail"

    it "does not re-apply events from after the persisted position on restart" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H3 idempotent restart: not implemented — stub must fail"

  describe "Concurrency Hazard H4: Readiness flag visibility" do
    it "never flips readiness to Ready before the last query_object_store write is durable" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> fail "H4 readiness durability: not implemented — stub must fail"
        Err _ -> fail "H4 readiness durability: not implemented — stub must fail"

    it "ensures writes are visible before any request lands on the ready query" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> fail "H4 stale read prevention: not implemented — stub must fail"
        Err _ -> fail "H4 stale read prevention: not implemented — stub must fail"

  describe "Concurrency Hazard H5: Hash-mismatch mid-flight" do
    it "deletes stale hash rows atomically before replay starts" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H5 atomic delete: not implemented — stub must fail"
        Err _ -> fail "H5 atomic delete: not implemented — stub must fail"

    it "replays affected query only; other queries unaffected" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H5 per-query isolation: not implemented — stub must fail"
        Err _ -> fail "H5 per-query isolation: not implemented — stub must fail"

    it "handles concurrent live events arriving during hash-mismatch deletion" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h5-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H5 concurrent live events: not implemented — stub must fail"
        Err _ -> fail "H5 concurrent live events: not implemented — stub must fail"

  describe "Concurrency Hazard H6: Chunk-boundary tearing" do
    it "produces identical state regardless of chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H6 chunk idempotency: not implemented — stub must fail"
        Err _ -> fail "H6 chunk idempotency: not implemented — stub must fail"

    it "does not lose events at chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H6 no event loss: not implemented — stub must fail"
        Err _ -> fail "H6 no event loss: not implemented — stub must fail"

    it "does not duplicate events at chunk boundaries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h6-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H6 no duplication: not implemented — stub must fail"
        Err _ -> fail "H6 no duplication: not implemented — stub must fail"

  describe "Concurrency Hazard H7: Init ordering" do
    it "registers a query after Application.run and requires full replay from position 0" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "late-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H7 late registration: not implemented — stub must fail"
        Err _ -> fail "H7 late registration: not implemented — stub must fail"

    it "does not silently drop events for unregistered queries" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h7-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H7 no silent drops: not implemented — stub must fail"
        Err _ -> fail "H7 no silent drops: not implemented — stub must fail"

  describe "Concurrency Hazard H8: AsyncTask cancellation on shutdown" do
    it "persists position consistently when rebuild is cancelled via SIGTERM" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildAllAsync subscriber rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H8 SIGTERM safety: not implemented — stub must fail"
        Err _ -> fail "H8 SIGTERM safety: not implemented — stub must fail"

    it "resumes correctly after SIGTERM-interrupted rebuild" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "h8-query" (StreamPosition 500) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "H8 resume after SIGTERM: not implemented — stub must fail"
        Err _ -> fail "H8 resume after SIGTERM: not implemented — stub must fail"

  describe "Concurrency Hazard H9: Multi-writer (future-proofing)" do
    it "rejects stale writes when multiple processes attempt concurrent updates" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H9 multi-writer CAS: not implemented — stub must fail"

    it "accommodates the contract for future multi-writer implementations" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "H9 multi-writer isolation: not implemented — stub must fail"

  describe "Property-based invariants" do
    it "position never decreases for any (query_name, instance_uuid) — atomicUpdate position monotonicity" \_ -> do
      result <-
        createTestStore
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Setup failed: #{toText (show err)}|]
        Ok _ -> fail "property: position monotonicity: not implemented — stub must fail"

    it "replay(E) == replay(replay(E)) for any event sequence E — rebuildFrom idempotency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "idempotent-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "property: rebuildFrom idempotency: not implemented — stub must fail"
        Err _ -> fail "property: rebuildFrom idempotency: not implemented — stub must fail"

    it "state is invariant to chunk boundary placement — chunk-boundary transparency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "boundary-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Ok _ -> fail "property: chunk-boundary transparency: not implemented — stub must fail"
        Err _ -> fail "property: chunk-boundary transparency: not implemented — stub must fail"

    it "readinessOf == Ready iff all readinessOfQuery return Ready — readiness aggregate consistency" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      subscriber <- Subscriber.new eventStore Registry.empty
      result <-
        Subscriber.readinessOf subscriber
          |> Task.asResult
      case result of
        Ok _ -> fail "property: readiness aggregate: not implemented — stub must fail"
        Err _ -> fail "property: readiness aggregate: not implemented — stub must fail"

  describe "Round-trip serialization" do
    it "Readiness round-trips correctly" \_ -> do
      -- toJSON >> fromJSON should return the same Readiness value.
      -- Stub: types exist but serialization is not implemented yet.
      fail "Readiness round-trip: not implemented — stub must fail"

    it "RebuildOptions round-trips correctly" \_ -> do
      fail "RebuildOptions round-trip: not implemented — stub must fail"

    it "QueryObjectStoreError round-trips correctly" \_ -> do
      fail "QueryObjectStoreError round-trip: not implemented — stub must fail"

    it "QueryRebuildError round-trips correctly" \_ -> do
      fail "QueryRebuildError round-trip: not implemented — stub must fail"
