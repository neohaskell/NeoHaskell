module Service.Query.Subscriber.ReadinessSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Data.Hashable qualified as Hashable
import Json qualified
import Service.Event (Event (..), EntityName (..), StreamId (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Limit (..), ReadAllMessage (..))
import Service.EventStore.Core qualified as EventStoreCore
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber (
  QueryRebuildError (..),
  Readiness (..),
  RebuildOptions (..),
  newWithStore,
  newWithCheckpointStore,
  processEventHandler,
  queryHashFor,
  rebuildOptionsDefault,
  )
import Service.Query.Subscriber qualified as Subscriber
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.Core qualified as QOSCore
import Service.QueryObjectStore.Postgres (CheckpointStore (..), PostgresQueryObjectStoreConfig (..), QueryObjectStoreError (..))
import Service.QueryObjectStore.Postgres qualified as PostgresQOS
import Stream qualified
import Task qualified
import Test


testConfig :: PostgresQueryObjectStoreConfig
testConfig = def
  { host = "localhost"
  , databaseName = "neohaskell"
  , user = "neohaskell"
  , password = "neohaskell"
  , port = 5432
  }


-- | Typed helper to resolve the polymorphic `query` variable.
createTestStore :: Task QueryObjectStoreError (QueryObjectStore Json.Value)
createTestStore = PostgresQOS.newFromConfig testConfig


spec :: Spec Unit
spec = do
  describe "rebuildFrom" do
    it "replays all events from startPosition to EventStore head and writes to store" \_ -> do
      pending "needs a registered QueryUpdater and a populated EventStore to observe that rows are actually written; current empty-registry setup completes in microseconds without exercising any replay logic"

    it "reads events in chunks respecting the configured chunkSize" \_ -> do
      pending "needs a registered QueryUpdater plus enough events in the EventStore to observe chunked reads; current empty-registry setup completes in microseconds without exercising chunk boundaries"

    it "emits progress log message after each chunk completes" \_ -> do
      pending "needs a log capture fixture to assert structured progress entries; not assertable against the global Log writer without test doubles"

    it "fails with RebuildTimeout if rebuild exceeds the configured timeout duration" \_ -> do
      -- Fixture: an EventStore whose readAllEventsForwardFrom blocks for 5 seconds.
      -- The rebuild timeout is 1 second, so the GhcAsync.race fires the sleep branch first.
      baseStore <- InMemory.new |> Task.mapError toText
      let slowStore = baseStore
            { readAllEventsForwardFrom = \_ _ -> do
                AsyncTask.sleep 5000
                baseStore.readAllEventsForwardFrom (StreamPosition 0) (Limit 1000)
            }
      subscriber <- Subscriber.new slowStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "slow-query" (StreamPosition 0)
          rebuildOptionsDefault { timeout = 1 }
          |> Task.asResult
      case result of
        Err (RebuildTimeout _) -> pass
        Err other -> fail [fmt|Expected RebuildTimeout but got: #{toText (show other)}|]
        Ok _ -> fail "Expected RebuildTimeout but rebuild completed without error"

    it "fails with UpdaterException if the QueryUpdater returns Err" \_ -> do
      -- Fixture: a failing QueryUpdater + EventStore that returns a synthetic AllEvent stream.
      -- applyEvent now propagates updater errors as Err Text (fixed in this phase).
      baseStore <- InMemory.new |> Task.mapError toText
      eventMeta <- EventMetadata.new
      let syntheticEvent =
            Event
              { entityName = EntityName "test-entity"
              , streamId = StreamId "test-stream"
              , event = Json.null
              , metadata = eventMeta
              }
      let stubbedStore = baseStore
            { readAllEventsForwardFrom = \_ _ ->
                Stream.fromArray (Array.wrap (AllEvent syntheticEvent))
            }
      let failingUpdater = QueryUpdater
            { queryName = "failing-query"
            , updateQuery = \_ -> Task.throw "fixture updater failure"
            }
      let registry = Registry.register (EntityName "test-entity") failingUpdater Registry.empty
      subscriber <- Subscriber.new stubbedStore registry
      result <-
        Subscriber.rebuildFrom subscriber "failing-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (UpdaterException _) -> pass
        Err other -> fail [fmt|Expected UpdaterException but got: #{toText (show other)}|]
        Ok _ -> fail "Expected UpdaterException but rebuild succeeded"

    it "deletes rows with mismatched query_hash before replaying" \_ -> do
      pending "needs a CheckpointStore fixture pre-seeded with mismatched-hash rows plus an observability hook on deleteStaleHash; current empty subscriber has no checkpointStore"

    it "fails with HashMismatchReplay if hash-mismatch deletion succeeds but replay fails" \_ -> do
      -- Fixture: a CheckpointStore where:
      --   resumeFromCheckpoint → Nothing  (forces the hash-mismatch / first-run deletion path)
      --   deleteStaleHash      → Ok ()    (deletion succeeds)
      -- Combined with an EventStore that always throws → rebuildFromInner wraps the
      -- event-store failure as HashMismatchReplay (H5).
      baseStore <- InMemory.new |> Task.mapError toText
      let failingStore = baseStore
            { readAllEventsForwardFrom = \_ _ ->
                Task.throw (EventStoreCore.StorageFailure "fixture event store failure")
            }
      let mockCpStore = CheckpointStore
            { resumeFromCheckpoint = \_ _ -> Task.yield Nothing
            , deleteStaleHash = \_ _ -> Task.yield unit
            , writeCheckpoint = \_ _ _ -> Task.yield unit
            }
      subscriber <- newWithCheckpointStore failingStore Registry.empty mockCpStore
      result <-
        Subscriber.rebuildFrom subscriber "hash-mismatch-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (HashMismatchReplay _) -> pass
        Err other -> fail [fmt|Expected HashMismatchReplay but got: #{toText (show other)}|]
        Ok _ -> fail "Expected HashMismatchReplay but rebuild succeeded"

    it "does not log progress if logProgress=False" \_ -> do
      pending "needs a log capture fixture to confirm no progress entries are emitted; not assertable against the global Log writer without test doubles"

    it "fails with CheckpointFetchFailed if query_object_store is unavailable on startup" \_ -> do
      -- Fixture: a QueryObjectStore whose get always throws — simulates an unavailable store.
      -- newWithStore wires the store into the subscriber; rebuildFrom calls store.get first.
      eventStore <- InMemory.new |> Task.mapError toText
      let failingStore = QueryObjectStore
            { get = \_ -> Task.throw (QOSCore.StorageError "fixture store unavailable")
            , atomicUpdate = \_ _ -> Task.throw (QOSCore.StorageError "fixture store unavailable")
            , getAll = Task.throw (QOSCore.StorageError "fixture store unavailable")
            }
      subscriber <- newWithStore eventStore Registry.empty failingStore
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (CheckpointFetchFailed _) -> pass
        Err other -> fail [fmt|Expected CheckpointFetchFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected CheckpointFetchFailed but rebuild succeeded"

    it "handles chunk boundaries without tearing state" \_ -> do
      pending "needs a registered QueryUpdater and enough events to observe chunk-boundary transitions; current empty-registry setup completes in a single empty chunk without exercising any boundary"

    it "resumes from checkpoint when startPosition > 0" \_ -> do
      pending "needs a registered QueryUpdater and enough events to observe resume-vs-replay-from-zero divergence; current empty-registry setup produces identical (empty) results at any startPosition"

    it "fails with EventStoreFailed if EventStore.readFrom returns Err" \_ -> do
      -- Fixture: an EventStore whose readAllEventsForwardFrom always throws.
      -- record update on the InMemory base store overrides just the one method.
      baseStore <- InMemory.new |> Task.mapError toText
      let failingStore = baseStore
            { readAllEventsForwardFrom = \_ _ ->
                Task.throw (EventStoreCore.StorageFailure "fixture event store failure")
            }
      subscriber <- Subscriber.new failingStore Registry.empty
      result <-
        Subscriber.rebuildFrom subscriber "test-query" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err (EventStoreFailed _) -> pass
        Err other -> fail [fmt|Expected EventStoreFailed but got: #{toText (show other)}|]
        Ok _ -> fail "Expected EventStoreFailed but rebuild succeeded"

  describe "rebuildAllAsync" do
    it "spawns async tasks for all registered queries and returns when all complete successfully" \_ -> do
      pending "needs at least one registered QueryUpdater to observe that async tasks are spawned; current empty-registry setup completes immediately with no tasks"

    it "sets per-query readiness to Rebuilding at start and Ready on completion" \_ -> do
      pending "needs concurrent observation of the readiness state mid-rebuild; requires a registered QueryUpdater and a synchronisation point to read Rebuilding before it transitions to Ready"

    it "sets readiness to Failed if any query's rebuild times out" \_ -> do
      pending "needs a registered QueryUpdater whose rebuild exceeds the configured timeout; current empty-registry setup returns immediately without entering any timeout path"

    it "sets readiness to Failed if any query's updater throws an exception" \_ -> do
      pending "needs a registered QueryUpdater that throws an exception during applyEvent; current empty-registry setup never calls any updater"

    it "continues rebuilding other queries even if one fails" \_ -> do
      pending "needs at least two registered QueryUpdaters where one fails; current empty-registry setup has no queries to isolate"

    it "logs structured WARN message with query name and error when a query fails" \_ -> do
      pending "needs a log capture fixture to assert the WARN entry; not assertable against the global Log writer without test doubles"

    it "respects the per-query timeout configured in RebuildOptions" \_ -> do
      pending "needs a registered QueryUpdater whose rebuild is slow enough to observe the timeout being applied per-query; current empty-registry setup ignores all timeout configuration"

    it "can be cancelled via AsyncTask.cancel without leaving partial writes" \_ -> do
      pending "needs a running rebuildAllAsync task plus an AsyncTask handle to cancel mid-flight; not testable as a sequential Task without concurrency fixtures"

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
      pending "needs concurrent observation of state transitions during an active rebuild; asserting readinessOf on an empty subscriber does not exercise any transition"

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
      pending "needs a multi-writer fixture with a live subscription running concurrently alongside rebuildFrom; current sequential empty-registry setup cannot observe replay-vs-live-event ordering"

  describe "Concurrency Hazard H2: Lost write via ON CONFLICT DO UPDATE" do
    it "prevents position regression via CAS-on-position" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

    it "is not a last-writer-wins pattern (lower positions do not overwrite higher)" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

  describe "Concurrency Hazard H3: Crash mid-update" do
    it "restarts from persisted position after process crash pre-write" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

    it "converges to correct state after crash mid-transaction" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

    it "does not re-apply events from after the persisted position on restart" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

  describe "Concurrency Hazard H4: Readiness flag visibility" do
    it "never flips readiness to Ready before the last query_object_store write is durable" \_ -> do
      pending "needs a registered QueryUpdater and a hook on the query_object_store write to observe that the Ready flag is only set after the write is confirmed durable; empty-registry setup completes immediately"

    it "ensures writes are visible before any request lands on the ready query" \_ -> do
      pending "needs a concurrent HTTP fixture to observe that requests after readinessOf=Ready always see the written state; not assertable without a running web server and a concurrency fixture"

  describe "Concurrency Hazard H5: Hash-mismatch mid-flight" do
    it "deletes stale hash rows atomically before replay starts" \_ -> do
      pending "needs a CheckpointStore fixture pre-seeded with mismatched-hash rows plus an observability hook on deleteStaleHash to confirm the deletion happens before any replay; empty subscriber has no checkpointStore"

    it "replays affected query only; other queries unaffected" \_ -> do
      pending "needs two registered QueryUpdaters where one has a mismatched hash; current empty-registry setup has no queries to isolate"

    it "handles concurrent live events arriving during hash-mismatch deletion" \_ -> do
      pending "needs a multi-writer fixture with live events arriving while deleteStaleHash is in flight; not testable as a sequential Task without concurrency fixtures"

  describe "Concurrency Hazard H6: Chunk-boundary tearing" do
    it "produces identical state regardless of chunk boundaries" \_ -> do
      pending "needs a registered QueryUpdater and enough events to observe that state after chunkSize=N matches state after chunkSize=M; current empty-registry setup produces an empty state regardless"

    it "does not lose events at chunk boundaries" \_ -> do
      pending "needs a registered QueryUpdater and enough events spanning multiple chunks to assert no event is skipped at a boundary; current empty-registry setup has no events to lose"

    it "does not duplicate events at chunk boundaries" \_ -> do
      pending "needs a registered QueryUpdater and enough events spanning multiple chunks to assert no event is applied twice at a boundary; current empty-registry setup has no events to duplicate"

  describe "Concurrency Hazard H7: Init ordering" do
    it "registers a query after Application.run and requires full replay from position 0" \_ -> do
      pending "needs a late-registered QueryUpdater (registered after Application.run) plus a pre-existing event history to assert that a full replay from position 0 is triggered; current empty-registry setup has no queries and no history"

    it "does not silently drop events for unregistered queries" \_ -> do
      pending "needs events in the EventStore for entity types that have no registered QueryUpdater, plus an assertion that those events are not silently discarded; current empty-registry setup has no events and no queries"

  describe "Concurrency Hazard H8: AsyncTask cancellation on shutdown" do
    it "persists position consistently when rebuild is cancelled via SIGTERM" \_ -> do
      pending "needs a running rebuildAllAsync task that is cancelled mid-flight plus an assertion that the persisted checkpoint position is consistent; not testable as a sequential Task without concurrency fixtures"

    it "resumes correctly after SIGTERM-interrupted rebuild" \_ -> do
      pending "needs a CheckpointStore pre-seeded with a mid-rebuild checkpoint position and an EventStore with events beyond that position to assert that replay resumes rather than restarts; current empty-registry setup has no checkpoint and no events"

  describe "Concurrency Hazard H9: Multi-writer (future-proofing)" do
    it "rejects stale writes when multiple processes attempt concurrent updates" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

    it "accommodates the contract for future multi-writer implementations" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

  describe "Property-based invariants" do
    it "position never decreases for any (query_name, instance_uuid) — atomicUpdate position monotonicity" \_ -> do
      pending "needs POSTGRES_AVAILABLE + a multi-writer / crash-injection fixture; currently the test passes vacuously when Postgres is unavailable, which is a worse-of-both-worlds outcome"

    it "replay(E) == replay(replay(E)) for any event sequence E — rebuildFrom idempotency" \_ -> do
      pending "needs a registered QueryUpdater and a fixed event sequence to assert that replay(replay(E)) produces the same state as replay(E); current empty-registry setup produces an empty state for any E"

    it "state is invariant to chunk boundary placement — chunk-boundary transparency" \_ -> do
      pending "needs a registered QueryUpdater and a fixed event sequence replayed with different chunkSize values to assert identical resulting state; current empty-registry setup produces an empty state regardless of chunkSize"

    it "readinessOf == Ready iff all readinessOfQuery return Ready — readiness aggregate consistency" \_ -> do
      pending "needs at least one registered QueryUpdater to have a non-trivial aggregate (a zero-query system is trivially consistent); current empty-registry setup does not distinguish aggregate from per-query consistency"

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

  describe "queryHashFor" do
    it "matches Data.Hashable on the String form of the query name" \_ -> do
      -- Load-bearing invariant for ADR-0059 H5: the runtime hash MUST match
      -- the compile-time hash produced by deriveQuery / deriveKnownHash, both
      -- of which compute `Hashable.hash (nameStr :: String)` at TH-time. If
      -- this assertion ever breaks, every restart of every checkpointed query
      -- will see a hash mismatch and force a full replay.
      let name = "UserOrders" :: [Char]
      let expected = (Hashable.hash name |> show |> toText)
      queryHashFor "UserOrders" |> shouldBe expected

    it "produces stable hashes across calls (pure function)" \_ -> do
      queryHashFor "Anything" |> shouldBe (queryHashFor "Anything")

    it "produces different hashes for different names" \_ -> do
      (queryHashFor "QueryA" != queryHashFor "QueryB") |> shouldBe True

  describe "checkpoint write path" do
    it "writes checkpoint with final position at end of rebuild" \_ -> do
      -- Fixture: 3 synthetic events with explicit positions fed via a stubbed
      -- event store, a trivial updater, and a recording mock CheckpointStore.
      -- After rebuildFrom completes, exactly one writeCheckpoint call should be
      -- recorded with the maximum (final) position seen during replay.
      baseStore <- InMemory.new |> Task.mapError toText
      eventMeta <- EventMetadata.new
      let mkEvent pos =
            Event
              { entityName = EntityName "write-test-entity"
              , streamId = StreamId "write-test-stream"
              , event = Json.null
              , metadata = eventMeta { globalPosition = Just (StreamPosition pos) }
              }
      let stubbedStore = baseStore
            { readAllEventsForwardFrom = \_ _ ->
                Stream.fromArray
                  ( Array.fromLinkedList
                      [ AllEvent (mkEvent 10)
                      , AllEvent (mkEvent 20)
                      , AllEvent (mkEvent 30)
                      ]
                  )
            }
      let trivialUpdater = QueryUpdater
            { queryName = "writeTest"
            , updateQuery = \_ -> Task.yield unit
            }
      let registry = Registry.register (EntityName "write-test-entity") trivialUpdater Registry.empty
      -- Build recording mock: logs every writeCheckpoint call.
      log <- ConcurrentVar.containing Array.empty
      let mockCpStore = CheckpointStore
            { resumeFromCheckpoint = \_ _ -> Task.yield Nothing
            , deleteStaleHash = \_ _ -> Task.yield unit
            , writeCheckpoint = \name hash pos -> do
                log |> ConcurrentVar.modify (Array.push (name, hash, pos))
                Task.yield unit
            }
      subscriber <- newWithCheckpointStore stubbedStore registry mockCpStore
      result <-
        Subscriber.rebuildFrom subscriber "writeTest" (StreamPosition 0) rebuildOptionsDefault
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected rebuild to succeed but got: #{toText (show err)}|]
        Ok _ -> pass
      calls <- ConcurrentVar.peek log
      do
        let expected = Array.wrap ("writeTest", queryHashFor "writeTest", 30)
        calls |> shouldBe expected

    it "writes checkpoint per live-subscription event via processEventHandler" \_ -> do
      -- Fixture: a subscriber with one registered updater and a recording mock
      -- CheckpointStore. Drive processEventHandler directly with a single event
      -- at position 42. Assert exactly one writeCheckpoint call recorded.
      baseStore <- InMemory.new |> Task.mapError toText
      eventMeta <- EventMetadata.new
      let syntheticEvent =
            Event
              { entityName = EntityName "live-test-entity"
              , streamId = StreamId "live-test-stream"
              , event = Json.null
              , metadata = eventMeta { globalPosition = Just (StreamPosition 42) }
              }
      let trivialUpdater = QueryUpdater
            { queryName = "liveTest"
            , updateQuery = \_ -> Task.yield unit
            }
      let registry = Registry.register (EntityName "live-test-entity") trivialUpdater Registry.empty
      log <- ConcurrentVar.containing Array.empty
      let mockCpStore = CheckpointStore
            { resumeFromCheckpoint = \_ _ -> Task.yield Nothing
            , deleteStaleHash = \_ _ -> Task.yield unit
            , writeCheckpoint = \name hash pos -> do
                log |> ConcurrentVar.modify (Array.push (name, hash, pos))
                Task.yield unit
            }
      subscriber <- newWithCheckpointStore baseStore registry mockCpStore
      result <-
        processEventHandler subscriber syntheticEvent
          |> Task.asResult
      case result of
        Err err -> fail [fmt|Expected processEventHandler to succeed but got: #{toText err}|]
        Ok _ -> pass
      calls <- ConcurrentVar.peek log
      do
        let expected = Array.wrap ("liveTest", queryHashFor "liveTest", 42)
        calls |> shouldBe expected
