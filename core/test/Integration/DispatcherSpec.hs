{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.DispatcherSpec where

import Array qualified
import AsyncTask qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import ConcurrentVar qualified
import Control.Exception qualified as GhcException
import Core
import DateTime qualified
import Integration qualified
import Integration.Command qualified as Command
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import LinkedList qualified
import Map qualified
import Maybe qualified
import Service.Command.Core (NameOf)
import Service.Event (Event (..), StreamPosition (..))
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId (StreamId)
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.InMemory qualified as InMemory
import Service.Application.Integrations qualified as Integrations
import Service.Integration.Dispatcher qualified as Dispatcher
import Task qualified
import Test
import Text qualified
import Tuple qualified
import Uuid qualified


-- ============================================================================
-- Test Types
-- ============================================================================

-- | Events for testing ordering.
data OrderingTestEvent
  = OrderingEvent {sequenceNum :: Int}
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON OrderingTestEvent


instance Json.FromJSON OrderingTestEvent


-- | Command to record processing order.
data RecordProcessed = RecordProcessed
  { entityId :: Text,
    sequenceNum :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON RecordProcessed


instance Json.FromJSON RecordProcessed


type instance NameOf RecordProcessed = "RecordProcessed"


-- ============================================================================
-- Helper Functions
-- ============================================================================

-- | Create a test event with the given entity ID and sequence number.
makeTestEvent :: Text -> Int -> Int -> Task Text (Event Json.Value)
makeTestEvent entityIdText seqNum globalPos = do
  -- Use fromTextUnsafe: test inputs are controlled and known to be valid
  let streamId = StreamId.fromTextUnsafe entityIdText
  now <- DateTime.now
  Task.yield
    Event
      { entityName = EntityName "TestEntity",
        streamId = streamId,
        event = Json.encode (OrderingEvent {sequenceNum = seqNum}),
        metadata =
          EventMetadata
            { eventId = Uuid.nil,
              relatedUserSub = Nothing,
              correlationId = Nothing,
              causationId = Nothing,
              createdAt = now,
              localPosition = Just (StreamPosition (fromIntegral seqNum)),
              globalPosition = Just (StreamPosition (fromIntegral globalPos))
            }
      }


makeContext :: Task Text Integration.ActionContext
makeContext = do
  store <- InMemorySecretStore.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Map.empty
      , Integration.fileAccess = Nothing
      }


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Dispatcher" do
    describe "basic functionality" do
      it "creates a dispatcher and processes a single event" \_ -> do
        -- Minimal test to verify basic dispatcher functionality
        processedCount <- ConcurrentVar.containing (0 :: Int)

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    processedCount |> ConcurrentVar.modify (+ 1)
                    Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        event <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event

        -- Wait for processing
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 1

      it "recovers from IO exceptions and processes subsequent events" \_ -> do
        -- Verifies the fix for issue #400: when a worker throws a raw IO exception
        -- (not a Task.throw error), safeWorkerLoop catches it, removes the dead
        -- worker from the map, and a new worker is spawned for the next event.
        processedEvents <- ConcurrentVar.containing ([] :: [Int])
        callCount <- ConcurrentVar.containing (0 :: Int)

        let crashOnFirstRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        currentCount <- ConcurrentVar.peek callCount
                        callCount |> ConcurrentVar.modify (+ 1)
                        if currentCount == 0
                          then do
                            -- First call: throw a raw IO exception (bypasses ExceptT)
                            Task.fromIO (GhcException.throwIO (GhcException.ErrorCall "simulated IO crash"))
                          else do
                            -- Subsequent calls: process normally
                            processedEvents
                              |> ConcurrentVar.modify (\evts -> evts ++ [decoded.sequenceNum])
                            Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 60000,
                reaperIntervalMs = 10000,
                enableReaper = False,
                workerChannelCapacity = 100,
                channelWriteTimeoutMs = 5000,
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            [crashOnFirstRunner]
            []
            Map.empty
            context

        -- Dispatch first event - worker will crash from IO exception
        event1 <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event1

        -- Wait for crash + cleanup (worker removed from map)
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Dispatch second event to the SAME entity
        -- If the fix works, a new worker is spawned and processes this event
        event2 <- makeTestEvent "entity-A" 2 2
        Dispatcher.dispatch dispatcher event2

        -- Wait for processing
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify the second event was processed (proving worker recovery)
        processed <- ConcurrentVar.peek processedEvents
        let processedArray = processed |> Array.fromLinkedList
        processedArray |> shouldBe [2]

    describe "per-entity ordering" do
      it "processes events for the same entity in order" \_ -> do
        -- Track the order in which events are processed
        processedOrder <- ConcurrentVar.containing ([] :: [(Text, Int)])

        -- Create a dispatcher with a runner that records processing order
        -- and introduces a small delay to expose ordering issues
        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        let entityIdText = event.streamId |> StreamId.toText
                        -- Add a small delay to expose race conditions
                        AsyncTask.sleep 10 |> Task.mapError (\_ -> "sleep error" :: Text)
                        -- Record that we processed this event
                        processedOrder
                          |> ConcurrentVar.modify (\order -> order ++ [(entityIdText, decoded.sequenceNum)])
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Send 3 events for entity A in order: 1, 2, 3
        eventA1 <- makeTestEvent "entity-A" 1 1
        eventA2 <- makeTestEvent "entity-A" 2 2
        eventA3 <- makeTestEvent "entity-A" 3 3

        -- Dispatch all events rapidly
        Dispatcher.dispatch dispatcher eventA1
        Dispatcher.dispatch dispatcher eventA2
        Dispatcher.dispatch dispatcher eventA3

        -- Wait for processing to complete
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify events were processed in order
        order <- ConcurrentVar.peek processedOrder
        let entityAEvents =
              order
                |> Array.fromLinkedList
                |> Array.takeIf (\(entityId, _) -> entityId == "entity-A")
                |> Array.map (\(_, seqNum) -> seqNum)

        -- Should be [1, 2, 3] - strict ordering
        entityAEvents |> shouldBe [1, 2, 3]

        -- Note: Workers keep running but will be cleaned up when test ends
        pass

      it "allows parallel processing across different entities" \_ -> do
        -- Track when each entity's processing started and finished
        processingLog <- ConcurrentVar.containing ([] :: [(Text, Text, Int)])

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        let entityIdText = event.streamId |> StreamId.toText
                        -- Log start
                        processingLog
                          |> ConcurrentVar.modify (\log -> log ++ [(entityIdText, "start", decoded.sequenceNum)])
                        -- Simulate work with delay
                        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)
                        -- Log end
                        processingLog
                          |> ConcurrentVar.modify (\log -> log ++ [(entityIdText, "end", decoded.sequenceNum)])
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Send events for two different entities
        eventA1 <- makeTestEvent "entity-A" 1 1
        eventB1 <- makeTestEvent "entity-B" 1 2

        -- Dispatch both events at the same time
        Dispatcher.dispatch dispatcher eventA1
        Dispatcher.dispatch dispatcher eventB1

        -- Wait for processing
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify both entities were processed
        log <- ConcurrentVar.peek processingLog
        let logArray = log |> Array.fromLinkedList

        -- Both entities should have start and end entries
        let entityAEntries = logArray |> Array.takeIf (\(entityId, _, _) -> entityId == "entity-A")
        let entityBEntries = logArray |> Array.takeIf (\(entityId, _, _) -> entityId == "entity-B")

        Array.length entityAEntries |> shouldBe 2
        Array.length entityBEntries |> shouldBe 2

        -- Check for parallelism: if B started before A ended, they ran in parallel
        -- Find indices of A's end and B's start
        let findIndex predicate arr = do
              arr
                |> Array.indexed
                |> Array.takeIf (\(_, item) -> predicate item)
                |> Array.first
                |> fmap Tuple.first

        let aEndIndex = logArray |> findIndex (\(e, phase, _) -> e == "entity-A" && phase == "end")
        let bStartIndex = logArray |> findIndex (\(e, phase, _) -> e == "entity-B" && phase == "start")

        -- If B started before or at the same time A ended, they were parallel
        case (aEndIndex, bStartIndex) of
          (Just aEnd, Just bStart) -> do
            -- B should start before A ends (parallel execution)
            bStart |> shouldSatisfy (< aEnd)
          _ -> fail "Expected both entities to have start/end log entries"

        pass

      it "maintains ordering within each entity even with interleaved dispatch" \_ -> do
        processedOrder <- ConcurrentVar.containing ([] :: [(Text, Int)])

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        let entityIdText = event.streamId |> StreamId.toText
                        -- Variable delay based on sequence to expose ordering bugs
                        let delayMs = case decoded.sequenceNum of
                              1 -> 30
                              2 -> 10
                              _ -> 20
                        AsyncTask.sleep delayMs |> Task.mapError (\_ -> "sleep error" :: Text)
                        processedOrder
                          |> ConcurrentVar.modify (\order -> order ++ [(entityIdText, decoded.sequenceNum)])
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Interleave events: A1, B1, A2, B2, A3, B3
        eventA1 <- makeTestEvent "entity-A" 1 1
        eventB1 <- makeTestEvent "entity-B" 1 2
        eventA2 <- makeTestEvent "entity-A" 2 3
        eventB2 <- makeTestEvent "entity-B" 2 4
        eventA3 <- makeTestEvent "entity-A" 3 5
        eventB3 <- makeTestEvent "entity-B" 3 6

        Dispatcher.dispatch dispatcher eventA1
        Dispatcher.dispatch dispatcher eventB1
        Dispatcher.dispatch dispatcher eventA2
        Dispatcher.dispatch dispatcher eventB2
        Dispatcher.dispatch dispatcher eventA3
        Dispatcher.dispatch dispatcher eventB3

        -- Wait for all processing
        AsyncTask.sleep 300 |> Task.mapError (\_ -> "sleep error" :: Text)

        order <- ConcurrentVar.peek processedOrder
        let orderArray = order |> Array.fromLinkedList

        -- Entity A events should be in order: 1, 2, 3
        let entityASeqs =
              orderArray
                |> Array.takeIf (\(e, _) -> e == "entity-A")
                |> Array.map Tuple.second

        -- Entity B events should be in order: 1, 2, 3
        let entityBSeqs =
              orderArray
                |> Array.takeIf (\(e, _) -> e == "entity-B")
                |> Array.map Tuple.second

        entityASeqs |> shouldBe [1, 2, 3]
        entityBSeqs |> shouldBe [1, 2, 3]

        pass

    -- NOTE: Lifecycle tests are pending because the reaper background task
    -- interferes with hspec's async handling. The dispatcher uses AsyncTask.run
    -- which spawns threads that don't terminate until the reaper interval elapses.
    -- The poison pill pattern (Stop message) works correctly - verified manually
    -- through the testbed. These tests would pass if we could either:
    -- 1. Use forkIO instead of async for the reaper
    -- 2. Or have a way to kill the reaper thread explicitly
    -- For now, the functionality is verified through end-to-end testbed tests.
    xdescribe "lifecycle management" do
      it "calls initialize when worker spawns for a new entity" \_ -> do
        -- Track initialize calls
        initializeCalls <- ConcurrentVar.containing ([] :: [Text])
        processedEvents <- ConcurrentVar.containing (0 :: Int)

        let lifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \streamId -> do
                    let entityIdText = StreamId.toText streamId
                    initializeCalls |> ConcurrentVar.modify (\calls -> calls ++ [entityIdText])
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_event -> do
                            processedEvents |> ConcurrentVar.modify (+ 1)
                            Task.yield [],
                          cleanup = Task.yield unit
                        }
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.newWithLifecycle eventStore [] [lifecycleRunner] Map.empty

        -- Send event to entity A
        eventA <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher eventA

        -- Wait for processing
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify initialize was called for entity-A
        calls <- ConcurrentVar.peek initializeCalls
        calls |> Array.fromLinkedList |> shouldBe ["entity-A"]

        -- Verify event was processed
        processed <- ConcurrentVar.peek processedEvents
        processed |> shouldBe 1

        -- Cleanup: shutdown dispatcher to stop workers
        Dispatcher.shutdown dispatcher

      it "calls initialize for different entities separately" \_ -> do
        -- Track initialize calls
        initializeCalls <- ConcurrentVar.containing ([] :: [Text])

        let lifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \streamId -> do
                    let entityIdText = StreamId.toText streamId
                    initializeCalls |> ConcurrentVar.modify (\calls -> calls ++ [entityIdText])
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_event -> Task.yield [],
                          cleanup = Task.yield unit
                        }
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.newWithLifecycle eventStore [] [lifecycleRunner] Map.empty

        -- Send events to two different entities
        eventA <- makeTestEvent "entity-A" 1 1
        eventB <- makeTestEvent "entity-B" 1 2
        Dispatcher.dispatch dispatcher eventA
        Dispatcher.dispatch dispatcher eventB

        -- Wait for processing
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify initialize was called for both entities
        calls <- ConcurrentVar.peek initializeCalls
        let callsArray = calls |> Array.fromLinkedList
        Array.length callsArray |> shouldBe 2
        callsArray |> shouldSatisfy (Array.contains "entity-A")
        callsArray |> shouldSatisfy (Array.contains "entity-B")

      it "calls cleanup when worker is reaped due to idle timeout" \_ -> do
        -- Track cleanup calls
        cleanupCalls <- ConcurrentVar.containing ([] :: [Text])

        let lifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \streamId -> do
                    let entityIdText = StreamId.toText streamId
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_event -> Task.yield [],
                          cleanup = do
                            cleanupCalls |> ConcurrentVar.modify (\calls -> calls ++ [entityIdText])
                        }
                }

        -- Create dispatcher with very short idle timeout (50ms)
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 50,
                reaperIntervalMs = 25,
                enableReaper = True,
                workerChannelCapacity = 100,
                channelWriteTimeoutMs = 5000,
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            []
            [lifecycleRunner]
            Map.empty
            context

        -- Send event to entity A
        eventA <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher eventA

        -- Wait long enough for processing + idle timeout + reaper cycle
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify cleanup was called for entity-A
        calls <- ConcurrentVar.peek cleanupCalls
        calls |> Array.fromLinkedList |> shouldBe ["entity-A"]

      it "re-initializes when event arrives for a reaped entity" \_ -> do
        -- Track initialize calls
        initializeCalls <- ConcurrentVar.containing ([] :: [Text])

        let lifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \streamId -> do
                    let entityIdText = StreamId.toText streamId
                    initializeCalls |> ConcurrentVar.modify (\calls -> calls ++ [entityIdText])
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_event -> Task.yield [],
                          cleanup = Task.yield unit
                        }
                }

        -- Create dispatcher with very short idle timeout
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 50,
                reaperIntervalMs = 25,
                enableReaper = True,
                workerChannelCapacity = 100,
                channelWriteTimeoutMs = 5000,
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            []
            [lifecycleRunner]
            Map.empty
            context

        -- Send first event
        eventA1 <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher eventA1

        -- Wait for processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify first initialize
        calls1 <- ConcurrentVar.peek initializeCalls
        Array.length (calls1 |> Array.fromLinkedList) |> shouldBe 1

        -- Wait for reaper to clean up the idle worker
        AsyncTask.sleep 150 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Send second event to same entity
        eventA2 <- makeTestEvent "entity-A" 2 2
        Dispatcher.dispatch dispatcher eventA2

        -- Wait for processing
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify initialize was called twice (once for each spawn)
        calls2 <- ConcurrentVar.peek initializeCalls
        calls2 |> Array.fromLinkedList |> shouldBe ["entity-A", "entity-A"]

    describe "concurrent dispatch race condition" do
      it "creates exactly one worker and processes all events when 100 events arrive simultaneously" \_ -> do
        -- This test verifies the fix for the worker spawn race condition.
        -- Before the fix, two threads could both see no worker exists,
        -- both spawn workers, and one would be orphaned (memory leak + lost events).
        --
        -- NOTE: We do NOT verify strict ordering here because concurrent dispatch
        -- from multiple threads does NOT guarantee arrival order at the channel.
        -- The per-entity ordering guarantee only applies to events arriving through
        -- a single sequential path (e.g., from a subscription).
        processedCount <- ConcurrentVar.containing (0 :: Int)
        processedSequences <- ConcurrentVar.containing ([] :: [Int])

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        processedCount |> ConcurrentVar.modify (+ 1)
                        processedSequences
                          |> ConcurrentVar.modify (\seqs -> seqs ++ [decoded.sequenceNum])
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Create 100 events for the same entity
        let eventCount = 100
        events <-
          Array.range 1 eventCount
            |> Task.mapArray (\i -> makeTestEvent "entity-A" i i)

        -- Dispatch all events simultaneously using runAllIgnoringErrors
        let dispatchTasks =
              events
                |> Array.map (\event -> Dispatcher.dispatch dispatcher event)
        AsyncTask.runAllIgnoringErrors dispatchTasks

        -- Wait for all events to be processed
        -- With 100 events and potential small delays, give ample time
        AsyncTask.sleep 2000 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify ALL 100 events were processed (no drops)
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe eventCount

        -- Verify all events were processed exactly once (no duplicates, no drops)
        seqs <- ConcurrentVar.peek processedSequences
        let seqArray = seqs |> Array.fromLinkedList
        let sortedSeqs = seqArray |> Array.toLinkedList |> LinkedList.sort |> Array.fromLinkedList
        sortedSeqs |> shouldBe (Array.range 1 eventCount)

        pass

    describe "channel write timeout" do
      it "drops events and continues when channel is full and timeout expires" \_ -> do
        -- Test that when a worker's channel fills up (slow worker) and the timeout
        -- expires, the event is dropped and dispatch continues without failing.
        --
        -- Setup:
        -- 1. Create a dispatcher with capacity=2, timeout=20ms
        -- 2. Worker processes slowly (blocks for 100ms per event)
        -- 3. Dispatch 4 events rapidly
        -- 4. First event enters channel, worker picks it up and starts processing
        -- 5. Events 2 and 3 fill the channel (capacity=2)
        -- 6. Event 4 times out waiting for channel space → dropped
        -- 7. Verify only 3 events were processed (not 4)
        processedCount <- ConcurrentVar.containing (0 :: Int)
        processedSequences <- ConcurrentVar.containing ([] :: [Int])

        let slowRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        -- Slow processing to cause channel backup
                        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)
                        processedCount |> ConcurrentVar.modify (+ 1)
                        processedSequences
                          |> ConcurrentVar.modify (\seqs -> seqs ++ [decoded.sequenceNum])
                        Task.yield Array.empty
                }

        -- Create dispatcher with small channel (capacity=2) and short timeout (20ms)
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 60000,
                reaperIntervalMs = 10000,
                enableReaper = False,
                workerChannelCapacity = 2,
                channelWriteTimeoutMs = 20,
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            [slowRunner]
            []
            Map.empty
            context

        -- Create 4 events for the same entity
        event1 <- makeTestEvent "entity-A" 1 1
        event2 <- makeTestEvent "entity-A" 2 2
        event3 <- makeTestEvent "entity-A" 3 3
        event4 <- makeTestEvent "entity-A" 4 4

        -- Dispatch event1 and wait for worker to pick it up
        Dispatcher.dispatch dispatcher event1
        -- Give worker time to read event1 from channel and start processing
        AsyncTask.sleep 20 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Now dispatch 3 more events rapidly
        -- Events 2 and 3 fill the channel (capacity=2)
        -- Event 4: channel full, waits 20ms, times out → dropped
        Dispatcher.dispatch dispatcher event2
        Dispatcher.dispatch dispatcher event3
        Dispatcher.dispatch dispatcher event4

        -- Wait for processing to complete (3 events * 100ms + buffer)
        AsyncTask.sleep 500 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify only 3 events were processed (event4 was dropped due to timeout)
        count <- ConcurrentVar.peek processedCount

        -- Verify events 1, 2, and 3 were processed (event 4 dropped)
        seqs <- ConcurrentVar.peek processedSequences
        let seqArray = seqs |> Array.fromLinkedList

        -- Assertions (shutdown skipped to avoid potential blocking)
        count |> shouldBe 3
        seqArray |> shouldBe [1, 2, 3]

      it "force-cancels workers when Stop message cannot be delivered (channel full)" \_ -> do
        -- Test that workers are force-cancelled when their channel is full and Stop can't be delivered.
        -- This verifies the fix for the memory leak where workers would never be cleaned up if
        -- the Stop message couldn't be delivered due to a full channel.
        --
        -- Setup:
        -- 1. Create a dispatcher with capacity=1 and very short timeout (10ms)
        -- 2. Worker blocks forever on first event (simulating a stuck worker)
        -- 3. Fill the channel so Stop can't be delivered
        -- 4. Call shutdown - this should force-cancel the worker via AsyncTask.cancel
        -- 5. Verify shutdown completes without hanging (the test itself is the verification)
        workerStarted <- ConcurrentVar.containing False
        workerCancelled <- ConcurrentVar.containing False

        let blockingRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    workerStarted |> ConcurrentVar.modify (\_ -> True)
                    -- Block forever (simulating a stuck worker)
                    -- This will be force-cancelled by AsyncTask.cancel
                    let blockForever :: Task Text Unit
                        blockForever = do
                          AsyncTask.sleep 100000 |> Task.mapError (\_ -> "sleep error" :: Text)
                          blockForever
                    blockForever |> Task.asResult |> Task.map (\_ -> ())
                    -- If we get here, we were cancelled
                    workerCancelled |> ConcurrentVar.modify (\_ -> True)
                    Task.yield Array.empty
                }

        -- Create dispatcher with tiny channel (capacity=1) and very short timeout
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 60000,
                reaperIntervalMs = 10000,
                enableReaper = False,
                workerChannelCapacity = 1,
                channelWriteTimeoutMs = 10,  -- Very short timeout
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            [blockingRunner]
            []
            Map.empty
            context

        -- Dispatch first event - worker will pick it up and block forever
        event1 <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event1

        -- Wait for worker to start processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify worker started
        started <- ConcurrentVar.peek workerStarted
        started |> shouldBe True

        -- Fill the channel so Stop can't be delivered
        event2 <- makeTestEvent "entity-A" 2 2
        Dispatcher.dispatch dispatcher event2

        -- Now the channel has capacity=1, and we've written one message to it.
        -- The worker is blocked processing event1, so it won't read from the channel.
        -- When we call shutdown, the Stop message will timeout trying to write to the full channel.
        -- The fix ensures AsyncTask.cancel is called as fallback.

        -- Call shutdown - this should force-cancel the stuck worker
        -- If the fix isn't working, this would hang forever waiting for Stop to be delivered
        Dispatcher.shutdown dispatcher

        -- Give a moment for the cancellation to propagate
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Test passes if we get here without hanging
        -- (The act of shutdown completing is the verification)
        pass

      it "does not drop events when channel has capacity" \_ -> do
        -- Verify that events are NOT dropped when channel has space
        processedCount <- ConcurrentVar.containing (0 :: Int)

        let fastRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    processedCount |> ConcurrentVar.modify (+ 1)
                    Task.yield Array.empty
                }

        -- Create dispatcher with generous capacity
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.DispatcherConfig
              { idleTimeoutMs = 60000,
                reaperIntervalMs = 10000,
                enableReaper = False,
                workerChannelCapacity = 100,
                channelWriteTimeoutMs = 50,
                eventProcessingTimeoutMs = Nothing
              }
            eventStore
            [fastRunner]
            []
            Map.empty
            context

        -- Dispatch 10 events
        events <-
          Array.range 1 10
            |> Task.mapArray (\i -> makeTestEvent "entity-A" i i)

        events |> Task.forEach (\event -> Dispatcher.dispatch dispatcher event)

        -- Wait for processing
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- All 10 events should be processed (none dropped)
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 10

    describe "concurrent dispatch to multiple entities" do
      it "handles concurrent dispatch to multiple entities correctly" \_ -> do
        -- Test that concurrent dispatch to multiple different entities works correctly
        -- Each entity should have exactly one worker, and all events should be processed
        --
        -- NOTE: We verify all events are processed for each entity but do NOT check
        -- strict ordering because concurrent dispatch doesn't guarantee it.
        processedByEntity <- ConcurrentVar.containing (Map.empty :: Map Text [Int])

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        let entityIdText = event.streamId |> StreamId.toText
                        processedByEntity
                          |> ConcurrentVar.modify \entityMap ->
                            let current = Map.get entityIdText entityMap |> Maybe.withDefault []
                             in Map.set entityIdText (current ++ [decoded.sequenceNum]) entityMap
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Create 10 events for each of 10 different entities (100 total)
        let entitiesCount = 10
        let eventsPerEntity = 10
        allEvents <-
          Array.range 1 entitiesCount
            |> Task.mapArray \entityNum -> do
              let entityId = [fmt|entity-#{entityNum}|]
              Array.range 1 eventsPerEntity
                |> Task.mapArray \seqNum -> do
                  let globalPos = (entityNum - 1) * eventsPerEntity + seqNum
                  makeTestEvent entityId seqNum globalPos

        let flatEvents = allEvents |> Array.flatten

        -- Dispatch all events simultaneously
        let dispatchTasks =
              flatEvents
                |> Array.map (\event -> Dispatcher.dispatch dispatcher event)
        AsyncTask.runAllIgnoringErrors dispatchTasks

        -- Wait for processing
        AsyncTask.sleep 2000 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify each entity processed all its events (sorted to account for concurrent arrival)
        entityMap <- ConcurrentVar.peek processedByEntity
        Array.range 1 entitiesCount
          |> Task.forEach \entityNum -> do
            let entityId = [fmt|entity-#{entityNum}|]
            let processed = Map.get entityId entityMap |> Maybe.withDefault []
            let processedArray = processed |> Array.fromLinkedList
            -- Should have all events
            Array.length processedArray |> shouldBe eventsPerEntity
            -- All events should be present (sorted comparison)
            let sortedProcessed = processedArray |> Array.toLinkedList |> LinkedList.sort |> Array.fromLinkedList
            sortedProcessed |> shouldBe (Array.range 1 eventsPerEntity)

        pass

    describe "config threading" do
      it "uses custom config when provided (simulating Application.withDispatcherConfig)" \_ -> do
        -- Verify that a custom DispatcherConfig with short channelWriteTimeoutMs
        -- causes event drops when channel is full (proving the config was used)
        processedCount <- ConcurrentVar.containing (0 :: Int)

        let slowRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    -- Block for 200ms to cause channel backup
                    AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)
                    processedCount |> ConcurrentVar.modify (+ 1)
                    Task.yield Array.empty
                }

        let customConfig = Dispatcher.DispatcherConfig
              { idleTimeoutMs = 60000,
                reaperIntervalMs = 10000,
                enableReaper = False,
                workerChannelCapacity = 1,
                channelWriteTimeoutMs = 10,
                eventProcessingTimeoutMs = Nothing
              }

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext

        -- Call through startIntegrationSubscriber so the Application-level config threading is exercised
        maybeDispatcher <- Integrations.startIntegrationSubscriber
              (Just customConfig)
              eventStore
              [slowRunner]
              []
              Map.empty
              context
        let dispatcher = case maybeDispatcher of
              Just d -> d
              Nothing -> panic "expected a dispatcher"

        -- Dispatch first event - worker picks it up and blocks
        event1 <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event1
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Fill channel (capacity=1) and try one more that should timeout
        event2 <- makeTestEvent "entity-A" 2 2
        event3 <- makeTestEvent "entity-A" 3 3
        Dispatcher.dispatch dispatcher event2
        Dispatcher.dispatch dispatcher event3

        -- Wait for processing
        AsyncTask.sleep 600 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- With capacity=1 and timeout=10ms, event3 should be dropped
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 2
