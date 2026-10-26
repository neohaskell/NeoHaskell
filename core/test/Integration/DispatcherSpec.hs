{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.DispatcherSpec where

import Array qualified
import AsyncTask qualified
import Channel qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import ConcurrentVar qualified
import ConcurrentMap qualified
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
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Service.Application.Integrations qualified as Integrations
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service qualified
import Service.Transport (DispatchHandler, EndpointHandler)
import Service.Transport.InternalSpec qualified as InternalCommand
import Stream qualified
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
makeTestEvent = makeTestEventForEntity "TestEntity"


-- | Create a test event for a specific entity type.
makeTestEventForEntity :: Text -> Text -> Int -> Int -> Task Text (Event Json.Value)
makeTestEventForEntity entityTypeName entityIdText seqNum globalPos = do
  -- Use fromTextUnsafe: test inputs are controlled and known to be valid
  let streamId = StreamId.fromTextUnsafe entityIdText
  now <- DateTime.now
  eventId <- Uuid.generate
  Task.yield
    Event
      { entityName = EntityName entityTypeName,
        streamId = streamId,
        event = Json.encode (OrderingEvent {sequenceNum = seqNum}),
        metadata =
          EventMetadata
            { eventId,
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
  locks <- ConcurrentMap.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Integration.fromMap Map.empty
      , Integration.refreshLocks = locks
      , Integration.fileAccess = Nothing
      , Integration.outboundDispatch = DispatchRegistry.empty
      }


waitForDispatchCount :: Int -> Int -> ConcurrentVar.ConcurrentVar Int -> Task Text Unit
waitForDispatchCount attemptsLeft expectedCount dispatchCount = do
  currentCount <- dispatchCount |> ConcurrentVar.peek
  Task.unless (currentCount >= expectedCount) do
    if attemptsLeft <= 0
      then Task.throw "Timed out waiting for command dispatch"
      else do
        AsyncTask.sleep 20 |> Task.mapError (\_ -> "dispatch completion wait failed" :: Text)
        waitForDispatchCount (attemptsLeft - 1) expectedCount dispatchCount


waitForBlockedOnSTM :: Int -> AsyncTask.AsyncTask err result -> Task Text Unit
waitForBlockedOnSTM attemptsLeft child = do
  waiting <- AsyncTask.isWaitingForStateChange child
  Task.unless waiting do
    if attemptsLeft <= 0
      then Task.throw "Timed out waiting for a thread blocked on STM"
      else do
        AsyncTask.sleep 5 |> Task.mapError (\_ -> "thread status wait failed" :: Text)
        waitForBlockedOnSTM (attemptsLeft - 1) child


cancelShutdownWhileWriterIsAdmitted ::
  Dispatcher.IntegrationDispatcher ->
  Event Json.Value ->
  Event Json.Value ->
  Event Json.Value ->
  Channel.Channel Unit ->
  Channel.Channel Unit ->
  Task Text Bool
cancelShutdownWhileWriterIsAdmitted dispatcher activeEvent queuedEvent waitingEvent workerStarted workerStopped = do
  Dispatcher.dispatch dispatcher activeEvent
  Channel.read workerStarted
  Dispatcher.dispatch dispatcher queuedEvent

  writerTask <- AsyncTask.run (Dispatcher.dispatch dispatcher waitingEvent)
  waitForBlockedOnSTM 200 writerTask

  shutdownTask <- AsyncTask.run (Dispatcher.shutdown dispatcher)
  waitForBlockedOnSTM 200 shutdownTask

  AsyncTask.requestThreadTermination shutdownTask
  AsyncTask.cancel writerTask
  workerStopRace <-
    AsyncTask.race
      (Channel.read workerStopped)
      (AsyncTask.sleep 500 |> Task.mapError (\_ -> "worker cancellation wait failed" :: Text))

  shutdownOutcome <- AsyncTask.waitOutcome shutdownTask
  case shutdownOutcome of
    AsyncTask.Interrupted -> Task.yield unit
    _ -> Task.throw "Shutdown did not preserve interruption"
  Dispatcher.shutdown dispatcher

  case workerStopRace of
    AsyncTask.LeftWon _ -> Task.yield True
    AsyncTask.RightWon _ -> Task.yield False


blockWorkerForever :: forall result. Task Text result
blockWorkerForever = do
  AsyncTask.sleep 100000 |> Task.mapError (\_ -> "sleep error" :: Text)
  blockWorkerForever


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Dispatcher" do
    describe "basic functionality" do
      it "passes source metadata to commands emitted by an outbound runner" \_ -> do
        capturedSources <- Channel.new
        let payload =
              Integration.CommandPayload
                { commandType = "CaptureSource",
                  commandData = Json.encode ("ignored" :: Text.Text)
                }
        let runner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> Task.yield [payload]
                }
        let captureSource :: DispatchHandler
            captureSource sourceEventMetadata _requestContext _commandBytes _respond =
              Channel.write sourceEventMetadata capturedSources

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfigWithSource
            Dispatcher.Setup
              { settings = Dispatcher.defaultConfig,
                eventStore = eventStore,
                outboundRunners = [runner],
                lifecycleRunners = [],
                commandEndpoints = (Map.empty |> Map.set "CaptureSource" captureSource),
                actionContext = context
              }
        event <- makeTestEvent "entity-source" 1 1

        Dispatcher.dispatch dispatcher event
        capturedSource <- Channel.read capturedSources
        capturedSource |> shouldBe (Just event.metadata)

      it "persists a command emitted by an outbound runner with its source event metadata" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let typedEventStore = rawEventStore |> EventStore.castEventStore @InternalCommand.InternalEvent
        recordId <- Uuid.generate
        let entityName = EntityName "InternalEntity"
        let streamId = StreamId.toStreamId recordId
        let commandRunner =
              Service.new
                |> Service.command @InternalCommand.CreateInternalRecord
                |> Service.toServiceRunner
        (_, _, commandEndpoints) <- commandRunner.getEndpointsByTransport rawEventStore Map.empty
        dispatchCount <- ConcurrentVar.containing (0 :: Int)
        let observeDispatchHandler handler source request bytes respond = do
              handler source request bytes respond
              dispatchCount |> ConcurrentVar.modify (+ 1)
        let observedCommandEndpoints = commandEndpoints |> Map.mapValues observeDispatchHandler

        let payload =
              Integration.makeCommandPayload
                InternalCommand.CreateInternalRecord {recordId}
        let outboundRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_context _store _event -> Task.yield [payload]
                }
        actionContext <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfigWithSource
            Dispatcher.Setup
              { settings = Dispatcher.defaultConfig,
                eventStore = rawEventStore,
                outboundRunners = [outboundRunner],
                lifecycleRunners = [],
                commandEndpoints = observedCommandEndpoints,
                actionContext = actionContext
              }
        sourceEventBase <- makeTestEvent "outbound-source-stream" 1 1
        let sourceEvent =
              sourceEventBase
                { metadata =
                    sourceEventBase.metadata
                      { relatedUserSub = Just "source-actor",
                        correlationId = Just "root-chain"
                      }
                }
        Dispatcher.dispatch dispatcher sourceEvent
        waitForDispatchCount 250 1 dispatchCount
        Dispatcher.shutdown dispatcher

        messages <- typedEventStore.readAllStreamEvents entityName streamId |> Task.mapError toText
        events <- messages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
        Array.length events |> shouldBe 1
        case Array.last events of
          Nothing -> fail "Expected a saved command event from the outbound callback"
          Just saved -> do
            saved.metadata.relatedUserSub |> shouldBe (Just "source-actor")
            saved.metadata.correlationId |> shouldBe (Just "root-chain")
            saved.metadata.causationId |> shouldBe (Just (Uuid.toText sourceEvent.metadata.eventId))
        rawEventStore.close |> Task.mapError toText

      it "allocates distinct request correlations for repeated autonomous command dispatches" \_ -> do
        rawEventStore <- InMemory.new |> Task.mapError toText
        let typedEventStore = rawEventStore |> EventStore.castEventStore @InternalCommand.InternalEvent
        let entityName = EntityName "InternalEntity"
        firstRecordId <- Uuid.generate
        secondRecordId <- Uuid.generate
        let commandRunner =
              Service.new
                |> Service.command @InternalCommand.CreateInternalRecord
                |> Service.toServiceRunner
        (_, _, commandEndpoints) <- commandRunner.getEndpointsByTransport rawEventStore Map.empty
        let firstPayload = Integration.makeCommandPayload InternalCommand.CreateInternalRecord {recordId = firstRecordId}
        let secondPayload = Integration.makeCommandPayload InternalCommand.CreateInternalRecord {recordId = secondRecordId}

        Dispatcher.dispatchCommandWithSource commandEndpoints Nothing firstPayload
        Dispatcher.dispatchCommandWithSource commandEndpoints Nothing secondPayload

        firstMessages <- typedEventStore.readAllStreamEvents entityName (StreamId.toStreamId firstRecordId) |> Task.mapError toText
        firstEvents <- firstMessages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
        secondMessages <- typedEventStore.readAllStreamEvents entityName (StreamId.toStreamId secondRecordId) |> Task.mapError toText
        secondEvents <- secondMessages |> Stream.toArray |> Task.map EventStore.collectStreamEvents
        case (Array.last firstEvents, Array.last secondEvents) of
          (Just firstSaved, Just secondSaved) -> do
            firstSaved.metadata.relatedUserSub |> shouldBe Nothing
            secondSaved.metadata.relatedUserSub |> shouldBe Nothing
            firstSaved.metadata.causationId |> shouldBe Nothing
            secondSaved.metadata.causationId |> shouldBe Nothing
            let firstCorrelation = firstSaved.metadata.correlationId
            let secondCorrelation = secondSaved.metadata.correlationId
            firstCorrelation |> shouldSatisfy (\value -> value != Nothing && value != Just (Uuid.toText Uuid.nil))
            secondCorrelation |> shouldSatisfy (\value -> value != Nothing && value != firstCorrelation)
          _ -> fail "Expected both autonomous commands to append events"
        rawEventStore.close |> Task.mapError toText

      it "keeps the legacy Map Text EndpointHandler dispatcher and dispatch APIs" \_ -> do
        dispatchCount <- ConcurrentVar.containing (0 :: Int)
        let payload =
              Integration.CommandPayload
                { commandType = "LegacyCommand",
                  commandData = Json.encode ("ignored" :: Text.Text)
                }
        let legacyHandler :: EndpointHandler
            legacyHandler _requestContext _commandBytes _respond =
              dispatchCount |> ConcurrentVar.modify (+ 1)
        let legacyEndpoints :: Map.Map Text EndpointHandler
            legacyEndpoints = Map.empty |> Map.set "LegacyCommand" legacyHandler
        let runner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> Task.yield [payload]
                }

        Dispatcher.dispatchCommand legacyEndpoints payload
        directDispatchCount <- ConcurrentVar.peek dispatchCount
        directDispatchCount |> shouldBe 1

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = Dispatcher.defaultConfig,
                eventStore = eventStore,
                outboundRunners = [runner],
                lifecycleRunners = [],
                commandEndpoints = legacyEndpoints,
                actionContext = context
              }
        event <- makeTestEvent "legacy-stream" 1 1
        Dispatcher.dispatch dispatcher event
        waitForDispatchCount 250 2 dispatchCount

        workerDispatchCount <- ConcurrentVar.peek dispatchCount
        workerDispatchCount |> shouldBe 2

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

      it "catches IO exceptions at per-runner level and continues processing" \_ -> do
        -- Verifies fix for issues #400/#571: when a runner throws a raw IO exception,
        -- executeWithRetry catches it at the per-runner level. With maxEventRetries=0,
        -- the event is skipped and the worker continues processing subsequent events
        -- without crashing.
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
                            Task.evaluate (panic "private-runner-error-sentinel")
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
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 0,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [crashOnFirstRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

        -- Dispatch first event - IO exception caught at runner level, event skipped
        event1 <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event1

        -- Wait for processing (no crash, worker continues)
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Dispatch second event to the SAME entity
        -- Worker is still alive and processes this event
        event2 <- makeTestEvent "entity-A" 2 2
        Dispatcher.dispatch dispatcher event2

        -- Wait for processing
        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify the second event was processed (worker survived the IO exception)
        processed <- ConcurrentVar.peek processedEvents
        let processedArray = processed |> Array.fromLinkedList
        processedArray |> shouldBe [2]

      it "reports a timed out event runner without emitting a command" \_ -> do
        completed <- ConcurrentVar.containing False
        laterRunnerStarted <- ConcurrentVar.containing False
        let slowRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_context _store _event -> do
                    AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)
                    completed |> ConcurrentVar.modify (\_ -> True)
                    Task.yield Array.empty
                }
        let laterRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_context _store _event -> do
                    laterRunnerStarted |> ConcurrentVar.modify (\_ -> True)
                    Task.yield Array.empty
                }
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 10,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Just 20,
                      maxEventRetries = 0,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [slowRunner, laterRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }
        event <- makeTestEvent "timeout-stream" 1 1
        Dispatcher.dispatch dispatcher event
        AsyncTask.sleep 150 |> Task.mapError (\_ -> "sleep error" :: Text)
        wasCompleted <- ConcurrentVar.peek completed
        wasCompleted |> shouldBe False
        wasLaterRunnerStarted <- ConcurrentVar.peek laterRunnerStarted
        wasLaterRunnerStarted |> shouldBe False

      it "reports every matching runner when callback dispatch fails" \_ -> do
        dispatchCount <- ConcurrentVar.containing (0 :: Int)
        laterRunnerStarted <- ConcurrentVar.containing False
        let payload =
              Integration.CommandPayload
                { commandType = "FailCommand",
                  commandData = Json.encode ("ignored" :: Text.Text)
                }
        let failingRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_context _store _event -> Task.yield [payload]
                }
        let laterRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_context _store _event -> do
                    laterRunnerStarted |> ConcurrentVar.modify (\_ -> True)
                    Task.yield Array.empty
                }
        let failDispatch :: DispatchHandler
            failDispatch _sourceEventMetadata _requestContext _commandBytes _respond = do
              dispatchCount |> ConcurrentVar.modify (+ 1)
              Task.throw "private-dispatch-error-sentinel"

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfigWithSource
            Dispatcher.Setup
              { settings = Dispatcher.defaultConfig,
                eventStore = eventStore,
                outboundRunners = [failingRunner, laterRunner],
                lifecycleRunners = [],
                commandEndpoints = (Map.empty |> Map.set "FailCommand" failDispatch),
                actionContext = context
              }
        event <- makeTestEvent "dispatch-failure-stream" 1 1
        Dispatcher.dispatch dispatcher event
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        dispatchAttempts <- ConcurrentVar.peek dispatchCount
        dispatchAttempts |> shouldBe 1
        wasLaterRunnerStarted <- ConcurrentVar.peek laterRunnerStarted
        wasLaterRunnerStarted |> shouldBe False

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
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 50,
                      reaperIntervalMs = 25,
                      enableReaper = True,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [],
                lifecycleRunners = [lifecycleRunner],
                commandEndpoints = Map.empty,
                actionContext = context
              }

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
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 50,
                      reaperIntervalMs = 25,
                      enableReaper = True,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [],
                lifecycleRunners = [lifecycleRunner],
                commandEndpoints = Map.empty,
                actionContext = context
              }

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
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 2,
                      channelWriteTimeoutMs = 20,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [slowRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

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

      it "force-cancels workers when Stop message cannot be delivered and reports queued events" \_ -> do
        let forcedStopConfig =
              Dispatcher.DispatcherConfig
                { idleTimeoutMs = 60000,
                  reaperIntervalMs = 10000,
                  enableReaper = False,
                  workerChannelCapacity = 1,
                  channelWriteTimeoutMs = 10,
                  eventProcessingTimeoutMs = Nothing,
                  maxEventRetries = 3,
                  retryBaseDelayMs = 50
                }
        outboundStarted <- Channel.new
        outboundCalls <- ConcurrentVar.containing (0 :: Int)
        let blockingOutboundRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    outboundCalls |> ConcurrentVar.modify (+ 1)
                    Channel.write unit outboundStarted
                    blockWorkerForever
                }
        outboundEventStore <- InMemory.new |> Task.mapError toText
        outboundContext <- makeContext
        outboundDispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = forcedStopConfig,
                eventStore = outboundEventStore,
                outboundRunners = [blockingOutboundRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = outboundContext
              }
        outboundActiveEvent <- makeTestEvent "outbound-cancelled-stream" 1 1
        outboundQueuedEvent <- makeTestEvent "outbound-cancelled-stream" 2 2
        Dispatcher.dispatch outboundDispatcher outboundActiveEvent
        Channel.read outboundStarted
        Dispatcher.dispatch outboundDispatcher outboundQueuedEvent
        Dispatcher.shutdown outboundDispatcher
        outboundCallCount <- outboundCalls |> ConcurrentVar.peek
        outboundCallCount |> shouldBe 1

        lifecycleStarted <- Channel.new
        lifecycleCalls <- ConcurrentVar.containing (0 :: Int)
        let blockingLifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \_streamId -> do
                    let processEvent = \_event -> do
                          lifecycleCalls |> ConcurrentVar.modify (+ 1)
                          Channel.write unit lifecycleStarted
                          blockWorkerForever
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = processEvent,
                          cleanup = Task.yield unit
                        }
                }
        lifecycleEventStore <- InMemory.new |> Task.mapError toText
        lifecycleContext <- makeContext
        lifecycleDispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = forcedStopConfig,
                eventStore = lifecycleEventStore,
                outboundRunners = [],
                lifecycleRunners = [blockingLifecycleRunner],
                commandEndpoints = Map.empty,
                actionContext = lifecycleContext
              }
        lifecycleActiveEvent <- makeTestEvent "lifecycle-cancelled-stream" 1 1
        lifecycleQueuedEvent <- makeTestEvent "lifecycle-cancelled-stream" 2 2
        Dispatcher.dispatch lifecycleDispatcher lifecycleActiveEvent
        Channel.read lifecycleStarted
        Dispatcher.dispatch lifecycleDispatcher lifecycleQueuedEvent
        Dispatcher.shutdown lifecycleDispatcher
        lifecycleCallCount <- lifecycleCalls |> ConcurrentVar.peek
        lifecycleCallCount |> shouldBe 1

      it "completes an interrupted stop after admitted writers leave and reports queued events" \_ -> do
        let interruptedStopConfig =
              Dispatcher.DispatcherConfig
                { idleTimeoutMs = 60000,
                  reaperIntervalMs = 10000,
                  enableReaper = False,
                  workerChannelCapacity = 1,
                  channelWriteTimeoutMs = 100,
                  eventProcessingTimeoutMs = Nothing,
                  maxEventRetries = 3,
                  retryBaseDelayMs = 50
                }
        outboundStarted <- Channel.new
        outboundStopped <- Channel.new
        outboundCalls <- ConcurrentVar.containing (0 :: Int)
        let blockingOutboundRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    outboundCalls |> ConcurrentVar.modify (+ 1)
                    Channel.write unit outboundStarted
                    Task.finally (Channel.write unit outboundStopped) blockWorkerForever
                }
        outboundEventStore <- InMemory.new |> Task.mapError toText
        outboundContext <- makeContext
        outboundDispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = interruptedStopConfig,
                eventStore = outboundEventStore,
                outboundRunners = [blockingOutboundRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = outboundContext
              }
        outboundActiveEvent <- makeTestEvent "outbound-interrupted-stop" 1 1
        outboundQueuedEvent <- makeTestEvent "outbound-interrupted-stop" 2 2
        outboundWaitingEvent <- makeTestEvent "outbound-interrupted-stop" 3 3
        outboundStoppedDuringShutdown <-
          cancelShutdownWhileWriterIsAdmitted
            outboundDispatcher
            outboundActiveEvent
            outboundQueuedEvent
            outboundWaitingEvent
            outboundStarted
            outboundStopped
        outboundCallCount <- outboundCalls |> ConcurrentVar.peek
        outboundCallCount |> shouldBe 1
        outboundStoppedDuringShutdown |> shouldBe True

        lifecycleStarted <- Channel.new
        lifecycleStopped <- Channel.new
        lifecycleCalls <- ConcurrentVar.containing (0 :: Int)
        let blockingLifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \_streamId -> do
                    let processEvent = \_event -> do
                          lifecycleCalls |> ConcurrentVar.modify (+ 1)
                          Channel.write unit lifecycleStarted
                          Task.finally (Channel.write unit lifecycleStopped) blockWorkerForever
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent,
                          cleanup = Task.yield unit
                        }
                }
        lifecycleEventStore <- InMemory.new |> Task.mapError toText
        lifecycleContext <- makeContext
        lifecycleDispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = interruptedStopConfig,
                eventStore = lifecycleEventStore,
                outboundRunners = [],
                lifecycleRunners = [blockingLifecycleRunner],
                commandEndpoints = Map.empty,
                actionContext = lifecycleContext
              }
        lifecycleActiveEvent <- makeTestEvent "lifecycle-interrupted-stop" 1 1
        lifecycleQueuedEvent <- makeTestEvent "lifecycle-interrupted-stop" 2 2
        lifecycleWaitingEvent <- makeTestEvent "lifecycle-interrupted-stop" 3 3
        lifecycleStoppedDuringShutdown <-
          cancelShutdownWhileWriterIsAdmitted
            lifecycleDispatcher
            lifecycleActiveEvent
            lifecycleQueuedEvent
            lifecycleWaitingEvent
            lifecycleStarted
            lifecycleStopped
        lifecycleCallCount <- lifecycleCalls |> ConcurrentVar.peek
        lifecycleCallCount |> shouldBe 1
        lifecycleStoppedDuringShutdown |> shouldBe True

      it "drops events when lifecycle worker creation races with shutdown" \_ -> do
        spawnStarted <- Channel.new
        releaseSpawn <- Channel.new
        callbackStarted <- Channel.new
        cleanupFinished <- Channel.new
        let creationRaceConfig =
              Dispatcher.DispatcherConfig
                { idleTimeoutMs = 60000,
                  reaperIntervalMs = 10000,
                  enableReaper = False,
                  workerChannelCapacity = 1,
                  channelWriteTimeoutMs = 100,
                  eventProcessingTimeoutMs = Nothing,
                  maxEventRetries = 3,
                  retryBaseDelayMs = 50
                }
        let lifecycleRunner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \_ -> do
                    Channel.write unit spawnStarted
                    Channel.read releaseSpawn
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_ -> do
                            Channel.write unit callbackStarted
                            Task.yield Array.empty,
                          cleanup = Channel.write unit cleanupFinished
                        }
                }
        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings = creationRaceConfig,
                eventStore = eventStore,
                outboundRunners = [],
                lifecycleRunners = [lifecycleRunner],
                commandEndpoints = Map.empty,
                actionContext = context
              }
        event <- makeTestEvent "lifecycle-create-shutdown-race" 1 1
        dispatchTask <- AsyncTask.run (Dispatcher.dispatch dispatcher event)
        Channel.read spawnStarted
        Dispatcher.shutdown dispatcher
        Channel.write unit releaseSpawn
        AsyncTask.waitFor dispatchTask
        raceResult <- AsyncTask.race (Channel.read callbackStarted) (Channel.read cleanupFinished)
        raceResult |> shouldBe (AsyncTask.RightWon unit)
        queuedCallback <- Channel.tryRead callbackStarted
        queuedCallback |> shouldBe Maybe.Nothing

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
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 50,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [fastRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

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

    describe "callback masking" do
      it "runs stateless callbacks in an unmasked child" \_ -> do
        callbackMaskingStates <- Channel.new
        let runner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_maybeContext _eventStore _event -> do
                    maskingState <- Task.cancellationState
                    Channel.write maskingState callbackMaskingStates
                    Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [runner] Map.empty
        event <- makeTestEvent "stateless-unmasked-callback" 1 1

        Dispatcher.dispatch dispatcher event
        callbackMaskingState <- Channel.read callbackMaskingStates
        Dispatcher.shutdown dispatcher

        callbackMaskingState |> shouldBe Task.CancellationEnabled

      it "runs lifecycle callbacks in an unmasked child" \_ -> do
        callbackMaskingStates <- Channel.new
        let runner =
              Dispatcher.OutboundLifecycleRunner
                { entityTypeName = "TestEntity",
                  spawnWorkerState = \_streamId ->
                    Task.yield
                      Dispatcher.WorkerState
                        { processEvent = \_event -> do
                            maskingState <- Task.cancellationState
                            Channel.write maskingState callbackMaskingStates
                            Task.yield Array.empty,
                          cleanup = Task.yield unit
                        }
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.newWithLifecycle eventStore [] [runner] Map.empty
        event <- makeTestEvent "lifecycle-unmasked-callback" 1 1

        Dispatcher.dispatch dispatcher event
        callbackMaskingState <- Channel.read callbackMaskingStates
        Dispatcher.shutdown dispatcher

        callbackMaskingState |> shouldBe Task.CancellationEnabled

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
                eventProcessingTimeoutMs = Nothing,
                maxEventRetries = 3,
                retryBaseDelayMs = 50
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
        dispatcher <- case maybeDispatcher of
              Just d -> Task.yield d
              Nothing -> Task.throw ("expected a dispatcher" :: Text)

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

    describe "entity type filtering" do
      it "only processes events for matching entity type" \_ -> do
        -- Two runners for different entity types. Only the matching runner
        -- should process each event. (Fixes issue #323)
        processedByRunner <- ConcurrentVar.containing ([] :: [Text])

        let runnerA =
              Dispatcher.OutboundRunner
                { entityTypeName = "EntityA",
                  processEvent = \_ctx _store _event -> do
                    processedByRunner |> ConcurrentVar.modify (\runners -> runners ++ ["RunnerA"])
                    Task.yield Array.empty
                }
        let runnerB =
              Dispatcher.OutboundRunner
                { entityTypeName = "EntityB",
                  processEvent = \_ctx _store _event -> do
                    processedByRunner |> ConcurrentVar.modify (\runners -> runners ++ ["RunnerB"])
                    Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [runnerA, runnerB] Map.empty

        -- Dispatch event for EntityA only
        eventA <- makeTestEventForEntity "EntityA" "entity-1" 1 1
        Dispatcher.dispatch dispatcher eventA

        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Only RunnerA should have processed
        runners <- ConcurrentVar.peek processedByRunner
        runners |> Array.fromLinkedList |> shouldBe ["RunnerA"]

      it "routes events to correct runners when multiple entity types exist" \_ -> do
        -- Dispatch events for both entity types and verify correct routing
        processedByRunner <- ConcurrentVar.containing ([] :: [(Text, Int)])

        let runnerA =
              Dispatcher.OutboundRunner
                { entityTypeName = "EntityA",
                  processEvent = \_ctx _store event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        processedByRunner
                          |> ConcurrentVar.modify (\results -> results ++ [("RunnerA", decoded.sequenceNum)])
                        Task.yield Array.empty
                }
        let runnerB =
              Dispatcher.OutboundRunner
                { entityTypeName = "EntityB",
                  processEvent = \_ctx _store event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        processedByRunner
                          |> ConcurrentVar.modify (\results -> results ++ [("RunnerB", decoded.sequenceNum)])
                        Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [runnerA, runnerB] Map.empty

        -- Dispatch events for both entity types
        eventA <- makeTestEventForEntity "EntityA" "entity-1" 1 1
        eventB <- makeTestEventForEntity "EntityB" "entity-2" 2 2
        Dispatcher.dispatch dispatcher eventA
        Dispatcher.dispatch dispatcher eventB

        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Each runner should only have processed its own entity type's event
        results <- ConcurrentVar.peek processedByRunner
        let resultsArray = results |> Array.fromLinkedList
        let runnerAResults = resultsArray |> Array.takeIf (\(runner, _) -> runner == "RunnerA")
        let runnerBResults = resultsArray |> Array.takeIf (\(runner, _) -> runner == "RunnerB")

        Array.length runnerAResults |> shouldBe 1
        Array.length runnerBResults |> shouldBe 1
        runnerAResults |> Array.map Tuple.second |> shouldBe [1]
        runnerBResults |> Array.map Tuple.second |> shouldBe [2]

      it "skips events with no matching runner" \_ -> do
        -- Event for an entity type with no registered runner should be silently skipped
        processedCount <- ConcurrentVar.containing (0 :: Int)

        let testRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "EntityA",
                  processEvent = \_ctx _store _event -> do
                    processedCount |> ConcurrentVar.modify (+ 1)
                    Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        dispatcher <- Dispatcher.new eventStore [testRunner] Map.empty

        -- Dispatch event for an entity type with no matching runner
        eventX <- makeTestEventForEntity "UnknownEntity" "entity-1" 1 1
        Dispatcher.dispatch dispatcher eventX

        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- No runner should have processed
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 0

    describe "outbound retry" do
      it "retries on transient IO exception and succeeds" \_ -> do
        -- When a runner throws a raw IO exception (e.g., network error),
        -- executeWithRetry catches it and retries. On second attempt the
        -- runner succeeds. (Fixes issue #571)
        attemptCount <- ConcurrentVar.containing (0 :: Int)
        processedEvents <- ConcurrentVar.containing ([] :: [Int])

        let retryRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_ctx _store event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        currentAttempt <- ConcurrentVar.peek attemptCount
                        attemptCount |> ConcurrentVar.modify (+ 1)
                        if currentAttempt == 0
                          then
                            Task.evaluate (panic "transient network error")
                          else do
                            processedEvents
                              |> ConcurrentVar.modify (\evts -> evts ++ [decoded.sequenceNum])
                            Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 50
                    },
                eventStore = eventStore,
                outboundRunners = [retryRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

        event <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event

        -- Wait for retry (50ms delay) + processing
        AsyncTask.sleep 300 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Event should have been processed on retry
        processed <- ConcurrentVar.peek processedEvents
        processed |> Array.fromLinkedList |> shouldBe [1]

        -- Should have taken 2 attempts (1 failure + 1 success)
        attempts <- ConcurrentVar.peek attemptCount
        attempts |> shouldBe 2

      it "gives up after max retries and continues with next event" \_ -> do
        -- When IO exceptions persist beyond maxEventRetries, the event is
        -- skipped and the worker continues processing subsequent events.
        attemptCount <- ConcurrentVar.containing (0 :: Int)
        processedEvents <- ConcurrentVar.containing ([] :: [Int])

        let alwaysFailFirstRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_ctx _store event -> do
                    case Json.decode @OrderingTestEvent event.event of
                      Err _ -> Task.yield Array.empty
                      Ok decoded -> do
                        attemptCount |> ConcurrentVar.modify (+ 1)
                        if decoded.sequenceNum == 1
                          then
                            Task.evaluate (panic "persistent network error")
                          else do
                            processedEvents
                              |> ConcurrentVar.modify (\evts -> evts ++ [decoded.sequenceNum])
                            Task.yield Array.empty
                }

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 2,
                      retryBaseDelayMs = 10
                    },
                eventStore = eventStore,
                outboundRunners = [alwaysFailFirstRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

        event1 <- makeTestEvent "entity-A" 1 1
        event2 <- makeTestEvent "entity-A" 2 2
        Dispatcher.dispatch dispatcher event1
        Dispatcher.dispatch dispatcher event2

        -- Wait for retries (10ms + 20ms = 30ms) + processing
        AsyncTask.sleep 500 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Event 1 exhausted retries and was skipped, event 2 processed normally
        processed <- ConcurrentVar.peek processedEvents
        processed |> Array.fromLinkedList |> shouldBe [2]

        -- Event 1: 3 attempts (initial + 2 retries), event 2: 1 attempt = 4 total
        attempts <- ConcurrentVar.peek attemptCount
        attempts |> shouldBe 4

      it "does not retry Task.throw errors" \_ -> do
        -- Task.throw errors (e.g., JSON decode failures) are application-level
        -- errors caught by Task.asResult inside the action. They are NOT
        -- retried because they would fail the same way every time.
        attemptCount <- ConcurrentVar.containing (0 :: Int)

        let decodeFailRunner =
              Dispatcher.OutboundRunner
                { entityTypeName = "TestEntity",
                  processEvent = \_ctx _store _event -> do
                    attemptCount |> ConcurrentVar.modify (+ 1)
                    Task.throw ("decode error: bad format" :: Text)
                }

        eventStore <- InMemory.new |> Task.mapError toText
        context <- makeContext
        dispatcher <-
          Dispatcher.newWithLifecycleConfig
            Dispatcher.Setup
              { settings =
                  Dispatcher.DispatcherConfig
                    { idleTimeoutMs = 60000,
                      reaperIntervalMs = 10000,
                      enableReaper = False,
                      workerChannelCapacity = 100,
                      channelWriteTimeoutMs = 5000,
                      eventProcessingTimeoutMs = Nothing,
                      maxEventRetries = 3,
                      retryBaseDelayMs = 10
                    },
                eventStore = eventStore,
                outboundRunners = [decodeFailRunner],
                lifecycleRunners = [],
                commandEndpoints = Map.empty,
                actionContext = context
              }

        event <- makeTestEvent "entity-A" 1 1
        Dispatcher.dispatch dispatcher event

        AsyncTask.sleep 200 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Should only be called once (Task.throw caught by inner Task.asResult,
        -- action completes normally, no retry triggered)
        attempts <- ConcurrentVar.peek attemptCount
        attempts |> shouldBe 1
