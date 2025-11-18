module Test.Service.EventStore.Subscriptions.Spec (spec) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Maybe qualified
import Result qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.EventStore.Core (BankAccountEvent, newInsertion)
import Test.Service.EventStore.Subscriptions.Context (Context (..))
import Test.Service.EventStore.Subscriptions.Context qualified as Context
import Uuid qualified


-- Helper function to filter events by entity name to avoid cross-test interference
entityFilteredSubscriber ::
  Event.EntityName ->
  (Event BankAccountEvent -> Task Text Unit) ->
  Event BankAccountEvent ->
  Task Text Unit
entityFilteredSubscriber targetEntity handler event = do
  if event.entityName == targetEntity
    then handler event
    else Task.yield unit


spec :: Task Text (EventStore BankAccountEvent) -> Spec Unit
spec newStore = do
  describe "Event Store Subscriptions" do
    before (Context.initialize newStore) do
      it "allows subscribing to all events and receives them when appended" \context -> do
        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define subscriber function that collects events
        let collectEvent event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
        let subscriber = entityFilteredSubscriber context.entityName collectEvent

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Insert some test events
        let insertEvent event = do
              context.store.insert event
                |> Task.mapError toText

        context.testEvents
          |> Array.take 3
          |> Task.mapArray insertEvent
          |> discard

        -- Wait briefly for async notifications to complete
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Check that we received the events
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 3

        -- Verify the events are in correct order
        received
          |> Array.get 0
          |> shouldSatisfy
            ( \maybeEvent ->
                case maybeEvent of
                  Just event -> event.metadata.localPosition |> Maybe.getOrDie |> (==) (Event.StreamPosition 0)
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "allows subscribing to specific entity events only" \context -> do
        -- Create another entity and stream to test filtering
        otherEntityNameText <- Uuid.generate |> Task.map toText
        let otherEntityName = Event.EntityName otherEntityNameText
        otherStreamId <- StreamId.new
        otherInsertion <- newInsertion 0
        let otherEvent =
              Event.InsertionPayload
                { streamId = otherStreamId,
                  entityName = otherEntityName,
                  insertionType = Event.AnyStreamState,
                  insertions = Array.fromLinkedList [otherInsertion]
                }

        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define subscriber function that collects events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield unit :: Task Text Unit

        -- Subscribe only to our test entity events
        subscriptionId <-
          context.store.subscribeToEntityEvents context.entityName subscriber
            |> Task.mapError toText

        -- Insert event for our entity (use position 3 since first test used 0,1,2)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.insert event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Insert event for other entity (should be filtered out)
        context.store.insert otherEvent
          |> Task.mapError toText
          |> discard

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Check that we only received events for our entity
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 1

        received
          |> Array.get 0
          |> shouldSatisfy
            ( \maybeEvent ->
                case maybeEvent of
                  Just event -> event.entityName == context.entityName
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "allows subscribing to specific stream events only" \context -> do
        -- Create another stream to test filtering
        otherStreamId <- StreamId.new
        otherInsertion <- newInsertion 0
        let otherEvent =
              Event.InsertionPayload
                { streamId = otherStreamId,
                  entityName = context.entityName,
                  insertionType = Event.AnyStreamState,
                  insertions = Array.fromLinkedList [otherInsertion]
                }

        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define subscriber function that collects events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield unit :: Task Text Unit

        -- Subscribe only to our test stream events
        subscriptionId <-
          context.store.subscribeToStreamEvents context.entityName context.streamId subscriber
            |> Task.mapError toText

        -- Insert event for our stream (use position 4 since previous tests used 0,1,2,3)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.insert event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Insert event for other stream (should be filtered out)
        context.store.insert otherEvent
          |> Task.mapError toText
          |> discard

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Check that we only received events for our stream
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 1

        received
          |> Array.get 0
          |> shouldSatisfy
            ( \maybeEvent ->
                case maybeEvent of
                  Just event -> event.streamId == context.streamId && event.entityName == context.entityName
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "handles subscription errors gracefully without affecting event store operations" \context -> do
        -- Create a subscriber that always fails
        let failingHandler _event = do
              Task.throw "Subscriber intentionally failed" :: Task Text Unit
        let failingSubscriber = entityFilteredSubscriber context.entityName failingHandler

        -- Subscribe with failing function
        subscriptionId <-
          context.store.subscribeToAllEvents failingSubscriber
            |> Task.mapError toText

        -- Insert events - should succeed despite failing subscriber (use position 5)
        insertResult <- case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.insert event |> Task.asResult
          Nothing -> do
            Task.yield (Err (EventStore.StorageFailure "No test event"))

        -- Event store operation should succeed
        insertResult
          |> shouldSatisfy Result.isOk

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Event should still be readable from store
        events <-
          context.store.readStreamForwardFrom
            context.entityName
            context.streamId
            (Event.StreamPosition 0)
            (EventStore.Limit 1)
            |> Task.mapError toText
            |> Task.andThen Stream.toArray

        events
          |> EventStore.collectStreamEvents
          |> Array.length
          |> shouldBe 1

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "supports multiple concurrent subscribers without interference" \context -> do
        -- Create multiple shared variables for different subscribers
        subscriber1Events <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))
        subscriber2Events <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))
        subscriber3Events <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define different subscriber functions
        let collectToVar1 event = subscriber1Events |> ConcurrentVar.modify (Array.push event)
        let collectToVar2 event = subscriber2Events |> ConcurrentVar.modify (Array.push event)
        let collectToVar3 event = subscriber3Events |> ConcurrentVar.modify (Array.push event)
        let subscriber1 = entityFilteredSubscriber context.entityName collectToVar1
        let subscriber2 = entityFilteredSubscriber context.entityName collectToVar2
        let subscriber3 = entityFilteredSubscriber context.entityName collectToVar3

        -- Subscribe all three concurrently
        subscriptionIds <-
          AsyncTask.runConcurrently
            ( context.store.subscribeToAllEvents subscriber1 |> Task.mapError toText,
              AsyncTask.runConcurrently
                ( context.store.subscribeToAllEvents subscriber2 |> Task.mapError toText,
                  context.store.subscribeToAllEvents subscriber3 |> Task.mapError toText
                )
            )

        let (sub1, (sub2, sub3)) = subscriptionIds

        -- Insert multiple events
        adjustedEvents <- case (context.testEvents |> Array.get 0, context.testEvents |> Array.get 1, context.testEvents |> Array.get 2) of
          (Just event1, Just event2, Just event3) -> do
            Task.yield
              (Array.fromLinkedList [event1, event2, event3])
          _ -> do
            Task.throw "Not enough test events"
        let insertEvent event = do
              context.store.insert event
                |> Task.mapError toText
        adjustedEvents
          |> Task.mapArray insertEvent
          |> discard

        -- Wait for async processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- All subscribers should have received all events
        received1 <- ConcurrentVar.get subscriber1Events
        received2 <- ConcurrentVar.get subscriber2Events
        received3 <- ConcurrentVar.get subscriber3Events

        received1 |> Array.length |> shouldBe 3
        received2 |> Array.length |> shouldBe 3
        received3 |> Array.length |> shouldBe 3

        -- Clean up all subscriptions
        AsyncTask.runConcurrently
          ( context.store.unsubscribe sub1 |> Task.mapError toText,
            AsyncTask.runConcurrently
              ( context.store.unsubscribe sub2 |> Task.mapError toText,
                context.store.unsubscribe sub3 |> Task.mapError toText
              )
          )
          |> discard

      it "stops delivering events after unsubscription" \context -> do
        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define subscriber function
        let collectEvent event = receivedEvents |> ConcurrentVar.modify (Array.push event)
        let subscriber = entityFilteredSubscriber context.entityName collectEvent

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Insert first event (use position 9)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.insert event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Wait for processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Unsubscribe
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

        -- Insert second event after unsubscription (use position 10)
        case context.testEvents |> Array.get 1 of
          Just event -> do
            context.store.insert event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Wait for potential processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Should only have received the first event
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 1

      it "handles high-frequency event publishing without data loss" \context -> do
        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        -- Define subscriber function that tracks events
        let collectEvent event = receivedEvents |> ConcurrentVar.modify (Array.push event)
        let subscriber = entityFilteredSubscriber context.entityName collectEvent

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Rapidly insert many events
        let rapidEventCount = 50
        rapidEvents <- createRapidTestEventsFromPosition context.streamId context.entityName rapidEventCount 0

        -- Insert all events as quickly as possible
        let insertEvent event = do
              context.store.insert event
                |> Task.mapError toText
        rapidEvents
          |> Task.mapArray insertEvent
          |> discard

        -- Wait for all async processing to complete
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify all events were received
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe rapidEventCount

        -- Verify events are in order (starting from position 0 in this stream)
        received
          |> Array.indexed
          |> Task.forEach
            ( \(index, event) -> do
                event.metadata.localPosition
                  |> Maybe.getOrDie
                  |> shouldBe (Event.StreamPosition (index |> fromIntegral))
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "subscribes from now on - only receives events inserted after subscription" \context -> do
        entity1IdText <- Uuid.generate |> Task.map toText
        let entity1Id = Event.EntityName entity1IdText
        entity2IdText <- Uuid.generate |> Task.map toText
        let entity2Id = Event.EntityName entity2IdText
        stream1Id <- StreamId.new
        stream2Id <- StreamId.new

        -- Insert events for both entities BEFORE subscribing (these should NOT be received)
        let eventCount = 5
        preSubscriptionEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount 0
        preSubscriptionEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount 0

        -- Insert pre-subscription events
        let insertEvent event = context.store.insert event |> Task.mapError toText
        preSubscriptionEvents1 |> Task.mapArray insertEvent |> discard
        preSubscriptionEvents2 |> Task.mapArray insertEvent |> discard

        -- Now set up subscription tracking
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        let subscriber event = do
              -- Only track events from our test entities
              if event.entityName == entity1Id || event.entityName == entity2Id
                then receivedEvents |> ConcurrentVar.modify (Array.push event)
                else Task.yield unit
              Task.yield unit :: Task Text Unit

        -- Subscribe to all events (this should only receive NEW events "from now on")
        subscriptionId <- context.store.subscribeToAllEvents subscriber |> Task.mapError toText

        -- Wait a bit to ensure subscription is active
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Insert events for both entities AFTER subscribing (these SHOULD be received)
        postSubscriptionEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount eventCount
        postSubscriptionEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount eventCount

        -- Insert post-subscription events
        postSubscriptionEvents1 |> Task.mapArray insertEvent |> discard
        postSubscriptionEvents2 |> Task.mapArray insertEvent |> discard

        -- Wait for events to be processed
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify we received exactly the post-subscription events
        received <- ConcurrentVar.get receivedEvents

        -- Should have received exactly eventCount * 2 events (only post-subscription)
        received |> Array.length |> shouldBe (eventCount * 2)

        -- Events should be properly ordered within each entity
        let entity1Events = received |> Array.takeIf (\event -> event.entityName == entity1Id)
        let entity2Events = received |> Array.takeIf (\event -> event.entityName == entity2Id)

        entity1Events |> Array.length |> shouldBe eventCount
        entity2Events |> Array.length |> shouldBe eventCount

        -- Clean up subscription
        context.store.unsubscribe subscriptionId |> Task.mapError toText |> discard

      it "subscribes from specific position - receives events after specified global position" \context -> do
        entity1IdText <- Uuid.generate |> Task.map toText
        let entity1Id = Event.EntityName entity1IdText
        entity2IdText <- Uuid.generate |> Task.map toText
        let entity2Id = Event.EntityName entity2IdText
        stream1Id <- StreamId.new
        stream2Id <- StreamId.new

        -- Insert FIRST batch of events to create historical data
        let eventCount = 5
        firstBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount 0

        -- Insert first batch and capture the global position
        let insertEvent event = context.store.insert event |> Task.mapError toText
        firstBatchResults <-
          firstBatchEvents1 |> Task.mapArray (\event -> context.store.insert event |> Task.mapError toText)

        -- Get the global position after the first batch (this is our subscription point)
        positionAfterFirstBatch <- case firstBatchResults |> Array.last of
          Just lastEvent -> Task.yield lastEvent.globalPosition
          Nothing -> Task.throw "No events in first batch"

        -- Set up subscription tracking
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        let subscriber event = do
              -- Only track events from our test entities
              if event.entityName == entity1Id || event.entityName == entity2Id
                then receivedEvents |> ConcurrentVar.modify (Array.push event)
                else Task.yield unit
              Task.yield unit :: Task Text Unit

        -- Subscribe from the specific position (should receive events AFTER this position)
        subscriptionId <-
          context.store.subscribeToAllEventsFromPosition positionAfterFirstBatch subscriber |> Task.mapError toText

        -- Wait a bit for subscription to process historical events
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Insert SECOND batch of events (these should definitely be received)
        secondBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount eventCount
        secondBatchEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount 0

        -- Insert second batch
        secondBatchEvents1 |> Task.mapArray insertEvent |> discard
        secondBatchEvents2 |> Task.mapArray insertEvent |> discard

        -- Wait for events to be processed
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify we received exactly eventCount * 2 events (from second batch only)
        received <- ConcurrentVar.get receivedEvents

        let (Event.StreamPosition expectedAfterPos) = positionAfterFirstBatch

        -- Should have received exactly eventCount * 2 events (second batch only)
        received |> Array.length |> shouldBe (eventCount * 2)

        -- All received events should have global positions AFTER our subscription position
        let eventsWithWrongPosition =
              received
                |> Array.takeIf
                  ( \event -> do
                      let (Event.StreamPosition eventGlobalPos) = event.metadata.globalPosition |> Maybe.getOrDie
                      eventGlobalPos <= expectedAfterPos
                  )

        eventsWithWrongPosition |> Array.length |> shouldBe 0

        -- Events should be properly partitioned by entity
        let entity1Events = received |> Array.takeIf (\event -> event.entityName == entity1Id)
        let entity2Events = received |> Array.takeIf (\event -> event.entityName == entity2Id)

        entity1Events |> Array.length |> shouldBe eventCount
        entity2Events |> Array.length |> shouldBe eventCount

        -- Clean up subscription
        context.store.unsubscribe subscriptionId |> Task.mapError toText |> discard

      it "subscribes from start - receives ALL events including historical ones" \context -> do
        entity1IdText <- Uuid.generate |> Task.map toText
        let entity1Id = Event.EntityName entity1IdText
        entity2IdText <- Uuid.generate |> Task.map toText
        let entity2Id = Event.EntityName entity2IdText
        stream1Id <- StreamId.new
        stream2Id <- StreamId.new

        -- Insert FIRST batch of events BEFORE subscribing (historical data)
        let eventCount = 5
        firstBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount 0
        firstBatchEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount 0

        -- Insert first batch
        let insertEvent event = context.store.insert event |> Task.mapError toText
        firstBatchEvents1 |> Task.mapArray insertEvent |> discard
        firstBatchEvents2 |> Task.mapArray insertEvent |> discard

        -- Set up subscription tracking
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array (Event BankAccountEvent))

        let subscriber event = do
              -- Only track events from our test entities
              if event.entityName == entity1Id || event.entityName == entity2Id
                then receivedEvents |> ConcurrentVar.modify (Array.push event)
                else Task.yield unit
              Task.yield unit :: Task Text Unit

        -- Subscribe from the START (should receive ALL events including historical ones)
        subscriptionId <-
          context.store.subscribeToAllEventsFromStart subscriber |> Task.mapError toText

        -- Wait for subscription to process historical events (catch-up phase)
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Insert SECOND batch of events AFTER subscribing (live events)
        secondBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount eventCount
        secondBatchEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount eventCount

        -- Insert second batch
        secondBatchEvents1 |> Task.mapArray insertEvent |> discard
        secondBatchEvents2 |> Task.mapArray insertEvent |> discard

        -- Wait for live events to be processed
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify we received ALL events (both historical and live)
        received <- ConcurrentVar.get receivedEvents

        -- Should have received eventCount * 4 total events
        -- (first batch: 5 entity1 + 5 entity2, second batch: 5 entity1 + 5 entity2)
        received |> Array.length |> shouldBe (eventCount * 4)

        -- Events should be properly partitioned by entity
        let entity1Events = received |> Array.takeIf (\event -> event.entityName == entity1Id)
        let entity2Events = received |> Array.takeIf (\event -> event.entityName == entity2Id)

        entity1Events |> Array.length |> shouldBe (eventCount * 2)
        entity2Events |> Array.length |> shouldBe (eventCount * 2)

        -- Clean up subscription
        context.store.unsubscribe subscriptionId |> Task.mapError toText |> discard


createTestEventsForEntity ::
  Event.StreamId -> Event.EntityName -> Int -> Int -> Task _ (Array (Event.InsertionPayload BankAccountEvent))
createTestEventsForEntity streamId entityName count startPosition = do
  let createEvent position = do
        insertions <- Array.fromLinkedList [position] |> Task.mapArray newInsertion
        Task.yield
          Event.InsertionPayload
            { streamId,
              entityName,
              insertionType = Event.AnyStreamState,
              insertions
            }

  Array.fromLinkedList [startPosition .. (startPosition + count - 1)]
    |> Task.mapArray createEvent


createRapidTestEventsFromPosition ::
  Event.StreamId -> Event.EntityName -> Int -> Int -> Task _ (Array (Event.InsertionPayload BankAccountEvent))
createRapidTestEventsFromPosition streamId entityName count startPosition = do
  let createEvent position = do
        insertions <- Array.fromLinkedList [position] |> Task.mapArray newInsertion
        Task.yield
          Event.InsertionPayload
            { streamId,
              entityName,
              insertionType = Event.AnyStreamState,
              insertions
            }

  Array.fromLinkedList [startPosition .. (startPosition + count - 1)]
    |> Task.mapArray createEvent
