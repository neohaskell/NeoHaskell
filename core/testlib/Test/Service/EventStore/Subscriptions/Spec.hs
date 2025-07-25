module Test.Service.EventStore.Subscriptions.Spec (spec) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Result qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Subscriptions.Context (Context (..))
import Test.Service.EventStore.Subscriptions.Context qualified as Context
import Uuid qualified


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Event Store Subscriptions" do
    beforeAll (Context.initialize newStore) do
      it "allows subscribing to all events and receives them when appended" \context -> do
        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define subscriber function that collects events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Append some test events
        let appendEvent event = do
              context.store.appendToStream event
                |> Task.mapError toText
        context.testEvents
          |> Array.take 3
          |> Task.mapArray appendEvent
          |> discard

        -- Wait briefly for async notifications to complete
        AsyncTask.sleep 10 |> Task.mapError (\_ -> "timeout")

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
                  Just event -> event.localPosition == Event.StreamPosition 0
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "allows subscribing to specific entity events only" \context -> do
        -- Create another entity and stream to test filtering
        otherEntityId <- Uuid.generate |> Task.map Event.EntityId
        otherStreamId <- Uuid.generate |> Task.map Event.StreamId
        otherEventId <- Uuid.generate
        let otherEvent =
              Event.InsertionEvent
                { id = otherEventId,
                  streamId = otherStreamId,
                  entityId = otherEntityId,
                  localPosition = Event.StreamPosition 0
                }

        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define subscriber function that collects events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe only to our test entity events
        subscriptionId <-
          context.store.subscribeToEntityEvents context.entityId subscriber
            |> Task.mapError toText

        -- Append event for our entity (use position 3 since first test used 0,1,2)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            let adjustedEvent =
                  Event.InsertionEvent
                    { id = event.id,
                      streamId = event.streamId,
                      entityId = event.entityId,
                      localPosition = Event.StreamPosition 3
                    }
            context.store.appendToStream adjustedEvent
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Append event for other entity (should be filtered out)
        context.store.appendToStream otherEvent
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
                  Just event -> event.entityId == context.entityId
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "allows subscribing to specific stream events only" \context -> do
        -- Create another stream to test filtering
        otherStreamId <- Uuid.generate |> Task.map Event.StreamId
        otherEventId <- Uuid.generate
        let otherEvent =
              Event.InsertionEvent
                { id = otherEventId,
                  streamId = otherStreamId,
                  entityId = context.entityId,
                  localPosition = Event.StreamPosition 0
                }

        -- Create a shared variable to collect received events
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define subscriber function that collects events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe only to our test stream events
        subscriptionId <-
          context.store.subscribeToStreamEvents context.entityId context.streamId subscriber
            |> Task.mapError toText

        -- Append event for our stream (use position 4 since previous tests used 0,1,2,3)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            let adjustedEvent =
                  Event.InsertionEvent
                    { id = event.id,
                      streamId = event.streamId,
                      entityId = event.entityId,
                      localPosition = Event.StreamPosition 4
                    }
            context.store.appendToStream adjustedEvent
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Append event for other stream (should be filtered out)
        context.store.appendToStream otherEvent
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
                  Just event -> event.streamId == context.streamId && event.entityId == context.entityId
                  Nothing -> False
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "handles subscription errors gracefully without affecting event store operations" \context -> do
        -- Create a subscriber that always fails
        let failingSubscriber _event = do
              Task.throw (EventStore.StorageFailure "Subscriber intentionally failed") :: Task EventStore.Error Unit

        -- Subscribe with failing function
        subscriptionId <-
          context.store.subscribeToAllEvents failingSubscriber
            |> Task.mapError toText

        -- Append events - should succeed despite failing subscriber (use position 5)
        appendResult <- case context.testEvents |> Array.get 0 of
          Just event -> do
            let adjustedEvent =
                  Event.InsertionEvent
                    { id = event.id,
                      streamId = event.streamId,
                      entityId = event.entityId,
                      localPosition = Event.StreamPosition 5
                    }
            context.store.appendToStream adjustedEvent |> Task.asResult
          Nothing -> do
            Task.yield (Err (EventStore.StorageFailure "No test event"))

        -- Event store operation should succeed
        appendResult
          |> shouldSatisfy Result.isOk

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Event should still be readable from store
        events <-
          context.store.readStreamForwardFrom
            context.entityId
            context.streamId
            (Event.StreamPosition 0)
            (EventStore.Limit 1)
            |> Task.mapError toText

        events
          |> Array.length
          |> shouldBe 1

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "supports multiple concurrent subscribers without interference" \context -> do
        -- Create multiple shared variables for different subscribers
        subscriber1Events <- ConcurrentVar.containing (Array.empty :: Array Event)
        subscriber2Events <- ConcurrentVar.containing (Array.empty :: Array Event)
        subscriber3Events <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define different subscriber functions
        let subscriber1 event = do
              subscriber1Events |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit
        let subscriber2 event = do
              subscriber2Events |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit
        let subscriber3 event = do
              subscriber3Events |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

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

        -- Append multiple events (use positions 6, 7, 8)
        let createAdjustedEvent baseEvent position = do
              Event.InsertionEvent
                { id = baseEvent.id,
                  streamId = baseEvent.streamId,
                  entityId = baseEvent.entityId,
                  localPosition = Event.StreamPosition position
                }
        adjustedEvents <- case (context.testEvents |> Array.get 0, context.testEvents |> Array.get 1, context.testEvents |> Array.get 2) of
          (Just event1, Just event2, Just event3) -> do
            Task.yield
              (Array.fromLinkedList [createAdjustedEvent event1 6, createAdjustedEvent event2 7, createAdjustedEvent event3 8])
          _ -> do
            Task.throw "Not enough test events"
        let appendEvent event = do
              context.store.appendToStream event
                |> Task.mapError toText
        adjustedEvents
          |> Task.mapArray appendEvent
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
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define subscriber function
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Append first event (use position 9)
        case context.testEvents |> Array.get 0 of
          Just event -> do
            let adjustedEvent =
                  Event.InsertionEvent
                    { id = event.id,
                      streamId = event.streamId,
                      entityId = event.entityId,
                      localPosition = Event.StreamPosition 9
                    }
            context.store.appendToStream adjustedEvent
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

        -- Append second event after unsubscription (use position 10)
        case context.testEvents |> Array.get 1 of
          Just event -> do
            let adjustedEvent =
                  Event.InsertionEvent
                    { id = event.id,
                      streamId = event.streamId,
                      entityId = event.entityId,
                      localPosition = Event.StreamPosition 10
                    }
            context.store.appendToStream adjustedEvent
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
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        -- Define subscriber function that tracks events
        let subscriber event = do
              receivedEvents |> ConcurrentVar.modify (Array.push event)
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe to all events
        subscriptionId <-
          context.store.subscribeToAllEvents subscriber
            |> Task.mapError toText

        -- Rapidly append many events (start from position 11)
        let rapidEventCount = 50
        rapidEvents <- createRapidTestEventsFromPosition context.streamId context.entityId rapidEventCount 11

        -- Append all events as quickly as possible
        let appendEvent event = do
              context.store.appendToStream event
                |> Task.mapError toText
        rapidEvents
          |> Task.mapArray appendEvent
          |> discard

        -- Wait for all async processing to complete
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify all events were received
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe rapidEventCount

        -- Verify events are in order (starting from position 11)
        received
          |> Array.indexed
          |> Task.forEach
            ( \(index, event) -> do
                event.localPosition
                  |> shouldBe (Event.StreamPosition (11 + index))
            )

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "subscribes from now on - only receives events appended after subscription" \context -> do
        entity1Id <- Uuid.generate |> Task.map Event.EntityId
        entity2Id <- Uuid.generate |> Task.map Event.EntityId
        stream1Id <- Uuid.generate |> Task.map Event.StreamId
        stream2Id <- Uuid.generate |> Task.map Event.StreamId

        -- Insert events for both entities BEFORE subscribing (these should NOT be received)
        let eventCount = 5
        preSubscriptionEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount 0
        preSubscriptionEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount 0

        -- Append pre-subscription events
        let appendEvent event = context.store.appendToStream event |> Task.mapError toText
        preSubscriptionEvents1 |> Task.mapArray appendEvent |> discard
        preSubscriptionEvents2 |> Task.mapArray appendEvent |> discard

        -- Now set up subscription tracking
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        let subscriber event = do
              -- Only track events from our test entities
              if event.entityId == entity1Id || event.entityId == entity2Id
                then receivedEvents |> ConcurrentVar.modify (Array.push event)
                else Task.yield ()
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe to all events (this should only receive NEW events "from now on")
        subscriptionId <- context.store.subscribeToAllEvents subscriber |> Task.mapError toText

        -- Wait a bit to ensure subscription is active
        AsyncTask.sleep 10 |> Task.mapError (\_ -> "timeout")

        -- Insert events for both entities AFTER subscribing (these SHOULD be received)
        postSubscriptionEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount eventCount
        postSubscriptionEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount eventCount

        -- Append post-subscription events
        postSubscriptionEvents1 |> Task.mapArray appendEvent |> discard
        postSubscriptionEvents2 |> Task.mapArray appendEvent |> discard

        -- Wait for events to be processed
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "timeout")

        -- Verify we received exactly the post-subscription events
        received <- ConcurrentVar.get receivedEvents

        -- Should have received exactly eventCount * 2 events (only post-subscription)
        received |> Array.length |> shouldBe (eventCount * 2)

        -- Events should be properly ordered within each entity
        let entity1Events = received |> Array.takeIf (\event -> event.entityId == entity1Id)
        let entity2Events = received |> Array.takeIf (\event -> event.entityId == entity2Id)

        entity1Events |> Array.length |> shouldBe eventCount
        entity2Events |> Array.length |> shouldBe eventCount

        -- Clean up subscription
        context.store.unsubscribe subscriptionId |> Task.mapError toText |> discard

      it "subscribes from specific position - receives events after specified global position" \context -> do
        entity1Id <- Uuid.generate |> Task.map Event.EntityId
        entity2Id <- Uuid.generate |> Task.map Event.EntityId
        stream1Id <- Uuid.generate |> Task.map Event.StreamId
        stream2Id <- Uuid.generate |> Task.map Event.StreamId

        -- Insert FIRST batch of events to create historical data
        let eventCount = 5
        firstBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount 0

        -- Append first batch and capture the global position
        let appendEvent event = context.store.appendToStream event |> Task.mapError toText
        firstBatchResults <-
          firstBatchEvents1 |> Task.mapArray (\event -> context.store.appendToStream event |> Task.mapError toText)

        -- Get the global position after the first batch (this is our subscription point)
        positionAfterFirstBatch <- case firstBatchResults |> Array.last of
          Just lastEvent -> Task.yield lastEvent.globalPosition
          Nothing -> Task.throw "No events in first batch"

        -- Set up subscription tracking
        receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

        let subscriber event = do
              -- Only track events from our test entities
              if event.entityId == entity1Id || event.entityId == entity2Id
                then receivedEvents |> ConcurrentVar.modify (Array.push event)
                else Task.yield ()
              Task.yield () :: Task EventStore.Error Unit

        -- Subscribe from the specific position (should receive events AFTER this position)
        subscriptionId <-
          context.store.subscribeToAllEventsFromPosition positionAfterFirstBatch subscriber |> Task.mapError toText

        -- Wait a bit for subscription to process historical events
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "timeout")

        -- Insert SECOND batch of events (these should definitely be received)
        secondBatchEvents1 <- createTestEventsForEntity stream1Id entity1Id eventCount eventCount
        secondBatchEvents2 <- createTestEventsForEntity stream2Id entity2Id eventCount 0

        -- Append second batch
        secondBatchEvents1 |> Task.mapArray appendEvent |> discard
        secondBatchEvents2 |> Task.mapArray appendEvent |> discard

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
                      let (Event.StreamPosition eventGlobalPos) = event.globalPosition
                      eventGlobalPos <= expectedAfterPos
                  )

        eventsWithWrongPosition |> Array.length |> shouldBe 0

        -- Events should be properly partitioned by entity
        let entity1Events = received |> Array.takeIf (\event -> event.entityId == entity1Id)
        let entity2Events = received |> Array.takeIf (\event -> event.entityId == entity2Id)

        entity1Events |> Array.length |> shouldBe eventCount
        entity2Events |> Array.length |> shouldBe eventCount

        -- Clean up subscription
        context.store.unsubscribe subscriptionId |> Task.mapError toText |> discard


createTestEventsForEntity :: Event.StreamId -> Event.EntityId -> Int -> Int -> Task _ (Array Event.InsertionEvent)
createTestEventsForEntity streamId entityId count startPosition = do
  let createEvent position = do
        eventId <- Uuid.generate
        Task.yield
          Event.InsertionEvent
            { id = eventId,
              streamId,
              entityId,
              localPosition = Event.StreamPosition position
            }

  Array.fromLinkedList [startPosition .. (startPosition + count - 1)]
    |> Task.mapArray createEvent


createRapidTestEventsFromPosition ::
  Event.StreamId -> Event.EntityId -> Int -> Int -> Task _ (Array Event.InsertionEvent)
createRapidTestEventsFromPosition streamId entityId count startPosition = do
  let createEvent position = do
        eventId <- Uuid.generate
        Task.yield
          Event.InsertionEvent
            { id = eventId,
              streamId,
              entityId,
              localPosition = Event.StreamPosition position
            }

  Array.fromLinkedList [startPosition .. (startPosition + count - 1)]
    |> Task.mapArray createEvent
