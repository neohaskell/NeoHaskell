module Test.Service.EventStore.Subscriptions.Spec (spec) where

import Array qualified
import ConcurrentVar qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Subscriptions.Context (Context (..))
import Test.Service.EventStore.Subscriptions.Context qualified as Context


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

        -- Check that we received the events (they should be there immediately with synchronous notification)
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

{- it "allows subscribing to specific entity events only" \context -> do
        -- Create another entity to test filtering
        otherEntityId <- Uuid.generate |> Task.map Event.EntityId
        otherEventId <- Uuid.generate
        let otherEvent = Event.InsertionEvent
              { id = otherEventId,
                streamId = context.streamId,
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
        subscriptionId <- context.store.subscribeToEntityEvents context.entityId subscriber
          |> Task.mapError toText

        -- Append event for our entity
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.appendToStream event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Append event for other entity (should be filtered out)
        context.store.appendToStream otherEvent
          |> Task.mapError toText
          |> discard

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError toText

        -- Check that we only received events for our entity
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 1

        received
          |> Array.get 0
          |> shouldSatisfy (\maybeEvent ->
              case maybeEvent of
                Just event -> event.entityId == context.entityId
                Nothing -> False)

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard
-}
{- it "allows subscribing to specific stream events only" \context -> do
        -- Create another stream to test filtering
        otherStreamId <- Uuid.generate |> Task.map Event.StreamId
        otherEventId <- Uuid.generate
        let otherEvent = Event.InsertionEvent
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
        subscriptionId <- context.store.subscribeToStreamEvents context.entityId context.streamId subscriber
          |> Task.mapError toText

        -- Append event for our stream
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.appendToStream event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Append event for other stream (should be filtered out)
        context.store.appendToStream otherEvent
          |> Task.mapError toText
          |> discard

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError toText

        -- Check that we only received events for our stream
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe 1

        received
          |> Array.get 0
          |> shouldSatisfy (\maybeEvent ->
              case maybeEvent of
                Just event -> event.streamId == context.streamId && event.entityId == context.entityId
                Nothing -> False)

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

      it "handles subscription errors gracefully without affecting event store operations" \context -> do
        -- Create a subscriber that always fails
        let failingSubscriber _event = do
              Task.throw (EventStore.StorageFailure "Subscriber intentionally failed") :: Task EventStore.Error Unit

        -- Subscribe with failing function
        subscriptionId <- context.store.subscribeToAllEvents failingSubscriber
          |> Task.mapError toText

        -- Append events - should succeed despite failing subscriber
        appendResult <- case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.appendToStream event |> Task.asResult
          Nothing -> do
            Task.yield (Err (EventStore.StorageFailure "No test event"))

        -- Event store operation should succeed
        appendResult
          |> shouldSatisfy (\result -> case result of
              Ok _ -> True
              Err _ -> False)

        -- Wait a bit for async processing
        AsyncTask.sleep 50 |> Task.mapError toText

        -- Event should still be readable from store
        events <- context.store.readStreamForwardFrom
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
        subscriptionIds <- AsyncTask.runConcurrently
          ( context.store.subscribeToAllEvents subscriber1 |> Task.mapError toText,
            AsyncTask.runConcurrently
              ( context.store.subscribeToAllEvents subscriber2 |> Task.mapError toText,
                context.store.subscribeToAllEvents subscriber3 |> Task.mapError toText
              )
          )

        let (sub1, (sub2, sub3)) = subscriptionIds

        -- Append multiple events
        let appendEvent event = do
              context.store.appendToStream event
                |> Task.mapError toText
        context.testEvents
          |> Array.take 3
          |> Task.mapArray appendEvent
          |> discard

        -- Wait for async processing
        AsyncTask.sleep 50 |> Task.mapError toText

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
        subscriptionId <- context.store.subscribeToAllEvents subscriber
          |> Task.mapError toText

        -- Append first event
        case context.testEvents |> Array.get 0 of
          Just event -> do
            context.store.appendToStream event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Wait for processing
        AsyncTask.sleep 50 |> Task.mapError toText

        -- Unsubscribe
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

        -- Append second event after unsubscription
        case context.testEvents |> Array.get 1 of
          Just event -> do
            context.store.appendToStream event
              |> Task.mapError toText
              |> discard
          Nothing -> do
            Task.throw "No test event"

        -- Wait for potential processing
        AsyncTask.sleep 50 |> Task.mapError toText

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
        subscriptionId <- context.store.subscribeToAllEvents subscriber
          |> Task.mapError toText

        -- Rapidly append many events
        let rapidEventCount = 50
        rapidEvents <- createRapidTestEvents context.streamId context.entityId rapidEventCount

        -- Append all events as quickly as possible
        let appendEvent event = do
              context.store.appendToStream event
                |> Task.mapError toText
        rapidEvents
          |> Task.mapArray appendEvent
          |> discard

        -- Wait for all async processing to complete
        AsyncTask.sleep 100 |> Task.mapError toText

        -- Verify all events were received
        received <- ConcurrentVar.get receivedEvents
        received
          |> Array.length
          |> shouldBe rapidEventCount

        -- Verify events are in order
        received
          |> Array.indexed
          |> Task.forEach (\(index, event) -> do
              event.localPosition
                |> shouldBe (Event.StreamPosition index))

        -- Clean up subscription
        context.store.unsubscribe subscriptionId
          |> Task.mapError toText
          |> discard

createRapidTestEvents :: Event.StreamId -> Event.EntityId -> Int -> Task _ (Array Event.InsertionEvent)
createRapidTestEvents streamId entityId count = do
  let createEvent position = do
        eventId <- Uuid.generate
        Task.yield Event.InsertionEvent
          { id = eventId,
            streamId,
            entityId,
            localPosition = Event.StreamPosition position
          }

  Array.fromLinkedList [0..(count - 1)]
    |> Task.mapArray createEvent
-}
