module Test.Service.EventStore.Subscriptions.SimpleSpec (spec) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent, newInsertion)
import Uuid qualified


spec :: Task Text (EventStore MyEvent) -> Spec Unit
spec newStore = do
  describe "Event Store Subscriptions" do
    it "can subscribe and receive events" \_ -> do
      store <- newStore

      -- Create test data
      streamId <- Uuid.generate |> Task.map Event.StreamId
      entityNameText <- Uuid.generate |> Task.map toText
      let entityName = Event.EntityName entityNameText
      insertion <- newInsertion 0

      let testEvent =
            Event.InsertionPayload
              { streamId,
                entityName,
                insertionType = Event.AnyStreamState,
                insertions = Array.fromLinkedList [insertion]
              }

      -- Create a shared variable to collect received events
      receivedEvents <- ConcurrentVar.containing (Array.empty :: Array Event)

      -- Define subscriber function that collects events
      let subscriber event = do
            receivedEvents |> ConcurrentVar.modify (Array.push event)
            Task.yield () :: Task EventStore.Error Unit

      -- Subscribe to all events
      subscriptionId <-
        store.subscribeToAllEvents subscriber
          |> Task.mapError toText

      -- Insert test event
      store.insert testEvent
        |> Task.mapError toText
        |> discard

      -- Wait briefly for async notifications to complete
      AsyncTask.sleep 10 |> Task.mapError (\_ -> "timeout")

      -- Check that we received the event
      received <- ConcurrentVar.get receivedEvents
      received
        |> Array.length
        |> shouldBe 1

      -- Clean up subscription
      store.unsubscribe subscriptionId
        |> Task.mapError toText
        |> discard
