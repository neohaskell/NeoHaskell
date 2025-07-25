module Test.Service.EventStore.Subscriptions.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event (EntityId, StreamId)
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Task qualified
import Uuid qualified


data Context = Context
  { store :: EventStore,
    streamId :: StreamId,
    entityId :: EntityId,
    testEvents :: Array Event.InsertionEvent
  }


initialize :: Task Text EventStore -> Task _ Context
initialize newStore = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entityId <- Uuid.generate |> Task.map Event.EntityId

  -- Create test insertion events
  testEvents <- createTestEvents streamId entityId 5

  Task.yield Context {store, streamId, entityId, testEvents}


createTestEvents :: StreamId -> EntityId -> Int -> Task _ (Array Event.InsertionEvent)
createTestEvents streamId entityId count = do
  let createEvent position = do
        eventId <- Uuid.generate
        Task.yield
          Event.InsertionEvent
            { id = eventId,
              streamId,
              entityId,
              localPosition = Event.StreamPosition position
            }

  Array.fromLinkedList [0 .. (count - 1)]
    |> Task.mapArray createEvent
