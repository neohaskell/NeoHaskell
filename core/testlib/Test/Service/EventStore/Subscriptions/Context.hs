module Test.Service.EventStore.Subscriptions.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event (EntityName, StreamId)
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Task qualified
import Uuid qualified


data Context = Context
  { store :: EventStore,
    streamId :: StreamId,
    entityName :: EntityName,
    testEvents :: Array Event.InsertionPayload
  }


initialize :: Task Text EventStore -> Task _ Context
initialize newStore = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entityName <- Uuid.generate |> Task.map Event.EntityName

  -- Create test insertion events
  testEvents <- createTestEvents streamId entityName 5

  Task.yield Context {store, streamId, entityName, testEvents}


createTestEvents :: StreamId -> EntityName -> Int -> Task _ (Array Event.InsertionPayload)
createTestEvents streamId entityName count = do
  let createEvent position = do
        eventId <- Uuid.generate
        Task.yield
          Event.InsertionPayload
            { id = eventId,
              streamId,
              entityName,
              localPosition = Event.StreamPosition position
            }

  Array.fromLinkedList [0 .. (count - 1)]
    |> Task.mapArray createEvent
