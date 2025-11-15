module Test.Service.EventStore.Subscriptions.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event (EntityName, StreamId)
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Task qualified
import Test.Service.EventStore.Core (MyEvent, newInsertion)
import Uuid qualified


data Context = Context
  { store :: EventStore MyEvent,
    streamId :: StreamId,
    entityName :: EntityName,
    testEvents :: Array (Event.InsertionPayload MyEvent)
  }


initialize :: Task Text (EventStore MyEvent) -> Task _ Context
initialize newStore = do
  store <- newStore
  streamId <- StreamId.new
  entityNameText <- Uuid.generate |> Task.map toText
  let entityName = Event.EntityName entityNameText

  -- Create test insertion events
  testEvents <- createTestEvents streamId entityName 5

  Task.yield Context {store, streamId, entityName, testEvents}


createTestEvents :: StreamId -> EntityName -> Int -> Task _ (Array (Event.InsertionPayload MyEvent))
createTestEvents streamId entityName count = do
  let createEvent index = do
        insertions <- Array.fromLinkedList [index] |> Task.mapArray newInsertion
        Task.yield
          Event.InsertionPayload
            { streamId,
              entityName,
              insertionType = Event.AnyStreamState,
              insertions
            }

  Array.fromLinkedList [0 .. (count - 1)]
    |> Task.mapArray createEvent
