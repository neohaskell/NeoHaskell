module Test.Service.EventStore.IndividualStreamOrdering.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entityId :: Event.EntityId,
    streamId :: Event.StreamId,
    store :: EventStore,
    generatedEvents :: Array Event.InsertionEvent,
    positions :: Array Event.StreamPosition
  }


initialize :: Task Text EventStore -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entityId <- Uuid.generate |> Task.map Event.EntityId
  generatedEvents <-
    Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray \index -> do
      let localPosition = Event.StreamPosition (index)
      id <- Uuid.generate
      Event.InsertionEvent {id, streamId, entityId, localPosition}
        |> Task.yield

  -- Append all events sequentially
  let appendEvents :: Array Event.InsertionEvent -> Task EventStore.Error (Array Event.StreamPosition)
      appendEvents events = do
        events |> Task.mapArray \event -> do
          result <- event |> store.appendToStream
          Task.yield result.localPosition

  positions <- generatedEvents |> appendEvents |> Task.mapError toText

  return Context {eventCount, streamId, store, generatedEvents, positions, entityId}
