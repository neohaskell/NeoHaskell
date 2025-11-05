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
    entityName :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore,
    generatedEvents :: Array Event.InsertionPayload,
    positions :: Array Event.StreamPosition
  }


initialize :: Task Text EventStore -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entityName <- Uuid.generate |> Task.map Event.EntityName
  generatedEvents <-
    Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray \index -> do
      let localPosition = Event.StreamPosition (index)
      id <- Uuid.generate
      Event.InsertionPayload {id, streamId, entityName, localPosition}
        |> Task.yield

  -- Append all events sequentially
  let appendEvents :: Array Event.InsertionPayload -> Task EventStore.Error (Array Event.StreamPosition)
      appendEvents events = do
        events |> Task.mapArray \event -> do
          result <- event |> store.appendToStream
          Task.yield result.localPosition

  positions <- generatedEvents |> appendEvents |> Task.mapError toText

  return Context {eventCount, streamId, store, generatedEvents, positions, entityName}
