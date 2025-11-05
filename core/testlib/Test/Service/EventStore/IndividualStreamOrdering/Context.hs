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
import Test.Service.EventStore.Core (MyEvent, newInsertion)
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entityName :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore MyEvent,
    payload :: Event.InsertionPayload MyEvent,
    position :: Event.StreamPosition
  }


initialize :: Task Text (EventStore MyEvent) -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entityName <- Uuid.generate |> Task.map (toText .> Event.EntityName)
  insertions <-
    Array.fromLinkedList [0 .. eventCount - 1]
      |> Task.mapArray
        newInsertion
  let payload = Event.InsertionPayload {streamId, entityName, insertionType = Event.AnyStreamState, insertions}

  insertResult <- payload |> store.insert |> Task.mapError toText
  let position = insertResult.localPosition

  return Context {eventCount, streamId, store, payload, position, entityName}
