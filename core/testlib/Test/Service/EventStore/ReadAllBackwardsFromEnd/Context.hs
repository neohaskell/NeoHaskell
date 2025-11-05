module Test.Service.EventStore.ReadAllBackwardsFromEnd.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Maybe qualified
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test.Service.EventStore.Core (MyEvent, newInsertion)
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entity1Id :: Event.EntityName,
    entity2Id :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore MyEvent,
    generatedEvents :: Array (Event.InsertionPayload MyEvent),
    maxGlobalPosition :: Event.StreamPosition
  }


initialize :: Task Text (EventStore MyEvent) -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entity1IdText <- Uuid.generate |> Task.map toText
  let entity1Id = Event.EntityName entity1IdText
  entity2IdText <- Uuid.generate |> Task.map toText
  let entity2Id = Event.EntityName entity2IdText

  let generateEvents :: Event.EntityName -> Task Text (Array (Event.InsertionPayload MyEvent))
      generateEvents entityName = do
        Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray \index -> do
          insertions <- Array.fromLinkedList [index] |> Task.mapArray newInsertion
          Event.InsertionPayload
            { streamId,
              entityName,
              insertionType = Event.AnyStreamState,
              insertions
            }
            |> Task.yield

  entity1Events <- generateEvents entity1Id
  entity2Events <- generateEvents entity2Id

  entity1Inserted <-
    entity1Events
      |> Task.mapArray (\payload -> payload |> store.insert)
      |> Task.mapError toText

  entity2Inserted <-
    entity2Events
      |> Task.mapArray (\payload -> payload |> store.insert)
      |> Task.mapError toText

  let maxGlobalPosition =
        entity1Inserted
          |> Array.append entity2Inserted
          |> Array.map (\event -> event.globalPosition)
          |> Array.maximum
          |> Maybe.withDefault (Event.StreamPosition 0)

  return
    Context
      { eventCount,
        streamId,
        store,
        generatedEvents = entity1Events |> Array.append entity2Events,
        maxGlobalPosition,
        entity1Id,
        entity2Id
      }
