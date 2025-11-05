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
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entity1Id :: Event.EntityName,
    entity2Id :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore,
    generatedEvents :: Array Event.InsertionPayload,
    maxGlobalPosition :: Event.StreamPosition
  }


initialize :: Task Text EventStore -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- Uuid.generate |> Task.map Event.StreamId
  entity1Id <- Uuid.generate |> Task.map Event.EntityName
  entity2Id <- Uuid.generate |> Task.map Event.EntityName

  let generateEvents :: Event.EntityName -> Task Text (Array Event.InsertionPayload)
      generateEvents entityName = do
        Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray \index -> do
          let localPosition = Event.StreamPosition index
          id <- Uuid.generate
          Event.InsertionPayload
            { id,
              streamId,
              entityName,
              localPosition
            }
            |> Task.yield

  entity1Events <- generateEvents entity1Id
  entity2Events <- generateEvents entity2Id

  entity1Inserted <-
    entity1Events
      |> Task.mapArray (\event -> event |> store.appendToStream)
      |> Task.mapError toText

  entity2Inserted <-
    entity2Events
      |> Task.mapArray (\event -> event |> store.appendToStream)
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
