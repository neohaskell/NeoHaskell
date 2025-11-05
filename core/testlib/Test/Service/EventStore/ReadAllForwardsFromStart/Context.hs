module Test.Service.EventStore.ReadAllForwardsFromStart.Context (
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
    entity1Id :: Event.EntityName,
    entity2Id :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore,
    generatedEvents :: Array Event.InsertionPayload,
    positions :: Array Event.StreamPosition
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
          let localPosition = Event.StreamPosition (index)
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

  let entity1Positions =
        entity1Inserted
          |> Array.map (\result -> result.localPosition)

  entity2Inserted <-
    entity2Events
      |> Task.mapArray (\event -> event |> store.appendToStream)
      |> Task.mapError toText

  let entity2Positions =
        entity2Inserted
          |> Array.map (\result -> result.localPosition)

  let allEvents = entity1Events |> Array.append entity2Events
  let allPositions = entity1Positions |> Array.append entity2Positions

  return
    Context
      { eventCount,
        streamId,
        store,
        generatedEvents = allEvents,
        positions = allPositions,
        entity1Id,
        entity2Id
      }
