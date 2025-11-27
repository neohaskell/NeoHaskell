module Test.Service.EventStore.ReadAllBackwardsFromEnd.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Maybe qualified
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test.Service.EventStore.Core (CartEvent, newInsertion)
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entity1Id :: Event.EntityName,
    entity2Id :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore CartEvent,
    generatedEvents :: Array (Event.InsertionPayload CartEvent),
    maxGlobalPosition :: Event.StreamPosition
  }


initialize :: Task Text (EventStore CartEvent) -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- StreamId.new
  entity1IdText <- Uuid.generate |> Task.map toText
  let entity1Id = Event.EntityName entity1IdText
  entity2IdText <- Uuid.generate |> Task.map toText
  let entity2Id = Event.EntityName entity2IdText

  -- Generate all insertions for both entities
  entity1Insertions <- [0 .. eventCount - 1] |> Task.mapArray newInsertion
  entity2Insertions <- [0 .. eventCount - 1] |> Task.mapArray newInsertion

  -- Insert entity1 events in chunks of 100 (batch size limit) and track results
  entity1Results <-
    entity1Insertions
      |> Array.chunksOf 100
      |> Task.mapArray
        ( \chunk -> do
            let payload = Event.InsertionPayload {streamId, entityName = entity1Id, insertionType = Event.AnyStreamState, insertions = chunk}
            payload |> store.insert |> Task.mapError toText
        )

  -- Insert entity2 events in chunks of 100 (batch size limit) and track results
  entity2Results <-
    entity2Insertions
      |> Array.chunksOf 100
      |> Task.mapArray
        ( \chunk -> do
            let payload = Event.InsertionPayload {streamId, entityName = entity2Id, insertionType = Event.AnyStreamState, insertions = chunk}
            payload |> store.insert |> Task.mapError toText
        )

  let maxGlobalPosition =
        entity1Results
          |> Array.append entity2Results
          |> Array.map (\result -> result.globalPosition)
          |> Array.maximum
          |> Maybe.withDefault (Event.StreamPosition 0)

  -- Create payload wrappers for compatibility (not used in tests, but kept for API consistency)
  let entity1Events =
        entity1Insertions
          |> Array.map
            ( \insertion ->
                Event.InsertionPayload
                  { streamId,
                    entityName = entity1Id,
                    insertionType = Event.AnyStreamState,
                    insertions = Array.wrap insertion
                  }
            )

  let entity2Events =
        entity2Insertions
          |> Array.map
            ( \insertion ->
                Event.InsertionPayload
                  { streamId,
                    entityName = entity2Id,
                    insertionType = Event.AnyStreamState,
                    insertions = Array.wrap insertion
                  }
            )

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
