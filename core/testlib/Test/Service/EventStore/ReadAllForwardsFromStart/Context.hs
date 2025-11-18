module Test.Service.EventStore.ReadAllForwardsFromStart.Context (
  Context (..),
  initialize,
) where

import Array qualified
import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test.Service.EventStore.Core (BankAccountEvent, newInsertion)
import Uuid qualified


data Context = Context
  { eventCount :: Int,
    entity1Id :: Event.EntityName,
    entity2Id :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore BankAccountEvent,
    generatedEvents :: Array (Event.InsertionPayload BankAccountEvent),
    positions :: Array Event.StreamPosition
  }


initialize :: Task Text (EventStore BankAccountEvent) -> Int -> Task Text Context
initialize newStore eventCount = do
  store <- newStore
  streamId <- StreamId.new
  entity1IdText <- Uuid.generate |> Task.map toText
  let entity1Id = Event.EntityName entity1IdText
  entity2IdText <- Uuid.generate |> Task.map toText
  let entity2Id = Event.EntityName entity2IdText

  -- Generate all insertions for both entities
  entity1Insertions <- Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray newInsertion
  entity2Insertions <- Array.fromLinkedList [0 .. eventCount - 1] |> Task.mapArray newInsertion

  -- Insert entity1 events in chunks of 100 (batch size limit)
  entity1Insertions |> Array.chunksOf 100 |> Task.forEach \chunk -> do
    let payload = Event.InsertionPayload {streamId, entityName = entity1Id, insertionType = Event.AnyStreamState, insertions = chunk}
    payload |> store.insert |> Task.mapError toText |> discard

  -- Insert entity2 events in chunks of 100 (batch size limit)
  entity2Insertions |> Array.chunksOf 100 |> Task.forEach \chunk -> do
    let payload = Event.InsertionPayload {streamId, entityName = entity2Id, insertionType = Event.AnyStreamState, insertions = chunk}
    payload |> store.insert |> Task.mapError toText |> discard

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

  let entity1Positions =
        Array.fromLinkedList [0 .. eventCount - 1]
          |> Array.map (fromIntegral .> Event.StreamPosition)

  let entity2Positions =
        Array.fromLinkedList [0 .. eventCount - 1]
          |> Array.map (fromIntegral .> Event.StreamPosition)

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
