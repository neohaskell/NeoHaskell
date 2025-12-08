module Test.Service.EventStore.IndividualStreamOrdering.Context (
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
import Test.Service.EventStore.Core (CartEvent, newInsertion)
import Uuid qualified


data Context = Context
  { eventCount :: Int64,
    entityName :: Event.EntityName,
    streamId :: Event.StreamId,
    store :: EventStore CartEvent,
    allInsertions :: Array (Event.Insertion CartEvent),
    positions :: Array Event.StreamPosition
  }


initialize :: Task Text (EventStore CartEvent) -> Int -> Task Text Context
initialize newStore eventCountNumber = do
  store <- newStore
  streamId <- StreamId.new
  entityName <- Uuid.generate |> Task.map (toText .> Event.EntityName)
  allInsertions <-
    [0 .. eventCountNumber - 1]
      |> Task.mapArray
        newInsertion

  -- Insert events in chunks of 100 (batch size limit)
  allInsertions |> Array.chunksOf 100 |> Task.forEach \chunk -> do
    let payload = Event.InsertionPayload {streamId, entityName, insertionType = Event.AnyStreamState, insertions = chunk}
    payload |> store.insert |> Task.mapError toText |> discard

  let positions = [0 .. eventCountNumber - 1] |> Array.map (fromIntegral .> Event.StreamPosition)
  let eventCount = fromIntegral eventCountNumber

  return Context {eventCount, streamId, store, allInsertions, positions, entityName}
