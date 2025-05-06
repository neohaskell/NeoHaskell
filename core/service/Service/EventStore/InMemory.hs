module Service.EventStore.InMemory (
  new,
) where

import Array qualified
import ConcurrentVar qualified
import Core
import DurableChannel qualified
import Lock qualified
import Map qualified
import Service.Event
import Service.EventStore.Core
import Task qualified


-- | For the in-memory event store, we will use a ConcurrentVar that stores
-- a map of stream IDs to a MutableArray of events.
-- This is the simplest way to have an EventStore that can be used in a test environment.
new :: Task Error EventStore
new = do
  store <- newEmptyStreamStore
  let eventStore =
        EventStore
          { appendToStream = appendToStreamImpl store,
            readStreamForwardFrom = panic "not implemented yet",
            readStreamBackwardFrom = panic "not implemented yet",
            readAllStreamEvents = panic "not implemented yet",
            readAllEventsForwardFrom = panic "not implemented yet"
          }
  Task.yield eventStore


-- PRIVATE

data StreamStore = StreamStore
  { streams :: ConcurrentVar (Map StreamId (DurableChannel Event)),
    globalLock :: Lock
  }


newEmptyStreamStore :: Task _ StreamStore
newEmptyStreamStore = do
  globalLock <- Lock.new
  streams <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams}


-- | Idempotently create a stream.
ensureStream :: StreamId -> StreamStore -> Task _ (DurableChannel Event)
ensureStream streamId store = do
  let modifier streamMap = do
        case streamMap |> Map.get streamId of
          Just channel -> do
            Task.yield (streamMap, channel)
          Nothing -> do
            channel <- DurableChannel.new
            Task.yield (streamMap |> Map.set streamId channel, channel)
  store.streams
    |> ConcurrentVar.modifyReturning modifier
    |> Lock.with store.globalLock


appendToStreamImpl ::
  StreamStore ->
  StreamId ->
  StreamPosition ->
  Event ->
  Task Error StreamPosition
appendToStreamImpl store streamId expectedPosition event = do
  channel <- store |> ensureStream streamId

  let appendCondition :: Array Event -> Bool
      appendCondition events =
        case Array.last events of
          Nothing -> True
          Just lastEvent ->
            lastEvent.position == expectedPosition

  hasWritten <- channel |> DurableChannel.checkAndWrite appendCondition event
  if hasWritten
    then do
      let (StreamPosition pos) = expectedPosition
      Task.yield (pos + 1 |> StreamPosition)
    else
      (ConcurrencyConflict streamId expectedPosition)
        |> Task.throw
