module Service.EventStore.InMemory (
  new,
) where

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

  let getLastPosition :: DurableChannel Event -> Task _ StreamPosition
      getLastPosition channel = do
        lastEvent <- channel |> DurableChannel.last
        case lastEvent of
          Nothing -> Task.yield (StreamPosition 0)
          Just event -> Task.yield event.position

  let appendEvent :: DurableChannel Event -> Task _ StreamPosition
      appendEvent channel = do
        channel |> DurableChannel.write event
        let (StreamPosition pos) = expectedPosition
        Task.yield (pos + 1 |> StreamPosition)

  -- FIXME: This should be check-then-write that is atomic at the channel level.
  expected <- getLastPosition channel
  if expected != expectedPosition
    then
      do
        (ConcurrencyConflict streamId expectedPosition expected)
        |> Task.throw
    else do
      appendEvent channel

-- FIXME: Take into considerations: https://chatgpt.com/share/68027542-d4c8-800a-ae5b-44995f0ceb34
