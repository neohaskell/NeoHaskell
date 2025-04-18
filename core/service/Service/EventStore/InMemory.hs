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


-- | Gets the durable channel for a stream.
getStreamChannel :: StreamId -> StreamStore -> Task _ (Maybe (DurableChannel Event))
getStreamChannel streamId store = do
  streamsMap <- ConcurrentVar.peek store.streams
  let channel = streamsMap |> Map.get streamId
  Task.yield channel


-- | Idempotently create a stream.
ensureStream :: StreamId -> StreamStore -> Task _ (DurableChannel Event)
ensureStream streamId store = do
  maybeChannel <- getStreamChannel streamId store
  case maybeChannel of
    Just channel -> Task.yield channel
    Nothing -> do
      channel <- DurableChannel.new
      store.streams
        -- FIXME: Instead of performing the check outside,
        -- we should instead perform the Map.get inside the modify
        |> ConcurrentVar.modify (Map.set streamId channel)
        |> Lock.with store.globalLock
      Task.yield channel


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

  let lockAndAppend :: DurableChannel Event -> Task _ StreamPosition
      lockAndAppend channel = do
        channel
          |> DurableChannel.write event
          |> Lock.with store.globalLock -- FIXME: We shouldn't be using the global lock here.
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
      lockAndAppend channel

-- FIXME: Take into considerations: https://chatgpt.com/share/68027542-d4c8-800a-ae5b-44995f0ceb34
