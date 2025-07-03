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
import Service.Event qualified as Event
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
            readStreamForwardFrom = readStreamForwardFromImpl store,
            readStreamBackwardFrom = readStreamBackwardFromImpl store,
            readAllStreamEvents = readAllStreamEventsImpl store,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl store,
            readAllEventsBackwardFrom = readAllEventsBackwardFromImpl store,
            readAllEventsForwardFromFiltered = readAllEventsForwardFromFilteredImpl store,
            readAllEventsBackwardFromFiltered = readAllEventsBackwardFromFilteredImpl store
          }
  Task.yield eventStore


-- PRIVATE

data StreamStore = StreamStore
  { globalStream :: (DurableChannel Event),
    streams :: ConcurrentVar (Map (EntityId, StreamId) (DurableChannel Event)),
    globalLock :: Lock
  }


newEmptyStreamStore :: Task _ StreamStore
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream}


-- | Idempotent stream creation.
ensureStream :: EntityId -> StreamId -> StreamStore -> Task _ (DurableChannel Event)
ensureStream entityId streamId store = do
  let modifier streamMap = do
        case streamMap |> Map.get (entityId, streamId) of
          Just channel -> do
            Task.yield (streamMap, channel)
          Nothing -> do
            channel <- DurableChannel.new
            Task.yield (streamMap |> Map.set (entityId, streamId) channel, channel)
  store.streams
    |> ConcurrentVar.modifyReturning modifier
    |> Lock.with store.globalLock


appendToStreamImpl ::
  StreamStore ->
  InsertionEvent ->
  Task Error Event
appendToStreamImpl store event = do
  let id = event.id
  let localPosition = event.localPosition
  let entityId = event.entityId
  let streamId = event.streamId
  let expectedPosition = localPosition
  channel <- store |> ensureStream entityId streamId

  let appendCondition :: Array Event -> Bool
      appendCondition events = do
        let currentLength = Array.length events
        let (StreamPosition pos) = expectedPosition
        pos == currentLength

  -- First, get the global index from the global stream
  globalIndex <-
    store.globalStream
      |> DurableChannel.writeWithIndex (\index -> event |> Event.fromInsertionEvent (StreamPosition index))

  -- Now create the event with the correct global position
  let globalPosition = StreamPosition globalIndex
  let finalEvent = Event {id, streamId, entityId, localPosition, globalPosition}

  -- Write to the individual stream with the correctly positioned event
  hasWritten <-
    channel |> DurableChannel.checkAndWrite appendCondition finalEvent

  if hasWritten
    then Task.yield finalEvent
    else
      (ConcurrencyConflict streamId expectedPosition)
        |> Task.throw


readStreamForwardFromImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamForwardFromImpl store entityId streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.dropWhile (\event -> event.localPosition < position)
        |> Array.take limit


readStreamBackwardFromImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamBackwardFromImpl store entityId streamId position (Limit (limit)) = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.takeIf (\event -> event.localPosition <= position)
        |> Array.reverse
        |> Array.take limit


readAllStreamEventsImpl ::
  StreamStore ->
  EntityId ->
  StreamId ->
  Task Error (Array Event)
readAllStreamEventsImpl store entityId streamId = do
  channel <- store |> ensureStream entityId streamId
  channel
    |> DurableChannel.getAndTransform unchanged


readAllEventsForwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readAllEventsForwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents |> Array.drop position |> Array.take limit |> Task.yield


readAllEventsBackwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readAllEventsBackwardFromImpl store (StreamPosition (position)) (Limit (limit)) = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos <= position
      )
    |> Array.reverse
    |> Array.take limit
    |> Task.yield


readAllEventsForwardFromFilteredImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityId ->
  Task Error (Array Event)
readAllEventsForwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityIds = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos >= position
      )
    |> Array.takeIf (\event -> entityIds |> Array.any (\entityId -> entityId == event.entityId))
    |> Array.take limit
    |> Task.yield


readAllEventsBackwardFromFilteredImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Array EntityId ->
  Task Error (Array Event)
readAllEventsBackwardFromFilteredImpl store (StreamPosition (position)) (Limit (limit)) entityIds = do
  allGlobalEvents <- store.globalStream |> DurableChannel.getAndTransform unchanged
  allGlobalEvents
    |> Array.takeIf
      ( \event -> do
          let (StreamPosition eventPos) = event.globalPosition
          eventPos <= position
      )
    |> Array.reverse
    |> Array.takeIf (\event -> entityIds |> Array.any (\entityId -> entityId == event.entityId))
    |> Array.take limit
    |> Task.yield
