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
            readStreamForwardFrom = readStreamForwardFromImpl store,
            readStreamBackwardFrom = readStreamBackwardFromImpl store,
            readAllStreamEvents = readAllStreamEventsImpl store,
            readAllEventsForwardFrom = readAllEventsForwardFromImpl store
          }
  Task.yield eventStore


-- PRIVATE

data StreamStore = StreamStore
  { globalStream :: (DurableChannel Event),
    streams :: ConcurrentVar (Map StreamId (DurableChannel Event)),
    globalLock :: Lock
  }


newEmptyStreamStore :: Task _ StreamStore
newEmptyStreamStore = do
  globalLock <- Lock.new
  globalStream <- DurableChannel.new
  streams <- ConcurrentVar.containing Map.empty
  Task.yield StreamStore {globalLock, streams, globalStream}


-- | Idempotent stream creation.
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
  Task Error AppendResult
appendToStreamImpl store streamId expectedPosition event = do
  channel <- store |> ensureStream streamId

  let appendCondition :: Array Event -> Bool
      appendCondition events = do
        let currentLength = Array.length events
        let (StreamPosition (Positive pos)) = expectedPosition
        pos == currentLength

  let globalPositionModifier :: Int -> Event
      globalPositionModifier index = do
        let globalPos = Just (StreamPosition (Positive (index + 1)))
        event {Service.Event.globalPosition = globalPos}

  hasWritten <- channel |> DurableChannel.checkAndWrite appendCondition event
  if hasWritten
    then do
      globalIndex <-
        store.globalStream
          |> DurableChannel.writeWithIndex globalPositionModifier
      let localPosition = expectedPosition
      let globalPosition = StreamPosition (Positive globalIndex)
      Task.yield AppendResult {localPosition, globalPosition}
    else
      (ConcurrencyConflict streamId expectedPosition)
        |> Task.throw


readStreamForwardFromImpl ::
  StreamStore ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamForwardFromImpl store streamId position (Limit (Positive limit)) = do
  channel <- store |> ensureStream streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.dropWhile (\event -> event.position < position)
        |> Array.take limit


readStreamBackwardFromImpl ::
  StreamStore ->
  StreamId ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readStreamBackwardFromImpl store streamId position (Limit (Positive limit)) = do
  channel <- store |> ensureStream streamId
  channel
    |> DurableChannel.getAndTransform \events ->
      events
        |> Array.dropIf (\event -> event.position > position)
        |> Array.reduce (\event acc -> if Array.length acc < limit then Array.push event acc else acc) Array.empty


readAllStreamEventsImpl ::
  StreamStore ->
  StreamId ->
  Task Error (Array Event)
readAllStreamEventsImpl store streamId = do
  channel <- store |> ensureStream streamId
  channel
    |> DurableChannel.getAndTransform unchanged


readAllEventsForwardFromImpl ::
  StreamStore ->
  StreamPosition ->
  Limit ->
  Task Error (Array Event)
readAllEventsForwardFromImpl _ _ _ =
  Task.yield Array.empty
