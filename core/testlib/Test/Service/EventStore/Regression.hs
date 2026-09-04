module Test.Service.EventStore.Regression (
  seedStream,
  recordInsertions,
  recordFetchedRevisions,
  requireInsertionType,
  failFirstWithConsistencyConflict,
  InsertBarrier,
  newInsertBarrier,
  barrierBeforeInsert,
  awaitInsertions,
  releaseInsertions,
) where

import Array qualified
import Channel qualified
import ConcurrentVar qualified
import Core
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher (..), FetchedEntity (..))
import Service.Event qualified as Event
import Service.EventStore.Core (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Task qualified


seedStream ::
  EventStore event ->
  Event.EntityName ->
  Event.StreamId ->
  Array event ->
  Task Text Unit
seedStream store entityName streamId events = do
  payload <- Event.payloadFromEvents entityName streamId events
  let creation = payload {Event.insertionType = Event.StreamCreation}
  creation
    |> store.insert
    |> Task.mapError toText
    |> discard


recordInsertions ::
  EventStore event ->
  Task error (EventStore event, Task error (Array (Event.InsertionPayload event)))
recordInsertions store = do
  recorded <- ConcurrentVar.containing Array.empty
  let insert payload = do
        ConcurrentVar.modify (Array.push payload) recorded
        store.insert payload
  let observingStore = store {EventStore.insert = insert}
  Task.yield (observingStore, ConcurrentVar.peek recorded)


recordFetchedRevisions ::
  EntityFetcher state event ->
  Task error (EntityFetcher state event, Task error (Array (Maybe Event.StreamPosition)))
recordFetchedRevisions fetcher = do
  recorded <- ConcurrentVar.containing Array.empty
  let fetch entityName streamId = do
        result <- fetcher.fetch entityName streamId
        let revision = case result of
              EntityNotFound -> Nothing
              EntityFound entity -> entity.lastPosition
        ConcurrentVar.modify (Array.push revision) recorded
        Task.yield result
  let observingFetcher = fetcher {fetch = fetch}
  Task.yield (observingFetcher, ConcurrentVar.peek recorded)


requireInsertionType :: Event.InsertionType -> EventStore event -> EventStore event
requireInsertionType expected store = do
  let insert payload = do
        let actual = payload.insertionType
        if actual == expected
          then store.insert payload
          else
            Task.throw
              ( EventStore.InsertionError
                  (Event.InsertionFailed [fmt|Expected insertion type #{expected}, got #{actual}|])
              )
  store {EventStore.insert = insert}


failFirstWithConsistencyConflict ::
  EventStore event ->
  Task error (EventStore event)
failFirstWithConsistencyConflict store = do
  failNext <- ConcurrentVar.containing True
  let insert payload = do
        shouldFail <- ConcurrentVar.swap False failNext
        if shouldFail
          then Task.throw (EventStore.InsertionError Event.ConsistencyCheckFailed)
          else store.insert payload
  Task.yield store {EventStore.insert = insert}


data InsertBarrier = InsertBarrier
  { arrivals :: Channel.Channel Unit,
    releases :: Channel.Channel Unit
  }


newInsertBarrier :: Task error InsertBarrier
newInsertBarrier = do
  arrivals <- Channel.new
  releases <- Channel.new
  Task.yield InsertBarrier {arrivals, releases}


barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event
barrierBeforeInsert barrier store = do
  let insert payload = do
        Channel.write unit barrier.arrivals
        Channel.read barrier.releases
        store.insert payload
  store {EventStore.insert = insert}


awaitInsertions :: Int -> InsertBarrier -> Task error Unit
awaitInsertions count barrier = do
  if count <= 0
    then Task.yield unit
    else do
      Channel.read barrier.arrivals
      awaitInsertions (count - 1) barrier


releaseInsertions :: Int -> InsertBarrier -> Task error Unit
releaseInsertions count barrier = do
  if count <= 0
    then Task.yield unit
    else do
      Channel.write unit barrier.releases
      releaseInsertions (count - 1) barrier
