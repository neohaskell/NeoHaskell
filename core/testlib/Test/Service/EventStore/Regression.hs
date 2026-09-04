-- | Deterministic fixtures for CommandExecutor and event-store regression specs.
--
-- >>> Event.AnyStreamState == Event.AnyStreamState
-- True
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
  intentionalRed,
) where

import Array qualified
import Channel qualified
import ConcurrentVar qualified
import Core
import Environment qualified
import Result qualified
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher (..), FetchedEntity (..))
import Service.Event qualified as Event
import Service.EventStore.Core (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Task qualified


-- | Create a stream from domain events using 'Event.StreamCreation'.
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


-- | Wrap a store and return an observer for every attempted insertion payload.
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


-- | Wrap a fetcher and return an observer for fetched local revisions.
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


-- | Reject insertions at the store boundary unless their type matches exactly.
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


-- | Fail exactly the first append with 'Event.ConsistencyCheckFailed'.
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


-- | A rendezvous that holds insertion attempts until a test releases them.
data InsertBarrier = InsertBarrier
  { arrivals :: Channel.Channel Unit,
    releases :: Channel.Channel Unit
  }


-- | Create an empty insertion rendezvous.
newInsertBarrier :: Task error InsertBarrier
newInsertBarrier = do
  arrivals <- Channel.new
  releases <- Channel.new
  Task.yield InsertBarrier {arrivals, releases}


-- | Wrap a store so each insertion waits at the supplied rendezvous.
barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event
barrierBeforeInsert barrier store = do
  let insert payload = do
        Channel.write unit barrier.arrivals
        Channel.read barrier.releases
        store.insert payload
  store {EventStore.insert = insert}


-- | Block until the requested number of insertion attempts have arrived.
awaitInsertions :: Int -> InsertBarrier -> Task error Unit
awaitInsertions count barrier = do
  if count <= 0
    then Task.yield unit
    else do
      Channel.read barrier.arrivals
      awaitInsertions (count - 1) barrier


-- | Release the requested number of waiting insertion attempts.
releaseInsertions :: Int -> InsertBarrier -> Task error Unit
releaseInsertions count barrier = do
  if count <= 0
    then Task.yield unit
    else do
      Channel.write unit barrier.releases
      releaseInsertions (count - 1) barrier


-- | Emit the exact red sentinel selected by the mechanical recipe smoke.
intentionalRed :: Text -> Task Text Unit
intentionalRed recipe = do
  selectedResult <-
    Environment.getVariable "NEOHASKELL_REGRESSION_SMOKE"
      |> Task.asResult
  case selectedResult of
    Result.Err _ -> Task.yield unit
    Result.Ok selected ->
      if selected == recipe
        then Task.throw [fmt|INTENTIONAL_RED:#{recipe}|]
        else Task.yield unit
