module Service.EntityFetcher.Core (
  EntityFetcher (..),
  EntityFetchResult (..),
  FetchedEntity (..),
  Error (..),
  new,
  newWithCache,
) where

import Basics
import Console qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Result qualified
import Service.Event (EntityName, Event (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamId (StreamId)
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore.Core (EventStore, Limit (..))
import Service.EventStore.Core qualified as EventStore
import Service.SnapshotCache.Core (SnapshotCache)
import Service.SnapshotCache.Core qualified as SnapshotCache
import Service.SnapshotCache.Snapshot (Snapshot (..), SnapshotKey (..))
import Stream qualified
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)


-- | Entity state along with position information for caching
data FetchedEntity state = FetchedEntity
  { state :: state,
    lastPosition :: Maybe StreamPosition
  }
  deriving (Eq, Show)


-- | Result of fetching an entity from the event store
data EntityFetchResult state
  = EntityNotFound
  | EntityFound (FetchedEntity state)
  deriving (Eq, Show)


-- | Errors that can occur during entity operations
data Error
  = EventStoreError EventStore.Error
  | ReductionError Text
  | CacheError SnapshotCache.Error
  deriving (Eq, Show)


-- | An entity fetcher is responsible for fetching an entity's current state
--   by reading all events from the event store and applying a reduction function.
--
--   The fetcher takes an initial state and a reduce function that applies events
--   to update the state.
data EntityFetcher state event = EntityFetcher
  { -- | Fetch an entity's current state by entity name and stream ID.
    --   Reads all events from the event store and applies the reduction function
    --   to compute the current state. Returns EntityNotFound if no events exist
    --   for the stream, or EntityFound with the current state if events exist.
    fetch :: EntityName -> StreamId -> Task Error (EntityFetchResult state)
  }


-- | Create a new entity fetcher with the given event store, initial state, and reduce function.
--
--   The reduce function takes an event and the current state, and returns the new state
--   after applying the event.
--
--   Example:
--
--   @
--   fetcher <- EntityFetcher.new eventStore initialState applyEvent
--   state <- fetcher.fetch entityName streamId
--   @
new ::
  forall state event.
  EventStore event ->
  state ->
  (event -> state -> state) ->
  Task Error (EntityFetcher state event)
new eventStore initialState reduceFunction = do
  let fetchImpl entityName streamId = do
        -- Read all events for this entity stream
        streamMessages <-
          eventStore.readAllStreamEvents entityName streamId
            |> Task.mapError EventStoreError

        -- Track if we've seen any events and the last position
        let processEventsWithTracking = do
              -- Use a tuple to track (hasEvents, currentState, lastPosition)
              maybeResult <-
                streamMessages
                  |> Stream.consumeMaybe
                    ( \(seenEvents, state, _lastPos) message -> do
                        case message of
                          -- Only process actual events, ignore other message types
                          EventStore.StreamEvent event -> do
                            let eventData = event.event
                            let newState = reduceFunction eventData state
                            let newPos = event.metadata.localPosition
                            -- Mark that we've seen at least one event
                            Task.yield (True, newState, newPos)
                          -- For all other message types, keep state unchanged
                          _ -> Task.yield (seenEvents, state, _lastPos)
                    )
                    (False, initialState, Nothing)
                  |> Task.mapError (\errorText -> EventStoreError (EventStore.StorageFailure errorText))

              -- Stream.consumeMaybe returns Maybe, handle it
              case maybeResult of
                Just (hasEvents, finalState, lastPos) -> do
                  -- Return appropriate result based on whether we saw any events
                  if hasEvents
                    then Task.yield (EntityFound (FetchedEntity finalState lastPos))
                    else Task.yield EntityNotFound
                Nothing -> do
                  -- Stream ended without processing, no events found
                  Task.yield EntityNotFound

        processEventsWithTracking

  Task.yield
    EntityFetcher
      { fetch = fetchImpl
      }


-- | Create a new entity fetcher with cache support.
--
--   When a cache is provided, the fetcher will:
--   1. Check the cache for an existing snapshot
--   2. If found, read only events after the snapshot position
--   3. Apply new events on top of the cached state
--   4. Update the cache with the final state
--
--   Example:
--
--   @
--   fetcher <- EntityFetcher.newWithCache eventStore cache initialState applyEvent
--   state <- fetcher.fetch entityName streamId
--   @
newWithCache ::
  forall state event.
  EventStore event ->
  SnapshotCache state ->
  state ->
  (event -> state -> state) ->
  Task Error (EntityFetcher state event)
newWithCache eventStore snapshotCache initialState reduceFunction = do
  let fetchImpl entityName streamId = do
        -- Step 1: Check cache for existing snapshot
        maybeSnapshot <-
          snapshotCache.get entityName streamId
            |> Task.asResult
            |> Task.map Result.toMaybe
            |> Task.map (\maybeResult -> case maybeResult of Just x -> x; Nothing -> Nothing)

        -- Step 2: Determine starting state and position
        let (startState, startPosition, hadCachedSnapshot) = case maybeSnapshot of
              Just snapshot -> (snapshot.state, snapshot.position, True)
              Nothing -> (initialState, StreamPosition 0, False)

        -- Step 3: Read events from startPosition onwards
        -- If we had a cached snapshot, we need to read from position + 1
        -- If no cache, we read all events (position 0)
        let nextPosition = case maybeSnapshot of
              Just _ ->
                let (StreamPosition pos) = startPosition
                 in StreamPosition (pos + 1)
              Nothing -> StreamPosition 0

        streamMessages <-
          eventStore.readStreamForwardFrom entityName streamId nextPosition (Limit maxValue)
            |> Task.mapError EventStoreError

        -- Step 4: Apply events and track final position
        maybeResult <-
          streamMessages
            |> Stream.consumeMaybe
              ( \(seenNewEvents, state, lastPos) message -> do
                  case message of
                    EventStore.StreamEvent event -> do
                      let eventData = event.event
                      let newState = reduceFunction eventData state
                      let newPos = event.metadata.localPosition
                      Task.yield (True, newState, newPos)
                    _ -> Task.yield (seenNewEvents, state, lastPos)
              )
              (False, startState, Just startPosition)
            |> Task.mapError (\errorText -> EventStoreError (EventStore.StorageFailure errorText))

        case maybeResult of
          Just (seenNewEvents, finalState, lastPos) -> do
            -- Determine final position - use lastPos from new events or startPosition if no new events
            let finalPosition = case lastPos of
                  Just pos -> Just pos
                  Nothing ->
                    if hadCachedSnapshot
                      then Just startPosition
                      else Nothing

            -- Step 5: Update cache (fire-and-forget, don't fail if cache update fails)
            case finalPosition of
              Just pos -> do
                let snapshot =
                      Snapshot
                        { key = SnapshotKey entityName streamId,
                          state = finalState,
                          position = pos
                        }
                cacheResult <-
                  snapshotCache.set snapshot
                    |> Task.asResult
                case cacheResult of
                  Err cacheError ->
                    Console.print [fmt|[EntityFetcher] Warning: Failed to update snapshot cache for #{entityName}/#{streamId}: #{toText cacheError}|]
                      |> Task.ignoreError
                  Ok _ -> pass
              Nothing -> pass

            -- Step 6: Return result
            -- If we had a cached snapshot, entity exists even if no new events
            -- If no cache and no events, entity doesn't exist
            if seenNewEvents || hadCachedSnapshot
              then Task.yield (EntityFound (FetchedEntity finalState finalPosition))
              else Task.yield EntityNotFound
          Nothing -> do
            -- Stream ended without processing
            if hadCachedSnapshot
              then Task.yield (EntityFound (FetchedEntity startState (Just startPosition)))
              else Task.yield EntityNotFound

  Task.yield
    EntityFetcher
      { fetch = fetchImpl
      }
