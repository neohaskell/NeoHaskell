module Service.EntityFetcher.Core (
  EntityFetcher (..),
  EntityFetchResult (..),
  Error (..),
  new,
) where

import Core
import Service.Event (EntityName, Event (..))
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified


-- | Result of fetching an entity from the event store
data EntityFetchResult state
  = EntityNotFound
  | EntityFound state
  deriving (Eq, Show)


-- | Errors that can occur during entity operations
data Error
  = EventStoreError EventStore.Error
  | ReductionError Text
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

        -- Track if we've seen any events
        let processEventsWithTracking = do
              -- Use a tuple to track (hasEvents, currentState)
              maybeResult <-
                streamMessages
                  |> Stream.consumeMaybe
                    ( \(seenEvents, state) message -> do
                        case message of
                          -- Only process actual events, ignore other message types
                          EventStore.StreamEvent event -> do
                            let eventData = event.event
                            let newState = reduceFunction eventData state
                            -- Mark that we've seen at least one event
                            Task.yield (True, newState)
                          -- For all other message types, keep state unchanged
                          _ -> Task.yield (seenEvents, state)
                    )
                    (False, initialState)
                  |> Task.mapError (\errorText -> EventStoreError (EventStore.StorageFailure errorText))

              -- Stream.consumeMaybe returns Maybe, handle it
              case maybeResult of
                Just (hasEvents, finalState) -> do
                  -- Return appropriate result based on whether we saw any events
                  if hasEvents
                    then Task.yield (EntityFound finalState)
                    else Task.yield EntityNotFound
                Nothing -> do
                  -- Stream ended without processing, no events found
                  Task.yield EntityNotFound

        processEventsWithTracking

  Task.yield
    EntityFetcher
      { fetch = fetchImpl
      }
