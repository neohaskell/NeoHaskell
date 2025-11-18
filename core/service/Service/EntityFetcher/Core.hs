module Service.EntityFetcher.Core (
  EntityFetcher (..),
  Error (..),
  new,
) where

import Core
import Service.Event (EntityName, Event (..), StreamId)
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified


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
    --   to compute the current state.
    fetch :: EntityName -> StreamId -> Task Error state
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

        -- Consume the stream, applying the reduction function to each event
        -- This processes events one-by-one without loading everything into memory
        finalState <-
          streamMessages
            |> Stream.consume
              ( \state message -> do
                  case message of
                    -- Only process actual events, ignore other message types
                    EventStore.StreamEvent event -> do
                      let eventData = event.event
                      let newState = reduceFunction eventData state
                      Task.yield newState
                    -- For all other message types, keep state unchanged
                    _ -> Task.yield state
              )
              initialState
            |> Task.mapError (\errorText -> EventStoreError (EventStore.StorageFailure errorText))

        Task.yield finalState

  Task.yield
    EntityFetcher
      { fetch = fetchImpl
      }
