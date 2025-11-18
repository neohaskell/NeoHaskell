module Service.Entity.Core (
  EntityReducer (..),
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


data EntityReducer state event = EntityReducer
  { fetch :: EntityName -> StreamId -> Task Error state
  }


new ::
  forall state event.
  EventStore event ->
  state ->
  (state -> event -> state) ->
  Task Error (EntityReducer state event)
new eventStore initialState reduceFunction = do
  let fetchImpl entityName streamId = do
        streamMessages <-
          eventStore.readAllStreamEvents entityName streamId
            |> Task.mapError EventStoreError

        finalState <-
          streamMessages
            |> Stream.consume
              ( \state message -> do
                  case message of
                    EventStore.StreamEvent event -> do
                      let eventData = event.event
                      let newState = reduceFunction state eventData
                      Task.yield newState
                    _ -> Task.yield state
              )
              initialState
            |> Task.mapError (EventStoreError <. EventStore.StorageFailure)

        Task.yield finalState

  Task.yield
    EntityReducer
      { fetch = fetchImpl
      }
