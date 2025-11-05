module Service.EventStore.Core (
  EventStore (..),
  Error (..),
  Limit (..),
  SubscriptionId (..),
) where

import Core
import Service.Event (
  EntityName,
  Event,
  InsertionFailure,
  InsertionPayload,
  InsertionSuccess,
  StreamId,
  StreamPosition,
 )


newtype Limit = Limit Int64
  deriving (Eq, Show, Ord, Generic)


newtype SubscriptionId = SubscriptionId Text
  deriving (Eq, Show, Ord, Generic)


data Error
  = StreamNotFound EntityName StreamId
  | EventNotFound StreamId StreamPosition
  | StorageFailure Text -- Generic storage errors with message
  | SubscriptionNotFound SubscriptionId
  | SubscriptionError SubscriptionId Text -- Subscription-specific errors
  | TruncationError
  | InsertionError InsertionFailure
  deriving (Eq, Show)


-- | An interface for the operations of an event store
data EventStore eventType = EventStore
  { -- | Insert an event in a stream
    --   Returns the local and global stream position if successful, or an error on conflict or failure.
    insert :: InsertionPayload eventType -> Task Error InsertionSuccess,
    -- | Read events from a stream in forward direction starting from a given revision.
    --   Returns an array of events or an Error.
    readStreamForwardFrom :: EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Array (Event eventType)),
    -- | Read events from a stream in backward direction starting from a given revision.
    --   Useful for looking at recent events.
    readStreamBackwardFrom ::
      EntityName -> StreamId -> StreamPosition -> Limit -> Task Error (Array (Event eventType)),
    -- | Read all events from a specific stream for a given entity.
    --   Returns an array of events or an Error.
    readAllStreamEvents :: EntityName -> StreamId -> Task Error (Array (Event eventType)),
    -- | Read events from the global stream in forward direction starting from a given global position.
    --   This reads across all streams and entities.
    readAllEventsForwardFrom :: StreamPosition -> Limit -> Task Error (Array (Event eventType)),
    -- | Read events from the global stream in backward direction starting from a given global position.
    --   This reads across all streams and entities in reverse order.
    readAllEventsBackwardFrom :: StreamPosition -> Limit -> Task Error (Array (Event eventType)),
    -- | Read events from the global stream in forward direction starting from a given global position,
    --   but only for specific entities. Useful for event sourcing projections that only care about certain entities.
    readAllEventsForwardFromFiltered ::
      StreamPosition -> Limit -> Array EntityName -> Task Error (Array (Event eventType)),
    -- | Read events from the global stream in backward direction starting from a given global position,
    --   but only for specific entities. Useful for filtered historical analysis and debugging.
    readAllEventsBackwardFromFiltered ::
      StreamPosition -> Limit -> Array EntityName -> Task Error (Array (Event eventType)),
    -- | Subscribe to all events in the event store. The subscriber function will be called
    --   for every event that gets appended. Returns a SubscriptionId for unsubscribing.
    subscribeToAllEvents :: ((Event eventType) -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to all events from a specific global position onwards. This allows catching up
    --   from historical events and then receiving new events as they are appended.
    subscribeToAllEventsFromPosition ::
      StreamPosition -> ((Event eventType) -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to all events from the very beginning of the event store. This delivers ALL
    --   historical events first, then continues with new events as they are appended.
    subscribeToAllEventsFromStart :: ((Event eventType) -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific entity. The subscriber function will be called
    --   only for events belonging to the specified entity.
    subscribeToEntityEvents :: EntityName -> ((Event eventType) -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific stream within an entity. The subscriber function
    --   will be called only for events in the specified entity and stream.
    subscribeToStreamEvents ::
      EntityName -> StreamId -> ((Event eventType) -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Unsubscribe from event notifications. After this call, the subscriber function
    --   will no longer be called for new events.
    unsubscribe :: SubscriptionId -> Task Error Unit,
    -- | Removes all the events up to a position
    truncateStream :: EntityName -> StreamId -> StreamPosition -> Task Error Unit
  }
