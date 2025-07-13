module Service.EventStore.Core (
  EventStore (..),
  Error (..),
  Limit (..),
  SubscriptionId (..),
) where

import Core
import Service.Event (EntityId, Event, InsertionEvent, StreamId, StreamPosition)


newtype Limit = Limit Int
  deriving (Eq, Show, Ord, Generic)


newtype SubscriptionId = SubscriptionId Text
  deriving (Eq, Show, Ord, Generic)


data Error
  = StreamNotFound StreamId
  | EventNotFound StreamId StreamPosition
  | ConcurrencyConflict StreamId StreamPosition
  | StorageFailure Text -- Generic storage errors with message
  | SubscriptionNotFound SubscriptionId
  | SubscriptionError SubscriptionId Text -- Subscription-specific errors
  deriving (Eq, Show)


-- | An interface for the operations of an event store
data EventStore = EventStore
  { -- | Append an event to a stream at a given expected revision.
    --   Returns the local and global stream position if successful, or an Error on conflict or failure.
    appendToStream :: InsertionEvent -> Task Error Event,
    -- | Read events from a stream in forward direction starting from a given revision.
    --   Returns an array of events or an Error.
    readStreamForwardFrom :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read events from a stream in backward direction starting from a given revision.
    --   Useful for looking at recent events.
    readStreamBackwardFrom :: EntityId -> StreamId -> StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read all events from a specific stream for a given entity.
    --   Returns an array of events or an Error.
    readAllStreamEvents :: EntityId -> StreamId -> Task Error (Array Event),
    -- | Read events from the global stream in forward direction starting from a given global position.
    --   This reads across all streams and entities.
    readAllEventsForwardFrom :: StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read events from the global stream in backward direction starting from a given global position.
    --   This reads across all streams and entities in reverse order.
    readAllEventsBackwardFrom :: StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read events from the global stream in forward direction starting from a given global position,
    --   but only for specific entities. Useful for event sourcing projections that only care about certain entities.
    readAllEventsForwardFromFiltered :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event),
    -- | Read events from the global stream in backward direction starting from a given global position,
    --   but only for specific entities. Useful for filtered historical analysis and debugging.
    readAllEventsBackwardFromFiltered :: StreamPosition -> Limit -> Array EntityId -> Task Error (Array Event),
    -- | Subscribe to all events in the event store. The subscriber function will be called
    --   for every event that gets appended. Returns a SubscriptionId for unsubscribing.
    subscribeToAllEvents :: (Event -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to all events from a specific global position onwards. This allows catching up
    --   from historical events and then receiving new events as they are appended.
    subscribeToAllEventsFromPosition :: StreamPosition -> (Event -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific entity. The subscriber function will be called
    --   only for events belonging to the specified entity.
    subscribeToEntityEvents :: EntityId -> (Event -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Subscribe to events for a specific stream within an entity. The subscriber function
    --   will be called only for events in the specified entity and stream.
    subscribeToStreamEvents :: EntityId -> StreamId -> (Event -> Task Error Unit) -> Task Error SubscriptionId,
    -- | Unsubscribe from event notifications. After this call, the subscriber function
    --   will no longer be called for new events.
    unsubscribe :: SubscriptionId -> Task Error Unit
  }
