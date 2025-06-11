module Service.EventStore.Core (
  EventStore (..),
  Error (..),
  Limit (..),
  AppendResult (..),
) where

import Core
import Service.Event (Event, StreamId, StreamPosition)


newtype Limit = Limit (Natural Int)
  deriving (Eq, Show, Ord, Generic)


data Error
  = StreamNotFound StreamId
  | EventNotFound StreamId StreamPosition
  | ConcurrencyConflict StreamId StreamPosition
  | StorageFailure Text -- Generic storage errors with message
  deriving (Eq, Show)


data AppendResult = AppendResult
  { localPosition :: StreamPosition,
    globalPosition :: StreamPosition
  }
  deriving (Eq, Show)


-- | An interface for the operations of an event store
data EventStore = EventStore
  { -- | Append an event to a stream at a given expected revision.
    --   Returns the local and global stream position if successful, or an Error on conflict or failure.
    appendToStream :: StreamId -> StreamPosition -> Event -> Task Error AppendResult,
    -- | Read events from a stream in forward direction starting from a given revision.
    --   Returns an array of events or an Error.
    readStreamForwardFrom :: StreamId -> StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read events from a stream in backward direction starting from a given revision.
    --   Useful for looking at recent events.
    readStreamBackwardFrom :: StreamId -> StreamPosition -> Limit -> Task Error (Array Event),
    -- | Read all events from a given stream.
    --   Equivalent to calling 'readStreamForwardFrom' with revision 0 and a high max count.
    readAllStreamEvents :: StreamId -> Task Error (Array Event),
    -- | Read a slice of all events across all streams starting from a global position.
    --   Useful for projections or audit logs.
    readAllEventsForwardFrom :: StreamPosition -> Limit -> Task Error (Array Event)
  }
