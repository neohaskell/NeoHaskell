module Service.EventStore (
  EventStore (..),
  Limit (..),
  Error (..),
  newInMemoryEventStore,
) where

-- import Channel (Channel, new)
import Core
import Service.Event (Event, StreamId, StreamPosition)


-- import Task qualified

newtype Limit = Limit (Positive Int)
  deriving (Eq, Show, Ord, Generic)


data Error
  = StreamNotFound StreamId
  | EventNotFound StreamId StreamPosition
  | ConcurrencyConflict StreamId StreamPosition StreamPosition -- expected vs actual
  | StorageFailure Text -- Generic storage errors with message
  deriving (Eq, Show)


-- | An interface for the operations of an event store
data EventStore = EventStore
  { -- | Append an event to a stream at a given expected revision.
    --   Returns the next stream revision if successful, or an Error on conflict or failure.
    appendToStream :: StreamId -> StreamPosition -> Event -> Task Error StreamPosition,
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


-- | For the in-memory event store, we will use a ConcurrentVar that stores
-- a map of stream IDs to a MutableArray of events.
-- This is the simplest way to have an EventStore that can be used in a test environment.
newInMemoryEventStore :: Task Error EventStore
newInMemoryEventStore = do
  -- let emptyStore :: Map StreamId (Channel Event)
  --     emptyStore = Map.empty
  -- Create a ConcurrentVar to hold the event store
  -- store <- ConcurrentVar.new (emptyStore)
  -- Create the event store interface
  -- let eventStore =
  --       EventStore
  --         { appendToStream = appendToStreamImpl store,
  --           readStreamForwardFrom = readStreamForwardFromImpl store,
  --           readStreamBackwardFrom = readStreamBackwardFromImpl store,
  --           readAllStreamEvents = readAllStreamEventsImpl store,
  --           readAllEventsForwardFrom = readAllEventsForwardFromImpl store
  --         }
  -- Task.yield eventStore
  panic "not implemented yet"
