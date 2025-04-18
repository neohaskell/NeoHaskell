module Service.EventStore.InMemory (
  new,
) where

import Core
import DurableChannel (DurableChannel)
import Map qualified
import Service.Event
import Service.EventStore.Core


-- | For the in-memory event store, we will use a ConcurrentVar that stores
-- a map of stream IDs to a MutableArray of events.
-- This is the simplest way to have an EventStore that can be used in a test environment.
new :: Task Error EventStore
new = do
  let emptyStore :: Map StreamId (DurableChannel Event)
      emptyStore = Map.empty
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
