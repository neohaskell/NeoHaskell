-- | # EventCounter Integration
--
-- A stateful integration that demonstrates the lifecycle pattern.
-- Maintains a counter per entity that tracks how many events were processed.
--
-- This simulates a real-world scenario where an integration might hold:
-- - A database connection pool
-- - An embedded interpreter
-- - A gRPC channel
-- - A WebSocket subscription
--
-- The counter is initialized when the worker spawns, incremented on each event,
-- and logged on cleanup (when the worker is reaped due to idle timeout).
module Testbed.Cart.Integrations.EventCounter (
  eventCounterIntegration,
  EventCounterState (..),
) where

import ConcurrentVar qualified
import Console qualified
import Core
import Integration.Lifecycle qualified as Lifecycle
import Service.Event.StreamId qualified as StreamId
import Task qualified


-- | State held by each entity's worker.
-- In a real integration, this might be a DB connection pool or interpreter.
data EventCounterState = EventCounterState
  { entityId :: Text
  , eventCount :: ConcurrentVar.ConcurrentVar Int
  }


-- | A stateful integration that counts events per entity.
--
-- Demonstrates the lifecycle pattern:
-- - initialize: Create counter for this entity
-- - processEvent: Increment counter, log count
-- - cleanup: Log final count when worker is reaped
eventCounterIntegration :: Lifecycle.OutboundConfig EventCounterState
eventCounterIntegration = Lifecycle.OutboundConfig
  { initialize = \streamId -> do
      let entityIdText = StreamId.toText streamId
      Console.print [fmt|[EventCounter] Initializing counter for entity: #{entityIdText}|]
      counter <- ConcurrentVar.containing 0
      Task.yield EventCounterState
        { entityId = entityIdText
        , eventCount = counter
        }

  , processEvent = \state _event -> do
      -- Atomically increment counter and get new value
      currentCount <- state.eventCount |> ConcurrentVar.modifyReturning (\n -> do
        let n' = n + 1
        Task.yield (n', n'))
      let entityIdText = state.entityId
      Console.print [fmt|[EventCounter] Entity #{entityIdText} processed event #{currentCount}|]
      -- This integration doesn't emit commands, just counts
      Task.yield []

  , cleanup = \state -> do
      finalCount <- state.eventCount |> ConcurrentVar.peek
      let entityIdText = state.entityId
      Console.print [fmt|[EventCounter] Cleanup for entity #{entityIdText}, final count: #{finalCount}|]
  }
