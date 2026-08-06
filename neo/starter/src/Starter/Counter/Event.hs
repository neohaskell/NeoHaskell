module Starter.Counter.Event (
  CounterEvent (..),
  getEventEntityId,
) where

import Core
import Json qualified
import Starter.Counter.Events.CounterCreated qualified as CounterCreated
import Starter.Counter.Events.CounterDecremented qualified as CounterDecremented
import Starter.Counter.Events.CounterIncremented qualified as CounterIncremented


-- | Sum of every event type in this bounded context.
--
-- Adding a new event is a three-step process:
--   1. Create `src/Starter/Counter/Events/YourEvent.hs`.
--   2. Add a variant below + a branch in `getEventEntityId`.
--   3. Add a case in `update` inside `Starter.Counter.Entity`.
data CounterEvent
  = CounterCreated CounterCreated.Event
  | CounterIncremented CounterIncremented.Event
  | CounterDecremented CounterDecremented.Event
  deriving (Generic, Show)


getEventEntityId :: CounterEvent -> Uuid
getEventEntityId event = case event of
  CounterCreated e -> e.entityId
  CounterIncremented e -> e.entityId
  CounterDecremented e -> e.entityId


instance Json.FromJSON CounterEvent


instance Json.ToJSON CounterEvent
