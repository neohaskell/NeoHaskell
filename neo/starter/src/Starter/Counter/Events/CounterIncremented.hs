module Starter.Counter.Events.CounterIncremented (CounterIncremented (..)) where

import Core
import Derive


data CounterIncremented = CounterIncremented
  { entityId :: Uuid
  , amount :: Int
  }


derive ''CounterIncremented eventPayload
