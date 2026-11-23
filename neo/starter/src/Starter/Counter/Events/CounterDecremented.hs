module Starter.Counter.Events.CounterDecremented (CounterDecremented (..)) where

import Core
import Derive


data CounterDecremented = CounterDecremented
  { entityId :: Uuid
  , amount :: Int
  }


derive ''CounterDecremented eventPayload
