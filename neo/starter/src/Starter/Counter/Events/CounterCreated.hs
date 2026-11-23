module Starter.Counter.Events.CounterCreated (CounterCreated (..)) where

import Core
import Derive


data CounterCreated = CounterCreated
  { entityId :: Uuid
  , label :: Text
  }


derive ''CounterCreated eventPayload
