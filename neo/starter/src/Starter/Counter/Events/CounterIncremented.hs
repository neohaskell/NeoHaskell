module Starter.Counter.Events.CounterIncremented (Event (..)) where

import Core
import Json qualified


-- | A counter's value was incremented by `amount`.
data Event = Event
  { entityId :: Uuid
  , amount :: Int
  }
  deriving (Generic, Show)


instance Json.FromJSON Event


instance Json.ToJSON Event
