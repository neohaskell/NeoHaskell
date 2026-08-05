module Starter.Counter.Events.CounterCreated (Event (..)) where

import Core
import Json qualified


-- | A new counter was created with a human-readable label. Starting value is 0.
data Event = Event
  { entityId :: Uuid
  , label :: Text
  }
  deriving (Generic, Show)


instance Json.FromJSON Event


instance Json.ToJSON Event
