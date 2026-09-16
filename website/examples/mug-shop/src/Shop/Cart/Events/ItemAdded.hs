module Shop.Cart.Events.ItemAdded (Event (..)) where

import Core
import Service.Event.TH qualified as EventTH

data Event = Event
  { entityId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Eq)

EventTH.event ''Event
