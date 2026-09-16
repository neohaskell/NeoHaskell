module Shop.Cart.Events.CartCreated (Event (..)) where

import Core
import Service.Event.TH qualified as EventTH

data Event = Event
  { entityId :: Uuid
  , ownerId :: Text
  }
  deriving (Eq)

EventTH.event ''Event
