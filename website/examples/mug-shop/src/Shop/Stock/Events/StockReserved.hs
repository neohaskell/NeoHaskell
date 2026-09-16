module Shop.Stock.Events.StockReserved (Event (..)) where

import Core
import Service.Event.TH qualified as EventTH

data Event = Event
  { entityId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }
  deriving (Eq)

EventTH.event ''Event
