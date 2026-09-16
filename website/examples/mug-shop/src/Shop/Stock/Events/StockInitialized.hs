module Shop.Stock.Events.StockInitialized (Event (..)) where

import Core
import Service.Event.TH qualified as EventTH

data Event = Event
  { entityId :: Uuid
  , productId :: Uuid
  , available :: Int
  }
  deriving (Eq)

EventTH.event ''Event
