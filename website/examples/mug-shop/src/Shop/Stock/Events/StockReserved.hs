module Shop.Stock.Events.StockReserved (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }
  deriving (Eq)

deriveEvent ''Event
