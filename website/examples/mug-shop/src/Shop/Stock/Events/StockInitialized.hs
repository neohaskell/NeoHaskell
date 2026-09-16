module Shop.Stock.Events.StockInitialized (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , productId :: Uuid
  , available :: Int
  }
  deriving (Eq)

deriveEvent ''Event
