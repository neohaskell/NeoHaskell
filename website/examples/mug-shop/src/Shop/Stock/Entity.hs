module Shop.Stock.Entity (StockEntity (..), initialState, update) where

import Core
import Shop.Stock.Event (StockEvent (..), getEventEntityId)
import Shop.Stock.Events.StockInitialized qualified as StockInitialized
import Shop.Stock.Events.StockReserved qualified as StockReserved
import Uuid qualified

data StockEntity = StockEntity
  { stockId :: Uuid
  , productId :: Uuid
  , available :: Int
  , reserved :: Int
  }

initialState :: StockEntity
initialState = StockEntity {stockId = Uuid.nil, productId = Uuid.nil, available = 0, reserved = 0}

update :: StockEvent -> StockEntity -> StockEntity
update change stock = case change of
  StockInitialized initialized ->
    StockEntity
      { stockId = initialized.entityId
      , productId = initialized.productId
      , available = initialized.available
      , reserved = 0
      }
  StockReserved reservation ->
    stock
      { available = stock.available - reservation.quantity
      , reserved = stock.reserved + reservation.quantity
      }

deriveEntity ''StockEntity ''StockEvent
