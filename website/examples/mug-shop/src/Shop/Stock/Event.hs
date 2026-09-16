module Shop.Stock.Event (StockEvent (..), getEventEntityId) where

import Core
import Shop.Stock.Events.StockInitialized qualified as StockInitialized
import Shop.Stock.Events.StockReserved qualified as StockReserved

data StockEvent
  = StockInitialized StockInitialized.Event
  | StockReserved StockReserved.Event
  deriving (Eq)

getEventEntityId :: StockEvent -> Uuid
getEventEntityId change = case change of
  StockInitialized fact -> fact.entityId
  StockReserved fact -> fact.entityId

deriveEvent ''StockEvent
