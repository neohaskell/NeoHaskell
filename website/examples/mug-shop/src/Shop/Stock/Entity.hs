module Shop.Stock.Entity (StockEntity (..), initialState, update) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
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
  deriving (Generic)

instance Json.FromJSON StockEntity
instance Json.ToJSON StockEntity

initialState :: StockEntity
initialState = StockEntity {stockId = Uuid.nil, productId = Uuid.nil, available = 0, reserved = 0}

type instance NameOf StockEntity = "StockEntity"
type instance EventOf StockEntity = StockEvent
type instance EntityOf StockEvent = StockEntity

instance Entity StockEntity where
  initialStateImpl = initialState
  updateImpl = update

instance Event StockEvent where
  getEventEntityIdImpl = getEventEntityId

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
