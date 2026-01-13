module Testbed.Stock.Core (
  StockEntity (..),
  StockEvent (..),
  initialState,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))


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
initialState =
  StockEntity
    { stockId = Uuid.nil
    , productId = Uuid.nil
    , available = 0
    , reserved = 0
    }


type instance NameOf StockEntity = "StockEntity"


instance Entity StockEntity where
  initialStateImpl = initialState
  updateImpl = update


data StockEvent
  = StockInitialized
      { entityId :: Uuid
      , productId :: Uuid
      , available :: Int
      }
  | StockReserved
      { entityId :: Uuid
      , quantity :: Int
      , cartId :: Uuid
      }
  deriving (Generic)


getEventEntityId :: StockEvent -> Uuid
getEventEntityId event = case event of
  StockInitialized {entityId} -> entityId
  StockReserved {entityId} -> entityId


type instance EventOf StockEntity = StockEvent


type instance EntityOf StockEvent = StockEntity


instance Event StockEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON StockEvent


instance Json.ToJSON StockEvent


update :: StockEvent -> StockEntity -> StockEntity
update event entity = case event of
  StockInitialized {entityId, productId, available} ->
    StockEntity
      { stockId = entityId
      , productId = productId
      , available = available
      , reserved = 0
      }
  StockReserved {quantity} ->
    entity
      { available = entity.available - quantity
      , reserved = entity.reserved + quantity
      }
