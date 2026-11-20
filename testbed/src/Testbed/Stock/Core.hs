module Testbed.Stock.Core (
  StockEntity (..),
  StockEvent (..),
  StockInitializedPayload (..),
  StockReservedPayload (..),
  initialState,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data StockEntity = StockEntity
  { stockId :: Uuid,
    productId :: Uuid,
    available :: Int,
    reserved :: Int
  }
  deriving (Generic)


instance Json.FromJSON StockEntity


instance Json.ToJSON StockEntity


initialState :: StockEntity
initialState =
  StockEntity
    { stockId = Uuid.nil,
      productId = Uuid.nil,
      available = 0,
      reserved = 0
    }


type instance NameOf StockEntity = "StockEntity"


instance Entity StockEntity where
  initialStateImpl = initialState
  updateImpl = update


data StockEvent
  = StockInitialized
      { entityId :: Uuid,
        productId :: Uuid,
        available :: Int
      }
  | StockReserved
      { entityId :: Uuid,
        quantity :: Int,
        cartId :: Uuid
      }
  deriving (Generic, Show)


data StockInitializedPayload = StockInitializedPayload
  { initializedEntityId :: Uuid,
    initializedProductId :: Uuid,
    initializedAvailable :: Int
  }


type instance EntityOf StockInitializedPayload = StockEntity


data StockReservedPayload = StockReservedPayload
  { reservedEntityId :: Uuid,
    reservedQuantity :: Int,
    reservedCartId :: Uuid
  }


type instance EntityOf StockReservedPayload = StockEntity


instance EventVariantOf StockEvent StockInitializedPayload where
  fromVariant payload =
    StockInitialized
      { entityId = payload.initializedEntityId,
        productId = payload.initializedProductId,
        available = payload.initializedAvailable
      }
  toVariant eventValue = case eventValue of
    StockInitialized {entityId, productId, available} ->
      Just
        StockInitializedPayload
          { initializedEntityId = entityId,
            initializedProductId = productId,
            initializedAvailable = available
          }
    StockReserved {} -> Nothing


instance EventVariantOf StockEvent StockReservedPayload where
  fromVariant payload =
    StockReserved
      { entityId = payload.reservedEntityId,
        quantity = payload.reservedQuantity,
        cartId = payload.reservedCartId
      }
  toVariant eventValue = case eventValue of
    StockReserved {entityId, quantity, cartId} ->
      Just
        StockReservedPayload
          { reservedEntityId = entityId,
            reservedQuantity = quantity,
            reservedCartId = cartId
          }
    StockInitialized {} -> Nothing


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
      { stockId = entityId,
        productId = productId,
        available = available,
        reserved = 0
      }
  StockReserved {quantity} ->
    entity
      { available = entity.available - quantity,
        reserved = entity.reserved + quantity
      }
