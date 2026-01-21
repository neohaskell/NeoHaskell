module Testbed.Cart.Core (
  CartEntity (..),
  CartEvent (..),
  initialState,
) where

import Array qualified
import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data CartEntity = CartEntity
  { cartId :: Uuid,
    ownerId :: Text,
    items :: Array CartItem
  }
  deriving (Generic)


instance Json.FromJSON CartEntity


instance Json.ToJSON CartEntity


instance Default CartEntity where
  def = initialState


data CartItem = CartItem
  { productId :: Uuid,
    amount :: Natural Int
  }
  deriving (Generic)


instance Json.FromJSON CartItem


instance Json.ToJSON CartItem


initialState :: CartEntity
initialState =
  CartEntity
    { cartId = Uuid.nil,
      ownerId = "",
      items = Array.empty
    }


type instance NameOf CartEntity = "CartEntity"


instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update


data CartEvent
  = CartCreated
      { entityId :: Uuid,
        ownerId :: Text
      }
  | ItemAdded
      { entityId :: Uuid,
        stockId :: Uuid,
        quantity :: Int
      }
  deriving (Generic)


getEventEntityId :: CartEvent -> Uuid
getEventEntityId event = case event of
  CartCreated {entityId} -> entityId
  ItemAdded {entityId} -> entityId


type instance EventOf CartEntity = CartEvent


type instance EntityOf CartEvent = CartEntity


instance Event CartEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON CartEvent


instance Json.ToJSON CartEvent


update :: CartEvent -> CartEntity -> CartEntity
update event entity = case event of
  CartCreated {entityId, ownerId} ->
    CartEntity
      { cartId = entityId,
        ownerId = ownerId,
        items = Array.empty
      }
  ItemAdded {stockId, quantity} ->
    entity
      { items =
          entity.items
            |> Array.push
              CartItem
                { productId = stockId,
                  amount = makeNaturalOrPanic quantity
                }
      }