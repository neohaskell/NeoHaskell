module Testbed.Cart.Core (
  CartEntity (..),
  CartEvent (..),
  initialState,
) where

import Array qualified
import Core
import Json qualified
import Service.Command (Entity (..))
import Uuid qualified


data CartEntity = CartEntity
  { cartId :: Uuid,
    items :: Array CartItem
  }
  deriving (Generic)


data CartItem = CartItem
  { productId :: Uuid,
    amount :: Natural Int
  }
  deriving (Generic)


initialState :: CartEntity
initialState =
  CartEntity
    { cartId =
        Uuid.nil,
      items =
        Array.empty
    }


instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update


data CartEvent
  = CartCreated {entityId :: Uuid}
  deriving (Generic)


type instance EventOf CartEntity = CartEvent


instance Json.FromJSON CartEvent


instance Json.ToJSON CartEvent


update :: CartEvent -> CartEntity -> CartEntity
update event _ =
  case event of
    CartCreated {entityId} ->
      CartEntity
        { cartId = entityId,
          items = Array.empty
        }