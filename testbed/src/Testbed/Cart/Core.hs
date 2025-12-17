module Testbed.Cart.Core (
  CartEntity (..),
  CartEvent (..),
) where

import Core
import Json qualified


data CartEntity = CartEntity
  { cartId :: Uuid,
    items :: Array CartItem
  }
  deriving (Generic)


type instance EventOf CartEntity = CartEvent


data CartItem = CartItem
  { productId :: Uuid,
    amount :: Natural Int
  }
  deriving (Generic)


data CartEvent
  = CartCreated {entityId :: Uuid}
  deriving (Generic)


instance Json.FromJSON CartEvent


instance Json.ToJSON CartEvent