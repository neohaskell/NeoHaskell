module Integration.App.Cart.Core (
  CartEntity (..),
  CartEvent (..),
) where

import Core


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


data CartEvent = CartEvent
