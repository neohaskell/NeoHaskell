module Integration.App where

import Core


data CreateCart = CreateCart
  deriving (Generic)


type instance EntityOf CreateCart = CartEntity


instance Command CreateCart where
  streamId _ = panic "Not implemented"


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
