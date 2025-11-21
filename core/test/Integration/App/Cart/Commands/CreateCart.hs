module Integration.App.Cart.Commands.CreateCart where

import Core


data CreateCart = CreateCart
  deriving (Generic)


type instance EntityOf CreateCart = CartEntity


instance Command CreateCart where
  streamId _ = panic "Not implemented"
  decide = panic "not implemented"
