module Integration.App.Cart.Commands.CreateCart (
  CreateCart (..),
  getEntityId,
  decide,
) where

import Core
import Integration.App.Cart.Entity (CartEntity)


data CreateCart = CreateCart
  deriving (Generic)


type Entity = CartEntity


getEntityId :: CreateCart -> StreamId
getEntityId _ = panic "Not implemented"


decide :: CreateCart -> Maybe Entity -> CommandResult CartEvent
decide = panic "Not implemented"


-- VVVVVV      should be auto generated     VVVVVV

type instance EntityOf CreateCart = Entity


instance Command CreateCart where
  getEntityIdImpl = getEntityId
  decideImpl = decide
