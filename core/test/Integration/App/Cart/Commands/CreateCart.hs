{-# LANGUAGE OverloadedLists #-}

module Integration.App.Cart.Commands.CreateCart (
  CreateCart (..),
  getEntityId,
  decide,
) where

import Core
import Decision qualified
import Integration.App.Cart.Core
import Service.CommandHandler.Core (deriveCommand)
import Uuid qualified


data CreateCart = CreateCart
  deriving (Generic, Typeable)


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      id <- Decision.generateUuid
      Decision.acceptNew [CartCreated {entityId = Uuid.toText id}]


type instance EntityOf CreateCart = CartEntity


deriveCommand ''CreateCart