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


data CreateCart = CreateCart
  deriving (Generic)


getEntityId :: CreateCart -> Maybe Text
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      cartCreatedId <- Decision.generateUuid
      Decision.acceptNew [CartCreated {cartCreatedId}]


type Entity = CartEntity


deriveCommand ''CreateCart