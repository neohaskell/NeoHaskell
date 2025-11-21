{-# LANGUAGE OverloadedLists #-}

module Integration.App.Cart.Commands.CreateCart (
  CreateCart (..),
  getEntityId,
  decide,
) where

import Core
import Decision qualified
import Integration.App.Cart.Core


data CreateCart = CreateCart
  deriving (Generic)


type Entity = CartEntity


getEntityId :: CreateCart -> Maybe Text
getEntityId _ = Nothing


decide :: CreateCart -> Maybe Entity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      cartCreatedId <- Decision.generateUuid
      Decision.acceptNew [CartCreated {cartCreatedId}]


-- VVVVVV      should be auto generated     VVVVVV

type instance EntityOf CreateCart = Entity


instance Command CreateCart where
  getEntityIdImpl = getEntityId
  decideImpl = decide
