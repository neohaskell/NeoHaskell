module Testbed.Cart.Commands.CreateCart (
  CreateCart (..),
  getEntityId,
  decide,
) where

import Core
import Decision qualified
import Service.Command.Core (ApiOf, WebApi)
import Service.CommandHandler.Core (command)
import Testbed.Cart.Core


data CreateCart = CreateCart
  deriving (Generic, Typeable, Show)


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      id <- Decision.generateUuid
      Decision.acceptNew [CartCreated {entityId = id}]


type instance EntityOf CreateCart = CartEntity


type instance ApiOf CreateCart = WebApi


command ''CreateCart