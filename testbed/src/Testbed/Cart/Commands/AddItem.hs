module Testbed.Cart.Commands.AddItem (
  AddItem (..),
  getEntityId,
  decide,
) where

import Core
import Decision qualified
import Service.Api.WebApi (WebApi)
import Service.Command.Core (ApiOf)
import Service.CommandHandler.Core (command)
import Testbed.Cart.Core


data AddItem = CreateCart
  deriving (Generic, Typeable, Show)


getEntityId :: AddItem -> Maybe Uuid
getEntityId _ = Nothing


decide :: AddItem -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      id <- Decision.generateUuid
      Decision.acceptNew [CartCreated {entityId = id}]


type instance EntityOf AddItem = CartEntity


type instance ApiOf AddItem = WebApi


command ''AddItem