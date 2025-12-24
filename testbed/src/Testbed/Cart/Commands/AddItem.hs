module Testbed.Cart.Commands.AddItem (
  AddItem (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Transport.Web (WebTransport)
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Testbed.Cart.Core


data AddItem = AddItem
  deriving (Generic, Typeable, Show)


instance Json.FromJSON AddItem


getEntityId :: AddItem -> Maybe Uuid
getEntityId _ = Nothing


decide :: AddItem -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decider.reject "Cart already exists!"
    Nothing -> do
      id <- Decider.generateUuid
      Decider.acceptNew [CartCreated {entityId = id}]


type instance EntityOf AddItem = CartEntity


type instance TransportOf AddItem = WebTransport


command ''AddItem