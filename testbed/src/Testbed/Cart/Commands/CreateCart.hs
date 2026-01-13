module Testbed.Cart.Commands.CreateCart (
  CreateCart (..),
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


data CreateCart = CreateCart
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateCart


instance Json.ToJSON CreateCart


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decider.reject "Cart already exists!"
    Nothing -> do
      id <- Decider.generateUuid
      Decider.acceptNew [CartCreated {entityId = id}]


type instance EntityOf CreateCart = CartEntity


type instance TransportOf CreateCart = WebTransport


command ''CreateCart