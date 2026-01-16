module Testbed.Cart.Commands.AddItem (
  AddItem (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core


data AddItem = AddItem
  { cartId :: Uuid
  , stockId :: Uuid
  , quantity :: Int
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON AddItem


getEntityId :: AddItem -> Maybe Uuid
getEntityId cmd = Just cmd.cartId


decide :: AddItem -> Maybe CartEntity -> Decision CartEvent
decide cmd entity = case entity of
  Nothing ->
    Decider.reject "Cart not found!"
  Just cart ->
    if cmd.quantity <= 0
      then Decider.reject "Quantity must be positive"
      else
        Decider.acceptExisting
          [ ItemAdded
              { entityId = cart.cartId
              , stockId = cmd.stockId
              , quantity = cmd.quantity
              }
          ]


type instance EntityOf AddItem = CartEntity


type instance TransportOf AddItem = WebTransport


command ''AddItem
