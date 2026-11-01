-- | Accept an existing cart without adding an event.
-- Use 'ConfirmCart' to exercise rules about successful zero-event commands.
module Testbed.Cart.Commands.ConfirmCart (ConfirmCart (..), getEntityId, decide) where

import Array qualified
import Core
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core (CartEntity, CartEvent)


data ConfirmCart = ConfirmCart {cartId :: Uuid}


getEntityId :: ConfirmCart -> Maybe Uuid
getEntityId selected = Just selected.cartId


decide :: ConfirmCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ maybeCart _ = case maybeCart of
  Nothing -> Decider.reject "Cart not found!"
  Just _ -> Decider.acceptExisting (Array.empty :: Array CartEvent)


type instance EntityOf ConfirmCart = CartEntity


type instance TransportsOf ConfirmCart = '[WebTransport]


command ''ConfirmCart
