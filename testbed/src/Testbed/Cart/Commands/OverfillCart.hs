-- | Emit a batch larger than the in-memory event store accepts.
-- Use 'OverfillCart' to verify that rule failures retain service errors.
module Testbed.Cart.Commands.OverfillCart (OverfillCart (..), getEntityId, decide) where

import Array qualified
import Core
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core (CartEntity (..), CartEvent (..))


data OverfillCart = OverfillCart {cartId :: Uuid}


getEntityId :: OverfillCart -> Maybe Uuid
getEntityId selected = Just selected.cartId


decide :: OverfillCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ maybeCart _ = case maybeCart of
  Nothing -> Decider.reject "Cart not found!"
  Just cart -> Decider.acceptExisting (Array.repeat 101 (ItemAdded {entityId = cart.cartId, stockId = cart.cartId, quantity = 1}))


type instance EntityOf OverfillCart = CartEntity


type instance TransportsOf OverfillCart = '[WebTransport]


command ''OverfillCart
