-- | Add two ordered item events to an existing cart.
-- Use 'AddTwoItems' to check exact event sequence assertions in business rules.
module Testbed.Cart.Commands.AddTwoItems (AddTwoItems (..), getEntityId, decide) where

import Array qualified
import Core
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core (CartEntity (..), CartEvent (..))


data AddTwoItems = AddTwoItems {cartId :: Uuid, firstStockId :: Uuid, secondStockId :: Uuid}


getEntityId :: AddTwoItems -> Maybe Uuid
getEntityId selected = Just selected.cartId


decide :: AddTwoItems -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide selected maybeCart _ = case maybeCart of
  Nothing -> Decider.reject "Cart not found!"
  Just cart -> do
    let nextQuantity = Array.length cart.items + 1
    Decider.acceptExisting
      [ ItemAdded {entityId = cart.cartId, stockId = selected.firstStockId, quantity = nextQuantity},
        ItemAdded {entityId = cart.cartId, stockId = selected.secondStockId, quantity = nextQuantity + 1}
      ]


type instance EntityOf AddTwoItems = CartEntity


type instance TransportsOf AddTwoItems = '[WebTransport]


command ''AddTwoItems
