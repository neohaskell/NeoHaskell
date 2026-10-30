-- | Create a marked cart so the typed inbound example can be observed through the cart query.
module Testbed.Cart.Commands.CreateTypedInboundCart (
  CreateTypedInboundCart (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.Cart.Core (CartEntity, CartEvent (..))


data CreateTypedInboundCart = CreateTypedInboundCart


getEntityId :: CreateTypedInboundCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateTypedInboundCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ entity _ = case entity of
  Just _ -> Decider.reject "Cart already exists!"
  Nothing -> do
    cartId <- Decider.generateUuid
    Decider.acceptNew [CartCreated {entityId = cartId, ownerId = "typed-inbound-example"}]


type instance EntityOf CreateTypedInboundCart = CartEntity


type instance TransportsOf CreateTypedInboundCart = '[InternalTransport]


command ''CreateTypedInboundCart
