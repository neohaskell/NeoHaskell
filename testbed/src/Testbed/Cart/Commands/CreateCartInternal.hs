module Testbed.Cart.Commands.CreateCartInternal (
  CreateCartInternal (..),
  getEntityId,
  decide,
) where

import Core
import Json qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Internal (InternalTransport)
import Testbed.Cart.Commands.CreateCart qualified as CreateCart
import Testbed.Cart.Core


data CreateCartInternal = CreateCartInternal
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateCartInternal


instance Json.ToJSON CreateCartInternal


getEntityId :: CreateCartInternal -> Maybe Uuid
getEntityId _ = Nothing


-- | Internal integration command that reuses CreateCart business logic.
decide :: CreateCartInternal -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ entity ctx =
  CreateCart.decide CreateCart.CreateCart entity ctx


type instance EntityOf CreateCartInternal = CartEntity


type instance TransportsOf CreateCartInternal = '[InternalTransport]


command ''CreateCartInternal
