module Testbed.Cart.Commands.CreateCart (
  CreateCart (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext (..), UserClaims (..))
import Service.Command.Core (TransportOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Web (WebTransport)
import Testbed.Cart.Core
import Uuid qualified


data CreateCart = CreateCart
  deriving (Generic, Typeable, Show)


instance Json.FromJSON CreateCart


instance Json.ToJSON CreateCart


instance ToSchema CreateCart


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


-- | Create a new cart. Uses authenticated user as owner, or generates anonymous owner.
decide :: CreateCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ entity ctx = do
  case entity of
    Just _ ->
      Decider.reject "Cart already exists!"
    Nothing -> do
      cartId <- Decider.generateUuid
      ownerId <- case ctx.user of
        Just user -> pure user.sub
        Nothing -> do
          anonId <- Decider.generateUuid
          pure (Uuid.toText anonId)
      Decider.acceptNew [CartCreated {entityId = cartId, ownerId = ownerId}]


type instance EntityOf CreateCart = CartEntity


type instance TransportOf CreateCart = WebTransport


command ''CreateCart