module Integration.App.Cart.Commands.CreateCart (
  CreateCart (..),
  WebApi (..),
  getEntityId,
  decide,
) where

import Core
import Decision qualified
import Integration.App.Cart.Core
import Service.CommandHandler.Core (deriveCommand)
import Service.Protocol (ApiFor)


-- | WebApi type for testing - carries HTTP API configuration
data WebApi = WebApi
  { port :: Int
  }
  deriving (Eq, Show, Generic, Typeable)


data CreateCart = CreateCart
  deriving (Generic, Typeable)


getEntityId :: CreateCart -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCart -> Maybe CartEntity -> Decision CartEvent
decide _ entity = do
  case entity of
    Just _ ->
      Decision.reject "Cart already exists!"
    Nothing -> do
      id <- Decision.generateUuid
      Decision.acceptNew [CartCreated {entityId = id}]


type instance EntityOf CreateCart = CartEntity


type instance ApiFor CreateCart = '[WebApi]


deriveCommand ''CreateCart