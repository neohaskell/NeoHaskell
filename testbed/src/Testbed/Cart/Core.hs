module Testbed.Cart.Core (
  CartEntity (..),
  CartEvent (..),
  initialState,
) where

import Array qualified
import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data CartEntity = CartEntity
  { cartId :: Uuid,
    items :: Array CartItem
  }
  deriving (Generic)


data CartItem = CartItem
  { productId :: Uuid,
    amount :: Natural Int
  }
  deriving (Generic)


initialState :: CartEntity
initialState =
  CartEntity
    { cartId =
        Uuid.nil,
      items =
        Array.empty
    }


type instance NameOf CartEntity = "CartEntity"


instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update


data CartEvent
  = CartCreated {entityId :: Uuid}
  deriving (Generic)


getEventEntityId :: CartEvent -> Uuid
getEventEntityId event =
  case event of
    CartCreated entityId -> entityId


type instance EventOf CartEntity = CartEvent


type instance EntityOf CartEvent = CartEntity


instance Event CartEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON CartEvent


instance Json.ToJSON CartEvent


update :: CartEvent -> CartEntity -> CartEntity
update event _ =
  case event of
    CartCreated {entityId} ->
      CartEntity
        { cartId = entityId,
          items = Array.empty
        }