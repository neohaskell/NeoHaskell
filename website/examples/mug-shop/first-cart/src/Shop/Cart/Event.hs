module Shop.Cart.Event (CartEvent (..), getEventEntityId) where

import Core
import Service.Event.TH qualified as EventTH
import Shop.Cart.Events.CartCreated qualified as CartCreated

data CartEvent
  = CartCreated CartCreated.Event
  deriving (Eq)

getEventEntityId :: CartEvent -> Uuid
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId

EventTH.event ''CartEvent
