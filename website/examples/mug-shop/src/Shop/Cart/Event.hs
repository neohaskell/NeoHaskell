module Shop.Cart.Event (CartEvent (..), getEventEntityId) where

import Core
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Shop.Cart.Events.ItemAdded qualified as ItemAdded

data CartEvent
  = CartCreated CartCreated.Event
  | ItemAdded ItemAdded.Event
  deriving (Eq)

getEventEntityId :: CartEvent -> Uuid
getEventEntityId change = case change of
  CartCreated fact -> fact.entityId
  ItemAdded fact -> fact.entityId

deriveEvent ''CartEvent
