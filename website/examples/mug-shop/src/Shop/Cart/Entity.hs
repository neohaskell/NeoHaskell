module Shop.Cart.Entity (CartEntity (..), initialState, update) where

import Core
import Json qualified
import Service.Command.Core (Event (..))
import Shop.Cart.Event (CartEvent (..), getEventEntityId)
import Shop.Cart.Events.CartCreated qualified as CartCreated
import Uuid qualified
import Array qualified
import Shop.Cart.Item (CartItem (..))
import Shop.Cart.Events.ItemAdded qualified as ItemAdded

data CartEntity = CartEntity
  { cartId :: Uuid
  , ownerId :: Text
  , items :: Array CartItem
  }
  deriving (Generic)

instance Json.FromJSON CartEntity
instance Json.ToJSON CartEntity

initialState :: CartEntity
initialState = CartEntity {cartId = Uuid.nil, ownerId = "", items = Array.empty}

instance Default CartEntity where
  def = initialState

type instance NameOf CartEntity = "CartEntity"
type instance EventOf CartEntity = CartEvent
type instance EntityOf CartEvent = CartEntity

instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update

instance Event CartEvent where
  getEventEntityIdImpl = getEventEntityId

update :: CartEvent -> CartEntity -> CartEntity
update change cart = case change of
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId, items = Array.empty}
  ItemAdded added ->
    cart {items = cart.items |> Array.push (CartItem {stockId = added.stockId, quantity = added.quantity})}
