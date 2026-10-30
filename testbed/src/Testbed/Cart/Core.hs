module Testbed.Cart.Core (
  CartEntity (..),
  CartEvent (..),
  CartCreatedPayload (..),
  ItemAddedPayload (..),
  initialState,
) where

import Array qualified
import Core
import Json qualified
import Service.Command.Core (Event (..))
import Uuid qualified


data CartEntity = CartEntity
  { cartId :: Uuid,
    ownerId :: Text,
    items :: Array CartItem,
    savedIBAN :: Maybe Text
  }
  deriving (Generic)


instance Json.FromJSON CartEntity


instance Json.ToJSON CartEntity


instance Default CartEntity where
  def = initialState


data CartItem = CartItem
  { productId :: Uuid,
    amount :: Natural Int
  }
  deriving (Generic)


instance Json.FromJSON CartItem


instance Json.ToJSON CartItem


initialState :: CartEntity
initialState =
  CartEntity
    { cartId = Uuid.nil,
      ownerId = "",
      items = Array.empty,
      savedIBAN = Nothing
    }


type instance NameOf CartEntity = "CartEntity"


instance Entity CartEntity where
  initialStateImpl = initialState
  updateImpl = update


data CartEvent
  = CartCreated
      { entityId :: Uuid,
        ownerId :: Text
      }
  | ItemAdded
      { entityId :: Uuid,
        stockId :: Uuid,
        quantity :: Int
      }
  | IBANSaved
      { entityId :: Uuid,
        iban :: Text
      }
  deriving (Generic, Show)


data ItemAddedPayload = ItemAddedPayload
  { itemAddedEntityId :: Uuid,
    itemAddedStockId :: Uuid,
    itemAddedQuantity :: Int
  }


type instance EntityOf ItemAddedPayload = CartEntity


data CartCreatedPayload = CartCreatedPayload
  { cartCreatedEntityId :: Uuid,
    cartCreatedOwnerId :: Text
  }


type instance EntityOf CartCreatedPayload = CartEntity


instance EventVariantOf CartEvent CartCreatedPayload where
  fromVariant payload =
    CartCreated
      { entityId = payload.cartCreatedEntityId,
        ownerId = payload.cartCreatedOwnerId
      }
  toVariant eventValue = case eventValue of
    CartCreated {entityId, ownerId} ->
      Just CartCreatedPayload
        { cartCreatedEntityId = entityId,
          cartCreatedOwnerId = ownerId
        }
    ItemAdded {} -> Nothing
    IBANSaved {} -> Nothing


instance EventVariantOf CartEvent ItemAddedPayload where
  fromVariant payload =
    ItemAdded
      { entityId = payload.itemAddedEntityId,
        stockId = payload.itemAddedStockId,
        quantity = payload.itemAddedQuantity
      }
  toVariant eventValue = case eventValue of
    ItemAdded {entityId, stockId, quantity} ->
      Just ItemAddedPayload
        { itemAddedEntityId = entityId,
          itemAddedStockId = stockId,
          itemAddedQuantity = quantity
        }
    CartCreated {} -> Nothing
    IBANSaved {} -> Nothing


getEventEntityId :: CartEvent -> Uuid
getEventEntityId event = case event of
  CartCreated {entityId} -> entityId
  ItemAdded {entityId} -> entityId
  IBANSaved {entityId} -> entityId


type instance EventOf CartEntity = CartEvent


type instance EntityOf CartEvent = CartEntity


instance Event CartEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON CartEvent


instance Json.ToJSON CartEvent


update :: CartEvent -> CartEntity -> CartEntity
update event entity = case event of
  CartCreated {entityId, ownerId} ->
    CartEntity
      { cartId = entityId,
        ownerId = ownerId,
        items = Array.empty,
        savedIBAN = Nothing
      }
  ItemAdded {stockId, quantity} ->
    entity
      { items =
          entity.items
            |> Array.push
              CartItem
                { productId = stockId,
                  amount = makeNaturalOrPanic quantity
                }
      }
  IBANSaved {iban} -> entity {savedIBAN = Just iban}
