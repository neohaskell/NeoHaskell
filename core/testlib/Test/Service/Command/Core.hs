module Test.Service.Command.Core (
  -- Cart Command Examples
  AddItemToCart (..),
  RemoveItemFromCart (..),
  CheckoutCart (..),
  -- Cart Entity
  CartEntity (..),
  CartEvent (..),
  initialCartState,
  applyCartEvent,
) where

import Array qualified
import Core
import Decision qualified
import Json qualified
import Uuid ()


-- ============================================================================
-- Cart Commands (Regular, Non-Tenant)
-- ============================================================================

data AddItemToCart = AddItemToCart
  { cartId :: Uuid,
    itemId :: Uuid,
    amount :: Int
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON AddItemToCart


instance Json.FromJSON AddItemToCart


data RemoveItemFromCart = RemoveItemFromCart
  { cartId :: Uuid,
    itemId :: Uuid
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON RemoveItemFromCart


instance Json.FromJSON RemoveItemFromCart


data CheckoutCart = CheckoutCart
  { cartId :: Uuid
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CheckoutCart


instance Json.FromJSON CheckoutCart


-- ============================================================================
-- Cart Entity and Events
-- ============================================================================

data CartEntity = CartEntity
  { cartId :: Uuid,
    cartItems :: Array (Uuid, Int), -- (itemId, quantity)
    cartCheckedOut :: Bool
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CartEntity


instance Json.FromJSON CartEntity


data CartEvent
  = CartCreated {entityId :: Uuid}
  | ItemAdded {entityId :: Uuid, itemId :: Uuid, amount :: Int}
  | ItemRemoved {entityId :: Uuid, itemId :: Uuid}
  | CartCheckedOut {entityId :: Uuid}
  deriving (Eq, Show, Ord, Generic)


type instance EventOf CartEntity = CartEvent


instance Json.ToJSON CartEvent


instance Json.FromJSON CartEvent


initialCartState :: CartEntity
initialCartState =
  CartEntity
    { cartId = def,
      cartItems = Array.empty,
      cartCheckedOut = False
    }


applyCartEvent :: CartEvent -> CartEntity -> CartEntity
applyCartEvent event state = do
  case event of
    CartCreated {entityId} ->
      CartEntity
        { cartId = entityId,
          cartItems = Array.empty,
          cartCheckedOut = False
        }
    ItemAdded {itemId, amount} -> do
      let existingItems = state.cartItems
      let updatedItems = addOrUpdateItem existingItems itemId amount
      state
        { cartItems = updatedItems
        }
    ItemRemoved {itemId} -> do
      let updatedItems = Array.dropIf (\(id, _) -> id == itemId) state.cartItems
      state
        { cartItems = updatedItems
        }
    CartCheckedOut {} ->
      state
        { cartCheckedOut = True
        }


addOrUpdateItem :: Array (Uuid, Int) -> Uuid -> Int -> Array (Uuid, Int)
addOrUpdateItem items itemId amount = do
  let existing = Array.find (\(id, _) -> id == itemId) items
  case existing of
    Just (_, currentAmount) -> do
      let filtered = Array.takeIf (\(id, _) -> id != itemId) items
      Array.append filtered (Array.wrap (itemId, currentAmount + amount))
    Nothing ->
      Array.append items (Array.wrap (itemId, amount))


-- ============================================================================
-- Cart Command Instances
-- ============================================================================

type instance EntityOf AddItemToCart = CartEntity


instance Command AddItemToCart where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl :: AddItemToCart -> Maybe CartEntity -> Decision CartEvent
  decideImpl cmd entity =
    case entity of
      Nothing ->
        Decision.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then
            Decision.reject "Cannot add items to a checked out cart"
          else
            ItemAdded {entityId = cart.cartId, itemId = cmd.itemId, amount = cmd.amount}
              |> Array.wrap
              |> Decision.acceptExisting


type instance EntityOf RemoveItemFromCart = CartEntity


instance Command RemoveItemFromCart where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl cmd entity =
    case entity of
      Nothing ->
        Decision.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then Decision.reject "Cannot remove items from a checked out cart"
          else do
            let hasItem = Array.any (\(id, _) -> id == cmd.itemId) cart.cartItems
            if not hasItem
              then Decision.reject "Item not in cart"
              else do
                let event = ItemRemoved {entityId = cart.cartId, itemId = cmd.itemId}
                Array.fromLinkedList [event]
                  |> Decision.acceptExisting


type instance EntityOf CheckoutCart = CartEntity


instance Command CheckoutCart where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl _ entity =
    case entity of
      Nothing ->
        Decision.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then Decision.reject "Cart already checked out"
          else do
            if Array.isEmpty cart.cartItems
              then Decision.reject "Cannot checkout empty cart"
              else do
                let event = CartCheckedOut {entityId = cart.cartId}
                Array.fromLinkedList [event]
                  |> Decision.acceptExisting
