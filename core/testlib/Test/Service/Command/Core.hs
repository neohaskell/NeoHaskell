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
import Json qualified
import Service.Command (CommandResult (..), EventOf)
import Service.Event (InsertionType (..))
import Service.Event.StreamId qualified as StreamId
import Uuid qualified


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
  = CartCreated {cartId :: Uuid}
  | ItemAdded {cartId :: Uuid, itemId :: Uuid, amount :: Int}
  | ItemRemoved {cartId :: Uuid, itemId :: Uuid}
  | CartCheckedOut {cartId :: Uuid}
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
    CartCreated {cartId} ->
      CartEntity
        { cartId = cartId,
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

instance Command AddItemToCart where
  type EntityOf AddItemToCart = CartEntity


  streamId cmd = cmd.cartId |> Uuid.toText |> StreamId.fromText


  decide :: AddItemToCart -> Maybe CartEntity -> CommandResult CartEvent
  decide cmd entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then
            RejectCommand "Cannot add items to a checked out cart"
          else
            ItemAdded {cartId = cart.cartId, itemId = cmd.itemId, amount = cmd.amount}
              |> Array.wrap
              |> AcceptCommand ExistingStream


instance Command RemoveItemFromCart where
  type EntityOf RemoveItemFromCart = CartEntity


  streamId cmd = cmd.cartId |> Uuid.toText |> StreamId.fromText


  decide cmd entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then RejectCommand "Cannot remove items from a checked out cart"
          else do
            let hasItem = Array.any (\(id, _) -> id == cmd.itemId) cart.cartItems
            if not hasItem
              then RejectCommand "Item not in cart"
              else do
                let event = ItemRemoved {cartId = cart.cartId, itemId = cmd.itemId}
                Array.fromLinkedList [event]
                  |> AcceptCommand ExistingStream


instance Command CheckoutCart where
  type EntityOf CheckoutCart = CartEntity


  streamId cmd = cmd.cartId |> Uuid.toText |> StreamId.fromText


  decide _ entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then RejectCommand "Cart already checked out"
          else do
            if Array.isEmpty cart.cartItems
              then RejectCommand "Cannot checkout empty cart"
              else do
                let event = CartCheckedOut {cartId = cart.cartId}
                Array.fromLinkedList [event]
                  |> AcceptCommand ExistingStream
