module Test.Service.Command.Core (
  -- Cart Command Examples
  AddItemToCart (..),
  RemoveItemFromCart (..),
  CheckoutCart (..),
  -- Authorization-aware Commands
  AuthenticatedAddItem (..),
  OwnedCartCheckout (..),
  -- Cart Entity (with owner support)
  CartEntity (..),
  OwnedCartEntity (..),
  initialCartState,
  initialOwnedCartState,
  applyCartEvent,
  applyOwnedCartEvent,
  -- Events
  OwnedCartEvent (..),
) where

import Array qualified
import Auth.Claims (UserClaims (..))
import Core
import Decider qualified
import Json qualified
import Service.Auth (RequestContext (..))
import Service.Command.Core (Event (..))
import Test.Service.EventStore.Core (CartEvent (..))
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


type instance EventOf CartEntity = CartEvent


type instance EntityOf CartEvent = CartEntity


instance Entity CartEntity where
  initialStateImpl = initialCartState
  updateImpl = applyCartEvent


instance Event CartEvent where
  getEventEntityIdImpl event =
    case event of
      CartCreated eid -> eid
      ItemAdded eid _ _ -> eid
      ItemRemoved eid _ -> eid
      CartCheckedOut eid -> eid


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
      CartEntity
        { cartId = state.cartId,
          cartItems = updatedItems,
          cartCheckedOut = state.cartCheckedOut
        }
    ItemRemoved {itemId} -> do
      let updatedItems = Array.dropIf (\(id, _) -> id == itemId) state.cartItems
      CartEntity
        { cartId = state.cartId,
          cartItems = updatedItems,
          cartCheckedOut = state.cartCheckedOut
        }
    CartCheckedOut {} ->
      CartEntity
        { cartId = state.cartId,
          cartItems = state.cartItems,
          cartCheckedOut = True
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


  decideImpl :: AddItemToCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
  decideImpl cmd entity _ctx =
    case entity of
      Nothing ->
        Decider.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then
            Decider.reject "Cannot add items to a checked out cart"
          else
            ItemAdded {entityId = cart.cartId, itemId = cmd.itemId, amount = cmd.amount}
              |> Array.wrap
              |> Decider.acceptExisting


type instance EntityOf RemoveItemFromCart = CartEntity


instance Command RemoveItemFromCart where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl :: RemoveItemFromCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
  decideImpl cmd entity _ctx =
    case entity of
      Nothing ->
        Decider.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then Decider.reject "Cannot remove items from a checked out cart"
          else do
            let hasItem = Array.any (\(id, _) -> id == cmd.itemId) cart.cartItems
            if not hasItem
              then Decider.reject "Item not in cart"
              else do
                let event = ItemRemoved {entityId = cart.cartId, itemId = cmd.itemId}
                [event]
                  |> Decider.acceptExisting


type instance EntityOf CheckoutCart = CartEntity


instance Command CheckoutCart where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl :: CheckoutCart -> Maybe CartEntity -> RequestContext -> Decision CartEvent
  decideImpl _cmd entity _ctx =
    case entity of
      Nothing ->
        Decider.reject "Cart does not exist"
      Just cart -> do
        if cart.cartCheckedOut
          then Decider.reject "Cart already checked out"
          else do
            if Array.isEmpty cart.cartItems
              then Decider.reject "Cannot checkout empty cart"
              else do
                let event = CartCheckedOut {entityId = cart.cartId}
                [event]
                  |> Decider.acceptExisting


-- ============================================================================
-- Authorization-Aware Commands (for testing RequestContext)
-- ============================================================================

-- | Command that requires authentication.
-- Rejects if ctx.user is Nothing.
data AuthenticatedAddItem = AuthenticatedAddItem
  { cartId :: Uuid,
    itemId :: Uuid,
    amount :: Int
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON AuthenticatedAddItem


instance Json.FromJSON AuthenticatedAddItem


type instance EntityOf AuthenticatedAddItem = CartEntity


instance Command AuthenticatedAddItem where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl :: AuthenticatedAddItem -> Maybe CartEntity -> RequestContext -> Decision CartEvent
  decideImpl cmd entity ctx =
    case ctx.user of
      Nothing ->
        Decider.reject "Authentication required"
      Just _user ->
        -- User is authenticated, proceed with normal logic
        case entity of
          Nothing ->
            Decider.reject "Cart does not exist"
          Just cart -> do
            if cart.cartCheckedOut
              then Decider.reject "Cannot add items to a checked out cart"
              else
                ItemAdded {entityId = cart.cartId, itemId = cmd.itemId, amount = cmd.amount}
                  |> Array.wrap
                  |> Decider.acceptExisting


-- ============================================================================
-- Owned Cart Entity (for ownership testing)
-- ============================================================================

-- | Cart entity with owner tracking for authorization tests.
data OwnedCartEntity = OwnedCartEntity
  { cartId :: Uuid,
    ownerId :: Text, -- User ID (sub claim from JWT)
    cartItems :: Array (Uuid, Int),
    cartCheckedOut :: Bool
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON OwnedCartEntity


instance Json.FromJSON OwnedCartEntity


-- | Events for owned cart.
data OwnedCartEvent
  = OwnedCartCreated {entityId :: Uuid, ownerId :: Text}
  | OwnedItemAdded {entityId :: Uuid, itemId :: Uuid, amount :: Int}
  | OwnedCartCheckedOut {entityId :: Uuid}
  deriving (Eq, Show, Generic)


instance Json.ToJSON OwnedCartEvent


instance Json.FromJSON OwnedCartEvent


type instance EventOf OwnedCartEntity = OwnedCartEvent


type instance EntityOf OwnedCartEvent = OwnedCartEntity


instance Entity OwnedCartEntity where
  initialStateImpl = initialOwnedCartState
  updateImpl = applyOwnedCartEvent


instance Event OwnedCartEvent where
  getEventEntityIdImpl event =
    case event of
      OwnedCartCreated eid _ -> eid
      OwnedItemAdded eid _ _ -> eid
      OwnedCartCheckedOut eid -> eid


initialOwnedCartState :: OwnedCartEntity
initialOwnedCartState =
  OwnedCartEntity
    { cartId = def,
      ownerId = "",
      cartItems = Array.empty,
      cartCheckedOut = False
    }


applyOwnedCartEvent :: OwnedCartEvent -> OwnedCartEntity -> OwnedCartEntity
applyOwnedCartEvent event state = do
  case event of
    OwnedCartCreated {entityId, ownerId} ->
      OwnedCartEntity
        { cartId = entityId,
          ownerId = ownerId,
          cartItems = Array.empty,
          cartCheckedOut = False
        }
    OwnedItemAdded {itemId, amount} -> do
      let existingItems = state.cartItems
      let updatedItems = addOrUpdateItem existingItems itemId amount
      OwnedCartEntity
        { cartId = state.cartId,
          ownerId = state.ownerId,
          cartItems = updatedItems,
          cartCheckedOut = state.cartCheckedOut
        }
    OwnedCartCheckedOut {} ->
      OwnedCartEntity
        { cartId = state.cartId,
          ownerId = state.ownerId,
          cartItems = state.cartItems,
          cartCheckedOut = True
        }


-- | Command that checks ownership.
-- Rejects if ctx.user.sub doesn't match entity.ownerId.
data OwnedCartCheckout = OwnedCartCheckout
  { cartId :: Uuid
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON OwnedCartCheckout


instance Json.FromJSON OwnedCartCheckout


type instance EntityOf OwnedCartCheckout = OwnedCartEntity


instance Command OwnedCartCheckout where
  getEntityIdImpl cmd = cmd.cartId |> Just


  decideImpl :: OwnedCartCheckout -> Maybe OwnedCartEntity -> RequestContext -> Decision OwnedCartEvent
  decideImpl _cmd entity ctx =
    case ctx.user of
      Nothing ->
        Decider.reject "Authentication required"
      Just user ->
        case entity of
          Nothing ->
            Decider.reject "Cart does not exist"
          Just cart -> do
            -- Check ownership
            if cart.ownerId != user.sub
              then Decider.reject "You do not own this cart"
              else do
                if cart.cartCheckedOut
                  then Decider.reject "Cart already checked out"
                  else do
                    if Array.isEmpty cart.cartItems
                      then Decider.reject "Cannot checkout empty cart"
                      else do
                        let event = OwnedCartCheckedOut {entityId = cart.cartId}
                        [event]
                          |> Decider.acceptExisting
