module Test.Service.Command.Core (
  -- Cart Command Examples
  AddItemToCart (..),
  RemoveItemFromCart (..),
  CheckoutCart (..),
  -- Order Command Examples (Tenant)
  CreateOrder (..),
  CancelOrder (..),
  -- Cart Entity
  CartEntity (..),
  CartEvent (..),
  initialCartState,
  applyCartEvent,
  -- Order Entity (Tenant)
  OrderEntity (..),
  OrderEvent (..),
  OrderItem (..),
  initialOrderState,
  applyOrderEvent,
) where

import Core
import Json qualified
import Service.Command (Command (..), CommandResult (..), TenantCommand (..))
import Service.Event (InsertionType (..))
import Service.Event.StreamId (StreamId)


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
  { id :: Uuid,
    items :: Array (Uuid, Int), -- (itemId, quantity)
    checkedOut :: Bool,
    version :: Int
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


instance Json.ToJSON CartEvent


instance Json.FromJSON CartEvent


initialCartState :: CartEntity
initialCartState =
  CartEntity
    { id = Uuid.nil,
      items = Array.empty,
      checkedOut = False,
      version = 0
    }


applyCartEvent :: CartEvent -> CartEntity -> CartEntity
applyCartEvent event state = do
  let newVersion = state.version + 1
  case event of
    CartCreated {cartId} ->
      CartEntity
        { id = cartId,
          items = Array.empty,
          checkedOut = False,
          version = newVersion
        }
    ItemAdded {itemId, amount} -> do
      let existingItems = state.items
      let updatedItems = addOrUpdateItem existingItems itemId amount
      state
        { items = updatedItems,
          version = newVersion
        }
    ItemRemoved {itemId} -> do
      let updatedItems = Array.filter (\(id, _) -> id /= itemId) state.items
      state
        { items = updatedItems,
          version = newVersion
        }
    CartCheckedOut {} ->
      state
        { checkedOut = True,
          version = newVersion
        }


addOrUpdateItem :: Array (Uuid, Int) -> Uuid -> Int -> Array (Uuid, Int)
addOrUpdateItem items itemId amount = do
  let existing = Array.find (\(id, _) -> id == itemId) items
  case existing of
    Just (_, currentAmount) -> do
      let filtered = Array.filter (\(id, _) -> id /= itemId) items
      Array.append filtered (Array.singleton (itemId, currentAmount + amount))
    Nothing ->
      Array.append items (Array.singleton (itemId, amount))


-- ============================================================================
-- Cart Command Instances
-- ============================================================================

instance Command AddItemToCart where
  type Entity = CartEntity

  streamId cmd = StreamId cmd.cartId

  decide cmd entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.checkedOut
          then RejectCommand "Cannot add items to a checked out cart"
          else do
            let event = ItemAdded {cartId = cart.id, itemId = cmd.itemId, amount = cmd.amount}
            Array.ofLinkedList [event]
              |> AcceptCommand ExistingStream


instance Command RemoveItemFromCart where
  type Entity = CartEntity

  streamId cmd = StreamId cmd.cartId

  decide cmd entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.checkedOut
          then RejectCommand "Cannot remove items from a checked out cart"
          else do
            let hasItem = Array.any (\(id, _) -> id == cmd.itemId) cart.items
            if not hasItem
              then RejectCommand "Item not in cart"
              else do
                let event = ItemRemoved {cartId = cart.id, itemId = cmd.itemId}
                Array.ofLinkedList [event]
                  |> AcceptCommand ExistingStream


instance Command CheckoutCart where
  type Entity = CartEntity

  streamId cmd = StreamId cmd.cartId

  decide cmd entity =
    case entity of
      Nothing ->
        RejectCommand "Cart does not exist"
      Just cart -> do
        if cart.checkedOut
          then RejectCommand "Cart already checked out"
          else do
            if Array.isEmpty cart.items
              then RejectCommand "Cannot checkout empty cart"
              else do
                let event = CartCheckedOut {cartId = cart.id}
                Array.ofLinkedList [event]
                  |> AcceptCommand ExistingStream


-- ============================================================================
-- Order Commands (Tenant)
-- ============================================================================

data OrderItem = OrderItem
  { productId :: Uuid,
    quantity :: Int,
    price :: Int -- in cents
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON OrderItem


instance Json.FromJSON OrderItem


data CreateOrder = CreateOrder
  { orderId :: Uuid,
    customerId :: Uuid,
    items :: Array OrderItem
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CreateOrder


instance Json.FromJSON CreateOrder


data CancelOrder = CancelOrder
  { orderId :: Uuid
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CancelOrder


instance Json.FromJSON CancelOrder


-- ============================================================================
-- Order Entity and Events
-- ============================================================================

data OrderEntity = OrderEntity
  { id :: Uuid,
    tenantId :: Uuid,
    customerId :: Uuid,
    items :: Array OrderItem,
    cancelled :: Bool,
    version :: Int
  }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON OrderEntity


instance Json.FromJSON OrderEntity


data OrderEvent
  = OrderCreated
      { orderId :: Uuid,
        tenantId :: Uuid,
        customerId :: Uuid,
        items :: Array OrderItem
      }
  | OrderCancelled {orderId :: Uuid}
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON OrderEvent


instance Json.FromJSON OrderEvent


initialOrderState :: OrderEntity
initialOrderState =
  OrderEntity
    { id = Uuid.nil,
      tenantId = Uuid.nil,
      customerId = Uuid.nil,
      items = Array.empty,
      cancelled = False,
      version = 0
    }


applyOrderEvent :: OrderEvent -> OrderEntity -> OrderEntity
applyOrderEvent event state = do
  let newVersion = state.version + 1
  case event of
    OrderCreated {orderId, tenantId, customerId, items} ->
      OrderEntity
        { id = orderId,
          tenantId = tenantId,
          customerId = customerId,
          items = items,
          cancelled = False,
          version = newVersion
        }
    OrderCancelled {} ->
      state
        { cancelled = True,
          version = newVersion
        }


-- ============================================================================
-- Order Command Instances (Tenant)
-- ============================================================================

instance TenantCommand CreateOrder where
  type Entity = OrderEntity

  streamId cmd tenantId = do
    let tenantText = Uuid.toText tenantId
    let orderText = Uuid.toText cmd.orderId
    StreamId (tenantText <> "-" <> orderText)

  decide cmd entity tenantId =
    case entity of
      Just _existingOrder ->
        RejectCommand "Order already exists"
      Nothing -> do
        if Array.isEmpty cmd.items
          then RejectCommand "Cannot create order with no items"
          else do
            let event =
                  OrderCreated
                    { orderId = cmd.orderId,
                      tenantId = tenantId,
                      customerId = cmd.customerId,
                      items = cmd.items
                    }
            Array.ofLinkedList [event]
              |> AcceptCommand StreamCreation


instance TenantCommand CancelOrder where
  type Entity = OrderEntity

  streamId cmd tenantId = do
    let tenantText = Uuid.toText tenantId
    let orderText = Uuid.toText cmd.orderId
    StreamId (tenantText <> "-" <> orderText)

  decide cmd entity _tenantId =
    case entity of
      Nothing ->
        RejectCommand "Order does not exist"
      Just order -> do
        if order.cancelled
          then RejectCommand "Order already cancelled"
          else do
            let event = OrderCancelled {orderId = order.id}
            Array.ofLinkedList [event]
              |> AcceptCommand ExistingStream
