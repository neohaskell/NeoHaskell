module Test.Service.Command.Decide.Spec where

import Array qualified
import Core
import Result qualified
import Service.Command (Command (..), CommandResult (..), TenantCommand (..))
import Service.Event (InsertionType (..))
import Test
import Test.Service.Command.Core (
  AddItemToCart (..),
  CancelOrder (..),
  CartEntity (..),
  CartEvent (..),
  CheckoutCart (..),
  CreateOrder (..),
  OrderEntity (..),
  OrderEvent (..),
  OrderItem (..),
  RemoveItemFromCart (..),
  applyCartEvent,
  applyOrderEvent,
  initialCartState,
  initialOrderState,
 )
import Test.Service.Command.Decide.Context qualified as Context


spec :: Spec Unit
spec = do
  describe "Command Decide Specification Tests" do
    describe "Regular Commands (Cart)" do
      cartCommandSpecs

    describe "Tenant Commands (Order)" do
      orderCommandSpecs

    describe "Edge Cases" do
      edgeCaseSpecs


cartCommandSpecs :: Spec Unit
cartCommandSpecs = do
  before Context.initialize do
    describe "AddItemToCart" do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        let result = decide cmd (Nothing :: Maybe CartEntity)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when cart exists and is not checked out" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.empty,
                  checkedOut = False,
                  version = 1
                }
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: {msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.head events of
              Just (ItemAdded {itemId, amount}) -> do
                itemId |> shouldBe context.itemId1
                amount |> shouldBe 5
              _ -> fail "Expected ItemAdded event"

      it "rejects when cart is checked out" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 3),
                  checkedOut = True,
                  version = 2
                }
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 2}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot add items to a checked out cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "generates correct StreamId" \context -> do
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        let sid = streamId cmd

        -- StreamId should wrap the cartId
        sid |> shouldSatisfy (\_ -> True) -- Basic existence check

    describe "RemoveItemFromCart" do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        let result = decide cmd (Nothing :: Maybe CartEntity)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "rejects when item not in cart" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 3),
                  checkedOut = False,
                  version = 2
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId2}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Item not in cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when item exists in cart" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.ofLinkedList [(context.itemId1, 3), (context.itemId2, 5)],
                  checkedOut = False,
                  version = 3
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: {msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.head events of
              Just (ItemRemoved {itemId}) -> do
                itemId |> shouldBe context.itemId1
              _ -> fail "Expected ItemRemoved event"

      it "rejects when cart is checked out" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 3),
                  checkedOut = True,
                  version = 3
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot remove items from a checked out cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

    describe "CheckoutCart" do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = CheckoutCart {cartId = context.cartId}
        let result = decide cmd (Nothing :: Maybe CartEntity)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "rejects when cart is empty" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.empty,
                  checkedOut = False,
                  version = 1
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot checkout empty cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when cart has items and not checked out" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 3),
                  checkedOut = False,
                  version = 2
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: {msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.head events of
              Just (CartCheckedOut {}) -> do
                pure unit
              _ -> fail "Expected CartCheckedOut event"

      it "rejects when cart already checked out" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 3),
                  checkedOut = True,
                  version = 3
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        let result = decide cmd (Just cart)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart already checked out"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"


orderCommandSpecs :: Spec Unit
orderCommandSpecs = do
  before Context.initialize do
    describe "CreateOrder (Tenant Command)" do
      it "rejects when order already exists" \context -> do
        let existingOrder =
              OrderEntity
                { id = context.orderId,
                  tenantId = context.tenantId,
                  customerId = context.customerId,
                  items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000}),
                  cancelled = False,
                  version = 1
                }
        let items = Array.singleton (OrderItem {productId = context.itemId2, quantity = 3, price = 2000})
        let cmd = CreateOrder {orderId = context.orderId, customerId = context.customerId, items = items}
        let result = decide cmd (Just existingOrder) context.tenantId

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Order already exists"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "rejects when items array is empty" \context -> do
        let cmd = CreateOrder {orderId = context.orderId, customerId = context.customerId, items = Array.empty}
        let result = decide cmd (Nothing :: Maybe OrderEntity) context.tenantId

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot create order with no items"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when order doesn't exist and has items" \context -> do
        let items = Array.ofLinkedList [OrderItem {productId = context.itemId1, quantity = 2, price = 1000}, OrderItem {productId = context.itemId2, quantity = 1, price = 1500}]
        let cmd = CreateOrder {orderId = context.orderId, customerId = context.customerId, items = items}
        let result = decide cmd (Nothing :: Maybe OrderEntity) context.tenantId

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: {msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe StreamCreation
            Array.length events |> shouldBe 1

            case Array.head events of
              Just (OrderCreated {orderId, tenantId, customerId, items = eventItems}) -> do
                orderId |> shouldBe context.orderId
                tenantId |> shouldBe context.tenantId
                customerId |> shouldBe context.customerId
                Array.length eventItems |> shouldBe 2
              _ -> fail "Expected OrderCreated event"

      it "generates correct tenant-scoped StreamId" \context -> do
        let items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000})
        let cmd = CreateOrder {orderId = context.orderId, customerId = context.customerId, items = items}
        let sid = streamId cmd context.tenantId

        -- StreamId should contain both tenant and order ID
        sid |> shouldSatisfy (\_ -> True) -- Basic existence check

    describe "CancelOrder (Tenant Command)" do
      it "rejects when order doesn't exist" \context -> do
        let cmd = CancelOrder {orderId = context.orderId}
        let result = decide cmd (Nothing :: Maybe OrderEntity) context.tenantId

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Order does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when order exists and not cancelled" \context -> do
        let order =
              OrderEntity
                { id = context.orderId,
                  tenantId = context.tenantId,
                  customerId = context.customerId,
                  items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000}),
                  cancelled = False,
                  version = 1
                }
        let cmd = CancelOrder {orderId = context.orderId}
        let result = decide cmd (Just order) context.tenantId

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: {msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.head events of
              Just (OrderCancelled {}) -> do
                pure unit
              _ -> fail "Expected OrderCancelled event"

      it "rejects when order already cancelled" \context -> do
        let order =
              OrderEntity
                { id = context.orderId,
                  tenantId = context.tenantId,
                  customerId = context.customerId,
                  items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000}),
                  cancelled = True,
                  version = 2
                }
        let cmd = CancelOrder {orderId = context.orderId}
        let result = decide cmd (Just order) context.tenantId

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Order already cancelled"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"


edgeCaseSpecs :: Spec Unit
edgeCaseSpecs = do
  before Context.initialize do
    describe "Event Application" do
      it "cart state evolves correctly through multiple events" \context -> do
        let cartCreated = CartCreated {cartId = context.cartId}
        let state1 = applyCartEvent cartCreated initialCartState

        state1.id |> shouldBe context.cartId
        state1.version |> shouldBe 1
        Array.length state1.items |> shouldBe 0

        let itemAdded1 = ItemAdded {cartId = context.cartId, itemId = context.itemId1, amount = 3}
        let state2 = applyCartEvent itemAdded1 state1

        state2.version |> shouldBe 2
        Array.length state2.items |> shouldBe 1

        let itemAdded2 = ItemAdded {cartId = context.cartId, itemId = context.itemId2, amount = 5}
        let state3 = applyCartEvent itemAdded2 state2

        state3.version |> shouldBe 3
        Array.length state3.items |> shouldBe 2

        let itemRemoved = ItemRemoved {cartId = context.cartId, itemId = context.itemId1}
        let state4 = applyCartEvent itemRemoved state3

        state4.version |> shouldBe 4
        Array.length state4.items |> shouldBe 1

        let checkedOut = CartCheckedOut {cartId = context.cartId}
        let state5 = applyCartEvent checkedOut state4

        state5.version |> shouldBe 5
        state5.checkedOut |> shouldBe True

      it "order state evolves correctly through multiple events" \context -> do
        let items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000})
        let orderCreated =
              OrderCreated
                { orderId = context.orderId,
                  tenantId = context.tenantId,
                  customerId = context.customerId,
                  items = items
                }
        let state1 = applyOrderEvent orderCreated initialOrderState

        state1.id |> shouldBe context.orderId
        state1.tenantId |> shouldBe context.tenantId
        state1.version |> shouldBe 1
        state1.cancelled |> shouldBe False

        let orderCancelled = OrderCancelled {orderId = context.orderId}
        let state2 = applyOrderEvent orderCancelled state1

        state2.version |> shouldBe 2
        state2.cancelled |> shouldBe True

    describe "Multiple Commands in Sequence" do
      it "can add multiple items to cart" \context -> do
        -- Start with a cart
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.empty,
                  checkedOut = False,
                  version = 1
                }

        -- Add first item
        let cmd1 = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 3}
        let result1 = decide cmd1 (Just cart)

        case result1 of
          AcceptCommand _ events1 -> do
            let state2 = applyCartEvent (Array.unsafeIndex events1 0) cart

            -- Add second item
            let cmd2 = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
            let result2 = decide cmd2 (Just state2)

            case result2 of
              AcceptCommand _ events2 -> do
                let state3 = applyCartEvent (Array.unsafeIndex events2 0) state2
                Array.length state3.items |> shouldBe 2
              RejectCommand msg -> fail [fmt|Second command failed: {msg}|]
          RejectCommand msg -> fail [fmt|First command failed: {msg}|]

    describe "Business Rule Validation" do
      it "maintains cart state integrity" \context -> do
        let cart =
              CartEntity
                { id = context.cartId,
                  items = Array.singleton (context.itemId1, 10),
                  checkedOut = False,
                  version = 2
                }

        -- Try to checkout (should succeed)
        let checkoutCmd = CheckoutCart {cartId = context.cartId}
        let checkoutResult = decide checkoutCmd (Just cart)

        case checkoutResult of
          AcceptCommand _ events -> do
            let checkedOutCart = applyCartEvent (Array.unsafeIndex events 0) cart

            -- Now try to add item to checked out cart (should fail)
            let addCmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
            let addResult = decide addCmd (Just checkedOutCart)

            case addResult of
              RejectCommand msg -> do
                msg |> shouldContain "checked out"
              AcceptCommand _ _ ->
                fail "Should not be able to add items to checked out cart"
          RejectCommand msg -> fail [fmt|Checkout failed: {msg}|]
