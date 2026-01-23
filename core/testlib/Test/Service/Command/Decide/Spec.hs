module Test.Service.Command.Decide.Spec where

import Array qualified
import Auth.Claims (UserClaims (..))
import Core
import Data.Time.Calendar qualified as GhcCalendar
import Data.Time.Clock qualified as GhcClock
import Map qualified
import Service.Auth (RequestContext (..))
import Service.Auth qualified as Auth
import Service.Command.Core (DecisionContext (..), runDecision)
import Test
import Test.Service.Command.Core (
  AddItemToCart (..),
  AuthenticatedAddItem (..),
  CartEntity (..),
  CheckoutCart (..),
  OwnedCartCheckout (..),
  OwnedCartEntity (..),
  OwnedCartEvent (..),
  RemoveItemFromCart (..),
  applyCartEvent,
  initialCartState,
 )
import Test.Service.EventStore.Core (CartEvent (..))
import Test.Service.Command.Decide.Context qualified as Context
import Uuid qualified


-- Helper function to run a decision in tests
runTestDecision :: (HasCallStack) => Decision event -> Task Text (CommandResult event)
runTestDecision decision = do
  let decisionCtx = DecisionContext {genUuid = Uuid.generate}
  runDecision decisionCtx decision


spec :: Spec Unit
spec = do
  describe "Command decideImpl Specification Tests" do
    describe "Regular Commands (Cart)" do
      cartCommandSpecs

    describe "Authorization Commands" do
      authorizationSpecs

    describe "Edge Cases" do
      edgeCaseSpecs


cartCommandSpecs :: Spec Unit
cartCommandSpecs = do
  describe "AddItemToCart" do
    before Context.initialize do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        result <- runTestDecision (decideImpl @AddItemToCart cmd Nothing Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when cart exists and is not checked out" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = False
                }
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        result <- runTestDecision (decideImpl @AddItemToCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand _ ->
            fail "Expected acceptance but got rejection"
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.get 0 events of
              Just (ItemAdded {itemId, amount}) -> do
                itemId |> shouldBe context.itemId1
                amount |> shouldBe 5
              _ -> fail "Expected ItemAdded event"

      it "rejects when cart is checked out" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 3),
                  cartCheckedOut = True
                }
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 2}
        result <- runTestDecision (decideImpl @AddItemToCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot add items to a checked out cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "generates correct StreamId" \context -> do
        let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}
        let sid = getEntityIdImpl @AddItemToCart cmd

        -- StreamId should wrap the cartId from the command
        sid |> shouldBe (Just context.cartId)

        -- Also verify it would fail for a different cartId
        let differentId = context.itemId1 -- Using itemId as a different Uuid
        sid |> shouldNotBe (Just differentId)
  describe "RemoveItemFromCart" do
    before Context.initialize do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        result <- runTestDecision (decideImpl @RemoveItemFromCart cmd Nothing Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "rejects when item not in cart" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 3),
                  cartCheckedOut = False
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId2}
        result <- runTestDecision (decideImpl @RemoveItemFromCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Item not in cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when item exists in cart" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = [(context.itemId1, 3), (context.itemId2, 5)],
                  cartCheckedOut = False
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        result <- runTestDecision (decideImpl @RemoveItemFromCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand _ ->
            fail "Expected acceptance but got rejection"
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.get 0 events of
              Just (ItemRemoved {itemId}) -> do
                itemId |> shouldBe context.itemId1
              _ -> fail "Expected ItemRemoved event"

      it "rejects when cart is checked out" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 3),
                  cartCheckedOut = True
                }
        let cmd = RemoveItemFromCart {cartId = context.cartId, itemId = context.itemId1}
        result <- runTestDecision (decideImpl @RemoveItemFromCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot remove items from a checked out cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

  describe "CheckoutCart" do
    before Context.initialize do
      it "rejects when cart doesn't exist" \context -> do
        let cmd = CheckoutCart {cartId = context.cartId}
        result <- runTestDecision (decideImpl @CheckoutCart cmd Nothing Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart does not exist"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "rejects when cart is empty" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = False
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        result <- runTestDecision (decideImpl @CheckoutCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot checkout empty cart"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"

      it "accepts when cart has items and not checked out" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 3),
                  cartCheckedOut = False
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        result <- runTestDecision (decideImpl @CheckoutCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand _ ->
            fail "Expected acceptance but got rejection"
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.get 0 events of
              Just (CartCheckedOut {}) -> do
                pure unit
              _ -> fail "Expected CartCheckedOut event"

      it "rejects when cart already checked out" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 3),
                  cartCheckedOut = True
                }
        let cmd = CheckoutCart {cartId = context.cartId}
        result <- runTestDecision (decideImpl @CheckoutCart cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cart already checked out"
          AcceptCommand _ _ ->
            fail "Expected rejection but got acceptance"


edgeCaseSpecs :: Spec Unit
edgeCaseSpecs = do
  describe "Event Application" do
    before Context.initialize do
      it "cart state evolves correctly through multiple events" \context -> do
        let cartCreated = CartCreated {entityId = context.cartId}
        let state1 = applyCartEvent cartCreated initialCartState

        state1.cartId |> shouldBe context.cartId
        Array.length state1.cartItems |> shouldBe 0

        let itemAdded1 = ItemAdded {entityId = context.cartId, itemId = context.itemId1, amount = 3}
        let state2 = applyCartEvent itemAdded1 state1

        Array.length state2.cartItems |> shouldBe 1

        let itemAdded2 = ItemAdded {entityId = context.cartId, itemId = context.itemId2, amount = 5}
        let state3 = applyCartEvent itemAdded2 state2

        Array.length state3.cartItems |> shouldBe 2

        let itemRemoved = ItemRemoved {entityId = context.cartId, itemId = context.itemId1}
        let state4 = applyCartEvent itemRemoved state3

        Array.length state4.cartItems |> shouldBe 1

        let checkedOut = CartCheckedOut {entityId = context.cartId}
        let state5 = applyCartEvent checkedOut state4

        state5.cartCheckedOut |> shouldBe True

  describe "Multiple Commands in Sequence" do
    before Context.initialize do
      it "can add multiple items to cart" \context -> do
        -- Start with a cart
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = False
                }

        -- Add first item
        let cmd1 = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 3}
        result1 <- runTestDecision (decideImpl @AddItemToCart cmd1 (Just cart) Auth.emptyContext)

        case result1 of
          AcceptCommand _ events1 -> do
            case Array.get 0 events1 of
              Just event1 -> do
                let state2 = applyCartEvent event1 cart

                -- Add second item
                let cmd2 = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
                result2 <- runTestDecision (decideImpl @AddItemToCart cmd2 (Just state2) Auth.emptyContext)

                case result2 of
                  AcceptCommand _ events2 -> do
                    case Array.get 0 events2 of
                      Just event2 -> do
                        let state3 = applyCartEvent event2 state2
                        Array.length state3.cartItems |> shouldBe 2
                      Nothing -> fail "Expected event in events2"
                  RejectCommand _ -> fail "Second command failed"
              Nothing -> fail "Expected event in events1"
          RejectCommand _ -> fail "First command failed"

  describe "Business Rule Validation" do
    before Context.initialize do
      it "maintains cart state integrity" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.wrap (context.itemId1, 10),
                  cartCheckedOut = False
                }

        -- Try to checkout (should succeed)
        let checkoutCmd = CheckoutCart {cartId = context.cartId}
        checkoutResult <- runTestDecision (decideImpl @CheckoutCart checkoutCmd (Just cart) Auth.emptyContext)

        case checkoutResult of
          AcceptCommand _ events -> do
            case Array.get 0 events of
              Just event -> do
                let checkedOutCart = applyCartEvent event cart

                -- Now try to add item to checked out cart (should fail)
                let addCmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
                addResult <- runTestDecision (decideImpl @AddItemToCart addCmd (Just checkedOutCart) Auth.emptyContext)

                case addResult of
                  RejectCommand _ -> do
                    pure unit -- Just verify it rejects
                  AcceptCommand _ _ ->
                    fail "Should not be able to add items to checked out cart"
              Nothing -> fail "Expected checkout event"
          RejectCommand _ -> fail "Checkout failed"


-- ============================================================================
-- Authorization Tests
-- ============================================================================


-- | Helper to create a test user with given sub (user ID)
testUser :: Text -> UserClaims
testUser userId =
  UserClaims
    { sub = userId,
      email = Nothing,
      name = Nothing,
      permissions = Array.empty,
      tenantId = Nothing,
      rawClaims = Map.empty
    }


-- | Helper to create an authenticated RequestContext
authenticatedContext :: Text -> RequestContext
authenticatedContext userId =
  RequestContext
    { user = Just (testUser userId)
    , files = Map.empty
    , requestId = Uuid.nil
    , timestamp = GhcClock.UTCTime (GhcCalendar.fromGregorian 1970 1 1) 0
    }


authorizationSpecs :: Spec Unit
authorizationSpecs = do
  describe "AuthenticatedAddItem (requires authentication)" do
    before Context.initialize do
      it "rejects when user is not authenticated" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = False
                }
        let cmd = AuthenticatedAddItem {cartId = context.cartId, itemId = context.itemId1, amount = 5}

        -- Use emptyContext (no user)
        result <- runTestDecision (decideImpl @AuthenticatedAddItem cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Authentication required"
          AcceptCommand _ _ ->
            fail "Expected rejection for unauthenticated user"

      it "accepts when user is authenticated" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = False
                }
        let cmd = AuthenticatedAddItem {cartId = context.cartId, itemId = context.itemId1, amount = 5}

        -- Use authenticated context
        let ctx = authenticatedContext "user-123"
        result <- runTestDecision (decideImpl @AuthenticatedAddItem cmd (Just cart) ctx)

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: #{msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

      it "still enforces business rules when authenticated" \context -> do
        let cart =
              CartEntity
                { cartId = context.cartId,
                  cartItems = Array.empty,
                  cartCheckedOut = True -- Cart is checked out
                }
        let cmd = AuthenticatedAddItem {cartId = context.cartId, itemId = context.itemId1, amount = 5}

        -- Authenticated but cart is checked out
        let ctx = authenticatedContext "user-123"
        result <- runTestDecision (decideImpl @AuthenticatedAddItem cmd (Just cart) ctx)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot add items to a checked out cart"
          AcceptCommand _ _ ->
            fail "Expected rejection for checked out cart"

  describe "OwnedCartCheckout (requires ownership)" do
    before Context.initialize do
      it "rejects when user is not authenticated" \context -> do
        let cart =
              OwnedCartEntity
                { cartId = context.cartId,
                  ownerId = "owner-user",
                  cartItems = Array.wrap (context.itemId1, 5),
                  cartCheckedOut = False
                }
        let cmd = OwnedCartCheckout {cartId = context.cartId}

        -- Use emptyContext (no user)
        result <- runTestDecision (decideImpl @OwnedCartCheckout cmd (Just cart) Auth.emptyContext)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Authentication required"
          AcceptCommand _ _ ->
            fail "Expected rejection for unauthenticated user"

      it "rejects when user does not own the cart" \context -> do
        let cart =
              OwnedCartEntity
                { cartId = context.cartId,
                  ownerId = "owner-user",
                  cartItems = Array.wrap (context.itemId1, 5),
                  cartCheckedOut = False
                }
        let cmd = OwnedCartCheckout {cartId = context.cartId}

        -- User is authenticated but not the owner
        let ctx = authenticatedContext "different-user"
        result <- runTestDecision (decideImpl @OwnedCartCheckout cmd (Just cart) ctx)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "You do not own this cart"
          AcceptCommand _ _ ->
            fail "Expected rejection for non-owner"

      it "accepts when user owns the cart" \context -> do
        let cart =
              OwnedCartEntity
                { cartId = context.cartId,
                  ownerId = "owner-user",
                  cartItems = Array.wrap (context.itemId1, 5),
                  cartCheckedOut = False
                }
        let cmd = OwnedCartCheckout {cartId = context.cartId}

        -- User is the owner
        let ctx = authenticatedContext "owner-user"
        result <- runTestDecision (decideImpl @OwnedCartCheckout cmd (Just cart) ctx)

        case result of
          RejectCommand msg ->
            fail [fmt|Expected acceptance but got rejection: #{msg}|]
          AcceptCommand insertionType events -> do
            insertionType |> shouldBe ExistingStream
            Array.length events |> shouldBe 1

            case Array.get 0 events of
              Just (OwnedCartCheckedOut {}) -> do
                pure unit
              _ -> fail "Expected OwnedCartCheckedOut event"

      it "still enforces business rules when owner" \context -> do
        let cart =
              OwnedCartEntity
                { cartId = context.cartId,
                  ownerId = "owner-user",
                  cartItems = Array.empty, -- Empty cart
                  cartCheckedOut = False
                }
        let cmd = OwnedCartCheckout {cartId = context.cartId}

        -- User is the owner but cart is empty
        let ctx = authenticatedContext "owner-user"
        result <- runTestDecision (decideImpl @OwnedCartCheckout cmd (Just cart) ctx)

        case result of
          RejectCommand msg -> do
            msg |> shouldBe "Cannot checkout empty cart"
          AcceptCommand _ _ ->
            fail "Expected rejection for empty cart"
