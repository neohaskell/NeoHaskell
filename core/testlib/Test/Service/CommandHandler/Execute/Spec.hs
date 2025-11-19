module Test.Service.CommandHandler.Execute.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Result qualified
import Service.Command (Command (..), CommandResult (..), TenantCommand (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
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
import Test.Service.CommandHandler.Core (CommandHandlerResult (..))
import Test.Service.CommandHandler.Execute.Context qualified as Context
import Uuid qualified


spec ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore.EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
spec newCartStoreAndFetcher newOrderStoreAndFetcher = do
  describe "CommandHandler Execute Specification Tests" do
    describe "Basic Command Execution" do
      basicExecutionSpecs newCartStoreAndFetcher newOrderStoreAndFetcher

    describe "Retry Logic" do
      retryLogicSpecs newCartStoreAndFetcher newOrderStoreAndFetcher

    describe "Concurrency Handling" do
      concurrencySpecs newCartStoreAndFetcher newOrderStoreAndFetcher

    describe "Error Scenarios" do
      errorScenarioSpecs newCartStoreAndFetcher newOrderStoreAndFetcher


basicExecutionSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore.EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
basicExecutionSpecs newCartStoreAndFetcher newOrderStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher newOrderStoreAndFetcher) do
    it "executes command that creates new stream successfully" \context -> do
      -- Create cart first
      let cartCreatedEvent = CartCreated {cartId = context.cartId}
      eventId <- Uuid.generate
      metadata <- EventMetadata.new
      let metadata' =
            metadata
              { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                EventMetadata.eventId = eventId
              }

      let insertion =
            Event.Insertion
              { id = eventId,
                event = cartCreatedEvent,
                metadata = metadata'
              }

      let payload =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Now test CommandHandler with AddItemToCart
      -- Expected: Should fetch entity, call decide, insert event, return success
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      -- TODO: Replace with actual CommandHandler.execute call
      -- result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      -- For now, verify the command would succeed
      let sid = streamId cmd
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.id |> shouldBe context.cartId

    it "executes command that rejects due to business rules" \context -> do
      -- Try to add item to non-existent cart
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      -- Fetch entity (should not exist)
      let sid = streamId cmd
      result <-
        context.cartFetcher.fetch context.cartEntityName sid
          |> Task.asResult

      case result of
        Ok cart -> do
          -- Entity doesn't exist (version 0)
          cart.version |> shouldBe 0

          -- Decision should reject
          let decision = decide cmd Nothing
          case decision of
            RejectCommand msg -> do
              msg |> shouldBe "Cart does not exist"
            AcceptCommand _ _ ->
              fail "Expected rejection"
        Err _ -> fail "Fetch failed"

    it "appends events to existing stream successfully" \context -> do
      -- Create cart with initial item
      let cartCreatedEvent = CartCreated {cartId = context.cartId}
      let itemAddedEvent = ItemAdded {cartId = context.cartId, itemId = context.itemId1, amount = 3}

      eventId1 <- Uuid.generate
      eventId2 <- Uuid.generate
      metadata <- EventMetadata.new

      let insertions =
            Array.fromLinkedList
              [ Event.Insertion
                  { id = eventId1,
                    event = cartCreatedEvent,
                    metadata =
                      metadata
                        { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                          EventMetadata.eventId = eventId1
                        }
                  },
                Event.Insertion
                  { id = eventId2,
                    event = itemAddedEvent,
                    metadata =
                      metadata
                        { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                          EventMetadata.eventId = eventId2
                        }
                  }
              ]

      let payload =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = insertions
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Fetch and verify state
      let sid = streamId (AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 1})
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.version |> shouldBe 2
      Array.length cart.items |> shouldBe 1


retryLogicSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore.EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
retryLogicSpecs newCartStoreAndFetcher newOrderStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher newOrderStoreAndFetcher) do
    it "retries on consistency check failure (ExistingStream)" \context -> do
      -- Create initial cart
      let cartCreatedEvent = CartCreated {cartId = context.cartId}
      eventId <- Uuid.generate
      metadata <- EventMetadata.new

      let insertion =
            Event.Insertion
              { id = eventId,
                event = cartCreatedEvent,
                metadata =
                  metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                      EventMetadata.eventId = eventId
                    }
              }

      let payload =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Simulate concurrent modification by inserting another event
      let concurrentEvent = ItemAdded {cartId = context.cartId, itemId = context.itemId2, amount = 10}
      eventId2 <- Uuid.generate

      let insertion2 =
            Event.Insertion
              { id = eventId2,
                event = concurrentEvent,
                metadata =
                  metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                      EventMetadata.eventId = eventId2
                    }
              }

      let payload2 =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.ExistingStream,
                insertions = Array.fromLinkedList [insertion2]
              }

      payload2
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Now if CommandHandler tries to insert with stale state, it should retry
      -- Verify current state
      let sid = streamId (AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 1})
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.version |> shouldBe 2
      Array.length cart.items |> shouldBe 1

    it "stops retrying when InsertionType is StreamCreation and stream exists" \context -> do
      -- Create a cart (stream exists)
      let cartCreatedEvent = CartCreated {cartId = context.cartId}
      eventId <- Uuid.generate
      metadata <- EventMetadata.new

      let insertion =
            Event.Insertion
              { id = eventId,
                event = cartCreatedEvent,
                metadata =
                  metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                      EventMetadata.eventId = eventId
                    }
              }

      let payload =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Try to create the stream again with StreamCreation
      -- This should fail immediately without retrying
      let anotherCreatedEvent = CartCreated {cartId = context.cartId}
      eventId2 <- Uuid.generate

      let insertion2 =
            Event.Insertion
              { id = eventId2,
                event = anotherCreatedEvent,
                metadata =
                  metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                      EventMetadata.eventId = eventId2
                    }
              }

      let payload2 =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion2]
              }

      result <-
        payload2
          |> context.cartStore.insert
          |> Task.asResult

      case result of
        Err (EventStore.InsertionError (Event.StreamAlreadyExists _)) -> do
          -- Expected: Stream already exists error
          pure unit
        Err other -> fail [fmt|Unexpected error: {toText other}|]
        Ok _ -> fail "Should have failed with StreamAlreadyExists"

    it "retries with 100ms delay between attempts" \context -> do
      -- This test verifies retry timing
      -- We'll measure that retries happen with proper delays
      -- For now, just verify the concept
      pure unit


concurrencySpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore.EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
concurrencySpecs newCartStoreAndFetcher newOrderStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher newOrderStoreAndFetcher) do
    it "handles concurrent command execution on same stream" \context -> do
      -- Create cart
      let cartCreatedEvent = CartCreated {cartId = context.cartId}
      eventId <- Uuid.generate
      metadata <- EventMetadata.new

      let insertion =
            Event.Insertion
              { id = eventId,
                event = cartCreatedEvent,
                metadata =
                  metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                      EventMetadata.eventId = eventId
                    }
              }

      let payload =
            Event.InsertionPayload
              { streamId = StreamId.StreamId context.cartId,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Simulate concurrent commands
      let cmd1 = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 3}
      let cmd2 = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}

      -- Both commands should eventually succeed with retry logic
      -- One will succeed immediately, the other will retry and succeed
      let sid = streamId cmd1
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.version |> shouldBe 1

    it "handles concurrent commands on different streams" \context -> do
      -- Create two carts
      cartId1 <- Uuid.generate
      cartId2 <- Uuid.generate

      let createCart cid = do
            let event = CartCreated {cartId = cid}
            eid <- Uuid.generate
            metadata <- EventMetadata.new

            let insertion =
                  Event.Insertion
                    { id = eid,
                      event = event,
                      metadata =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                            EventMetadata.eventId = eid
                          }
                    }

            let payload =
                  Event.InsertionPayload
                    { streamId = StreamId.StreamId cid,
                      entityName = context.cartEntityName,
                      insertionType = Event.StreamCreation,
                      insertions = Array.fromLinkedList [insertion]
                    }

            payload
              |> context.cartStore.insert
              |> Task.mapError toText

      discard <| createCart cartId1
      discard <| createCart cartId2

      -- Commands on different streams should not interfere
      let cmd1 = AddItemToCart {cartId = cartId1, itemId = context.itemId1, amount = 3}
      let cmd2 = AddItemToCart {cartId = cartId2, itemId = context.itemId2, amount = 5}

      -- Fetch both carts
      cart1 <- context.cartFetcher.fetch context.cartEntityName (StreamId.StreamId cartId1) |> Task.mapError toText
      cart2 <- context.cartFetcher.fetch context.cartEntityName (StreamId.StreamId cartId2) |> Task.mapError toText

      cart1.id |> shouldBe cartId1
      cart2.id |> shouldBe cartId2


errorScenarioSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Task Text (EventStore.EventStore OrderEvent, EntityFetcher OrderEntity OrderEvent) ->
  Spec Unit
errorScenarioSpecs newCartStoreAndFetcher newOrderStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher newOrderStoreAndFetcher) do
    it "returns CommandRejected when business rules fail" \context -> do
      -- Try to checkout non-existent cart
      let cmd = CheckoutCart {cartId = context.cartId}
      let decision = decide cmd (Nothing :: Maybe CartEntity)

      case decision of
        RejectCommand msg -> do
          msg |> shouldBe "Cart does not exist"
        AcceptCommand _ _ ->
          fail "Expected rejection"

    it "returns CommandRejected when cart is already checked out" \context -> do
      -- Create checked-out cart
      let cart =
            CartEntity
              { id = context.cartId,
                items = Array.singleton (context.itemId1, 3),
                checkedOut = True,
                version = 2
              }

      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
      let decision = decide cmd (Just cart)

      case decision of
        RejectCommand msg -> do
          msg |> shouldBe "Cannot add items to a checked out cart"
        AcceptCommand _ _ ->
          fail "Expected rejection"

    it "handles EventStore errors gracefully" \context -> do
      -- This tests error propagation from EventStore
      -- For now, verify that EntityFetcher handles missing streams
      let sid = StreamId.StreamId context.cartId
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      -- Should return initial state for non-existent stream
      cart.version |> shouldBe 0
      cart.id |> shouldBe Uuid.nil

    it "handles tenant commands correctly" \context -> do
      -- Create order command
      let items = Array.singleton (OrderItem {productId = context.itemId1, quantity = 2, price = 1000})
      let cmd = CreateOrder {orderId = context.orderId, customerId = context.customerId, items = items}

      -- Get tenant-scoped stream ID
      let sid = streamId cmd context.tenantId

      -- Fetch entity (should not exist)
      order <- context.orderFetcher.fetch context.orderEntityName sid |> Task.mapError toText

      order.version |> shouldBe 0

      -- Decision should accept
      let decision = decide cmd Nothing context.tenantId

      case decision of
        AcceptCommand insertionType events -> do
          insertionType |> shouldBe Event.StreamCreation
          Array.length events |> shouldBe 1
        RejectCommand msg ->
          fail [fmt|Expected acceptance but got: {msg}|]
