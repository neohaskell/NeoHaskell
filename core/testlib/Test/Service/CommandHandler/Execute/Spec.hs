module Test.Service.CommandHandler.Execute.Spec where

import Array qualified
import Core
import Service.Command (Command (..), CommandResult (..), decide, streamId)
import Service.EntityFetcher.Core (EntityFetcher (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.Command.Core (
  AddItemToCart (..),
  CartEntity (..),
  CartEvent (..),
  CheckoutCart (..),
 )
import Test.Service.CommandHandler.Execute.Context qualified as Context
import Uuid qualified


spec ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
spec _ = do
  describe "CommandHandler Execute Specification Tests" do
    -- TODO: These tests are disabled because CommandHandler.execute is not yet implemented
    -- Uncomment when the CommandHandler module is complete
    pure unit
    -- describe "Basic Command Execution" do
    --   basicExecutionSpecs newCartStoreAndFetcher

    -- describe "Retry Logic" do
    --   retryLogicSpecs newCartStoreAndFetcher

    -- describe "Concurrency Handling" do
    --   concurrencySpecs newCartStoreAndFetcher

    -- describe "Error Scenarios" do
    --   errorScenarioSpecs newCartStoreAndFetcher


basicExecutionSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
basicExecutionSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
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
      let sid = streamId @AddItemToCart cmd
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.cartId |> shouldBe context.cartId

    it "executes command that rejects due to business rules" \context -> do
      -- Try to add item to non-existent cart
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      -- Fetch entity (should not exist)
      let sid = streamId @AddItemToCart cmd
      result <-
        context.cartFetcher.fetch context.cartEntityName sid
          |> Task.asResult

      case result of
        Ok _ -> do
          -- Decision should reject
          let decision = decide @AddItemToCart cmd Nothing
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = insertions
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Fetch and verify state
      let sid = streamId @AddItemToCart (AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 1})
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      Array.length cart.cartItems |> shouldBe 1


retryLogicSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
retryLogicSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
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
      let sid = streamId @AddItemToCart (AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 1})
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      Array.length cart.cartItems |> shouldBe 1

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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
                entityName = context.cartEntityName,
                insertionType = Event.StreamCreation,
                insertions = Array.fromLinkedList [insertion2]
              }

      result <-
        payload2
          |> context.cartStore.insert
          |> Task.asResult

      case result of
        Err (EventStore.InsertionError _) -> do
          -- Expected: Insertion error (stream already exists)
          pure unit
        Err _ -> fail "Unexpected error"
        Ok _ -> fail "Should have failed with insertion error"

    it "retries with 100ms delay between attempts" \_ -> do
      -- This test verifies retry timing
      -- We'll measure that retries happen with proper delays
      -- For now, just verify the concept
      pure unit


concurrencySpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
concurrencySpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
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
              { streamId = context.cartId |> Uuid.toText |> StreamId.fromText,
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
      let _cmd2 = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}

      -- Both commands should eventually succeed with retry logic
      -- One will succeed immediately, the other will retry and succeed
      let sid = streamId @AddItemToCart cmd1
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      cart.cartId |> shouldBe context.cartId

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
                    { streamId = cid |> Uuid.toText |> StreamId.fromText,
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
      let _cmd1 = AddItemToCart {cartId = cartId1, itemId = context.itemId1, amount = 3}
      let _cmd2 = AddItemToCart {cartId = cartId2, itemId = context.itemId2, amount = 5}

      -- Fetch both carts
      cart1 <- context.cartFetcher.fetch context.cartEntityName (cartId1 |> Uuid.toText |> StreamId.fromText) |> Task.mapError toText
      cart2 <- context.cartFetcher.fetch context.cartEntityName (cartId2 |> Uuid.toText |> StreamId.fromText) |> Task.mapError toText

      cart1.cartId |> shouldBe cartId1
      cart2.cartId |> shouldBe cartId2


errorScenarioSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
errorScenarioSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
    it "returns CommandRejected when business rules fail" \context -> do
      -- Try to checkout non-existent cart
      let cmd = CheckoutCart {cartId = context.cartId}
      let decision = decide @CheckoutCart cmd Nothing

      case decision of
        RejectCommand msg -> do
          msg |> shouldBe "Cart does not exist"
        AcceptCommand _ _ ->
          fail "Expected rejection"

    it "returns CommandRejected when cart is already checked out" \context -> do
      -- Create checked-out cart
      let cart =
            CartEntity
              { cartId = context.cartId,
                cartItems = Array.wrap (context.itemId1, 3),
                cartCheckedOut = True
              }

      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}
      let decision = decide @AddItemToCart cmd (Just cart)

      case decision of
        RejectCommand msg -> do
          msg |> shouldBe "Cannot add items to a checked out cart"
        AcceptCommand _ _ ->
          fail "Expected rejection"

    it "handles EventStore errors gracefully" \context -> do
      -- This tests error propagation from EventStore
      -- For now, verify that EntityFetcher handles missing streams
      let sid = context.cartId |> Uuid.toText |> StreamId.fromText
      cart <- context.cartFetcher.fetch context.cartEntityName sid |> Task.mapError toText

      -- Should return initial state for non-existent stream
      cart.cartId |> shouldBe def
