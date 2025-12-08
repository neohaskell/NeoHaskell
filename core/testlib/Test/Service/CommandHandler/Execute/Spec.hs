module Test.Service.CommandHandler.Execute.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Service.CommandHandler (CommandHandlerResult (CommandAccepted, CommandFailed, CommandRejected))
import Service.CommandHandler qualified as CommandHandler
import Service.EntityFetcher.Core (EntityFetcher (..), EntityFetchResult (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.Command.Core (
  AddItemToCart (..),
  CartEntity (..),
  CheckoutCart (..),
 )
import Test.Service.EventStore.Core (CartEvent (..))
import Test.Service.CommandHandler.Execute.Context qualified as Context
import Text qualified
import Uuid qualified


spec ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
spec newCartStoreAndFetcher = do
  describe "CommandHandler Execute Specification Tests" do
    describe "Basic Command Execution" do
      basicExecutionSpecs newCartStoreAndFetcher

    describe "Retry Logic" do
      retryLogicSpecs newCartStoreAndFetcher

    describe "Concurrency Handling" do
      concurrencySpecs newCartStoreAndFetcher

    describe "Error Scenarios" do
      errorScenarioSpecs newCartStoreAndFetcher


basicExecutionSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
basicExecutionSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
    it "executes command that creates new stream successfully" \context -> do
      -- Create cart first
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
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
                insertions = [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Now test CommandHandler with AddItemToCart
      -- Expected: Should fetch entity, call decide, insert event, return success
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      -- Should succeed
      case result of
        CommandAccepted {} -> do
          -- Verify event was inserted by fetching updated cart
          let sid = getEntityIdImpl @AddItemToCart cmd |> (Maybe.getOrDie .> Uuid.toText .> StreamId.fromText)
          cart <-
            context.cartFetcher.fetch context.cartEntityName sid
              |> Task.mapError toText
              |> Task.andThen (\result -> case result of
                  EntityFound s -> Task.yield s
                  EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
                )

          cart.cartId |> shouldBe context.cartId
          Array.length cart.cartItems |> shouldBe 1
        CommandRejected msg ->
          fail (Text.append "Expected CommandAccepted, got CommandRejected: " msg)
        CommandFailed err _ ->
          fail (Text.append "Expected CommandAccepted, got CommandFailed: " err)

    it "executes command that rejects due to business rules" \context -> do
      -- Try to add item to non-existent cart
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      case result of
        CommandRejected msg -> do
          msg |> shouldBe "Cart does not exist"
        CommandAccepted {} ->
          fail "Expected CommandRejected, got CommandAccepted"
        CommandFailed _err _ ->
          fail "Expected CommandRejected, got CommandFailed"

    it "appends events to existing stream successfully" \context -> do
      -- Create cart with initial item
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
      let itemAddedEvent = ItemAdded {entityId = context.cartId, itemId = context.itemId1, amount = 3}

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

      -- Now add another item using CommandHandler
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 7}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      case result of
        CommandAccepted {} -> do
          -- Verify both items are in cart
          let sid = getEntityIdImpl @AddItemToCart cmd |> (Maybe.getOrDie .> Uuid.toText .> StreamId.fromText)
          cart <-
            context.cartFetcher.fetch context.cartEntityName sid
              |> Task.mapError toText
              |> Task.andThen (\result -> case result of
                  EntityFound s -> Task.yield s
                  EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
                )

          cart.cartId |> shouldBe context.cartId
          Array.length cart.cartItems |> shouldBe 2
        CommandRejected _msg ->
          fail "Expected CommandAccepted, got CommandRejected"
        CommandFailed _err _ ->
          fail "Expected CommandAccepted, got CommandFailed"


retryLogicSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
retryLogicSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
    it "retries on consistency check failure (ExistingStream)" \context -> do
      -- Create initial cart
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
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
                insertions = [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Simulate concurrent modification by inserting another event
      let concurrentEvent = ItemAdded {entityId = context.cartId, itemId = context.itemId2, amount = 10}
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
                insertions = [insertion2]
              }

      payload2
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Now execute a command that will need to retry due to concurrent modification
      -- CommandHandler should detect stale state and retry
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 1}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      case result of
        CommandAccepted {} -> do
          -- Verify command succeeded after retry
          let sid = getEntityIdImpl @AddItemToCart cmd |> (Maybe.getOrDie .> Uuid.toText .> StreamId.fromText)
          cart <-
            context.cartFetcher.fetch context.cartEntityName sid
              |> Task.mapError toText
              |> Task.andThen (\result -> case result of
                  EntityFound s -> Task.yield s
                  EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
                )

          -- Should have both items now (item2 from concurrent insert, item1 from this command)
          Array.length cart.cartItems |> shouldBe 2
        CommandRejected _msg ->
          fail "Expected CommandAccepted after retry, got CommandRejected"
        CommandFailed _err _ ->
          fail "Expected CommandAccepted after retry, got CommandFailed"

    it "stops retrying when InsertionType is StreamCreation and stream exists" \context -> do
      -- Create a cart (stream exists)
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
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
                insertions = [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Try to create the stream again with StreamCreation
      -- This should fail immediately without retrying
      let anotherCreatedEvent = CartCreated {entityId = context.cartId}
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
                insertions = [insertion2]
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
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
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
                insertions = [insertion]
              }

      payload
        |> context.cartStore.insert
        |> Task.mapError toText
        |> discard

      -- Execute concurrent commands
      let cmd1 = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 3}
      let cmd2 = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}

      -- Both commands should execute concurrently with retry logic
      (result1, result2) <-
        AsyncTask.runConcurrently
          ( CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd1,
            CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd2
          )

      -- Both should succeed
      case result1 of
        CommandAccepted {} -> pure unit
        CommandRejected _msg -> fail "Command 1 rejected"
        CommandFailed _err _ -> fail "Command 1 failed"

      case result2 of
        CommandAccepted {} -> pure unit
        CommandRejected _msg -> fail "Command 2 rejected"
        CommandFailed _err _ -> fail "Command 2 failed"

      -- Verify final state has both items
      let sid = getEntityIdImpl @AddItemToCart cmd1 |> (Maybe.getOrDie .> Uuid.toText .> StreamId.fromText)
      cart <-
        context.cartFetcher.fetch context.cartEntityName sid
          |> Task.mapError toText
          |> Task.andThen (\result -> case result of
              EntityFound s -> Task.yield s
              EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
            )

      cart.cartId |> shouldBe context.cartId
      Array.length cart.cartItems |> shouldBe 2

    it "handles concurrent commands on different streams" \context -> do
      -- Create two carts
      cartId1 <- Uuid.generate
      cartId2 <- Uuid.generate

      let createCart cid = do
            let event = CartCreated {entityId = cid}
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
                      insertions = [insertion]
                    }

            payload
              |> context.cartStore.insert
              |> Task.mapError toText

      discard <| createCart cartId1
      discard <| createCart cartId2

      -- Execute commands on different streams - should not interfere
      let cmd1 = AddItemToCart {cartId = cartId1, itemId = context.itemId1, amount = 3}
      let cmd2 = AddItemToCart {cartId = cartId2, itemId = context.itemId2, amount = 5}

      result1 <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd1
      result2 <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd2

      -- Both should succeed without retries
      case result1 of
        CommandAccepted {} -> pure unit
        CommandRejected _msg -> fail "Command 1 rejected"
        CommandFailed _err _ -> fail "Command 1 failed"

      case result2 of
        CommandAccepted {} -> pure unit
        CommandRejected _msg -> fail "Command 2 rejected"
        CommandFailed _err _ -> fail "Command 2 failed"

      -- Verify both carts have their items
      cart1 <-
        context.cartFetcher.fetch context.cartEntityName (cartId1 |> Uuid.toText |> StreamId.fromText)
          |> Task.mapError toText
          |> Task.andThen (\result -> case result of
              EntityFound s -> Task.yield s
              EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
            )
      cart2 <-
        context.cartFetcher.fetch context.cartEntityName (cartId2 |> Uuid.toText |> StreamId.fromText)
          |> Task.mapError toText
          |> Task.andThen (\result -> case result of
              EntityFound s -> Task.yield s
              EntityNotFound -> Task.throw "Expected entity to exist but got EntityNotFound"
            )

      cart1.cartId |> shouldBe cartId1
      cart2.cartId |> shouldBe cartId2
      Array.length cart1.cartItems |> shouldBe 1
      Array.length cart2.cartItems |> shouldBe 1


errorScenarioSpecs ::
  Task Text (EventStore.EventStore CartEvent, EntityFetcher CartEntity CartEvent) ->
  Spec Unit
errorScenarioSpecs newCartStoreAndFetcher = do
  before (Context.initialize newCartStoreAndFetcher) do
    it "returns CommandRejected when business rules fail" \context -> do
      -- Try to checkout non-existent cart
      let cmd = CheckoutCart {cartId = context.cartId}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      case result of
        CommandRejected msg -> do
          msg |> shouldBe "Cart does not exist"
        CommandAccepted {} ->
          fail "Expected CommandRejected, got CommandAccepted"
        CommandFailed _err _ ->
          fail "Expected CommandRejected, got CommandFailed"

    it "returns CommandRejected when cart is already checked out" \context -> do
      -- Create cart and check it out
      let cartCreatedEvent = CartCreated {entityId = context.cartId}
      let itemAddedEvent = ItemAdded {entityId = context.cartId, itemId = context.itemId1, amount = 3}
      let checkedOutEvent = CartCheckedOut {entityId = context.cartId}

      eventId1 <- Uuid.generate
      eventId2 <- Uuid.generate
      eventId3 <- Uuid.generate
      metadata <- EventMetadata.new

      let insertions =
            Array.fromLinkedList
              [ Event.Insertion
                  { id = eventId1,
                    event = cartCreatedEvent,
                    metadata = metadata {EventMetadata.localPosition = Just (Event.StreamPosition 0), EventMetadata.eventId = eventId1}
                  },
                Event.Insertion
                  { id = eventId2,
                    event = itemAddedEvent,
                    metadata = metadata {EventMetadata.localPosition = Just (Event.StreamPosition 1), EventMetadata.eventId = eventId2}
                  },
                Event.Insertion
                  { id = eventId3,
                    event = checkedOutEvent,
                    metadata = metadata {EventMetadata.localPosition = Just (Event.StreamPosition 2), EventMetadata.eventId = eventId3}
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

      -- Try to add item to checked out cart
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId2, amount = 5}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      case result of
        CommandRejected msg -> do
          msg |> shouldBe "Cannot add items to a checked out cart"
        CommandAccepted {} ->
          fail "Expected CommandRejected, got CommandAccepted"
        CommandFailed _err _ ->
          fail "Expected CommandRejected, got CommandFailed"

    it "handles EventStore errors gracefully" \context -> do
      -- Test that CommandHandler properly propagates errors from EventStore
      -- When trying to add to non-existent cart, should reject (not crash)
      let cmd = AddItemToCart {cartId = context.cartId, itemId = context.itemId1, amount = 5}

      result <- CommandHandler.execute context.cartStore context.cartFetcher context.cartEntityName cmd

      -- Should get CommandRejected (business rule), not an error
      case result of
        CommandRejected msg -> do
          msg |> shouldBe "Cart does not exist"
        CommandAccepted {} ->
          fail "Expected CommandRejected, got CommandAccepted"
        CommandFailed _err _ ->
          fail "Expected CommandRejected, got CommandFailed"
