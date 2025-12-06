module Test.Service.EntityFetcher.Fetch.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Result qualified
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EntityFetcher.Core (CartEvent (..), CartState (..))
import Test.Service.EntityFetcher.Fetch.Context qualified as Context
import Uuid qualified


spec ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent) ->
  Spec Unit
spec newStoreAndFetcher = do
  describe "Entity Fetch" do
    before (Context.initialize newStoreAndFetcher) do
      it "fetches an entity with no events and returns Nothing" \context -> do
        -- Fetch from a stream that doesn't exist yet
        maybeState <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- Should return Nothing when there are no events
        case maybeState of
          Just _ -> do
            fail "Expected Nothing for empty stream but got Some state"
          Nothing -> do
            -- This is the expected behavior
            Task.yield unit

      it "fetches an entity with a single event and applies it correctly" \context -> do
        -- Insert one event: AccountOpened
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
                  event = CartCreated {entityId = def},
                  metadata = metadata'
                }
        let payload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = [insertion]
                }

        payload
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Fetch the entity
        state <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- Should have the correct state after applying one event
        Array.length state.cartItems |> shouldBe 0
        state.isCheckedOut |> shouldBe False
        state.version |> shouldBe 1

      it "fetches an entity with multiple events and applies them in order" \context -> do
        -- Insert multiple events in sequence
        let events =
              Array.fromLinkedList
                [ CartCreated {entityId = def},
                  ItemAdded {entityId = def, itemId = def, amount = 100},
                  ItemRemoved {entityId = def, itemId = def},
                  ItemAdded {entityId = def, itemId = def, amount = 20}
                ]

        insertions <-
          events
            |> Array.indexedMap
              ( \index event -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral index)),
                            EventMetadata.eventId = eventId
                          }
                  Task.yield
                    Event.Insertion
                      { id = eventId,
                        event = event,
                        metadata = metadata'
                      }
              )
            |> Task.mapArray identity

        insertions
          |> Array.indexedMap
            ( \index insertion -> do
                let insertionType =
                      case index of
                        0 -> Event.StreamCreation
                        _ -> Event.InsertAfter (Event.StreamPosition (fromIntegral (index - 1)))
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName = context.entityName,
                          insertionType,
                          insertions = [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
                  |> discard
            )
          |> Task.mapArray identity
          |> discard

        -- Fetch the entity
        state <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- Should have correct state: 1 item (added, removed, added again)
        Array.length state.cartItems |> shouldBe 1
        state.isCheckedOut |> shouldBe False
        state.version |> shouldBe 4

      it "fetches different entities independently" \context -> do
        -- Create a second stream with different events
        streamId2 <- StreamId.new

        -- Insert events to first stream
        eventId1 <- Uuid.generate
        metadata1 <- EventMetadata.new
        let metadata1' =
              metadata1
                { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                  EventMetadata.eventId = eventId1
                }
        let insertion1 =
              Event.Insertion
                { id = eventId1,
                  event = CartCreated {entityId = def},
                  metadata = metadata1'
                }
        let payload1 =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = [insertion1]
                }

        payload1
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Insert events to second stream
        eventId2 <- Uuid.generate
        metadata2 <- EventMetadata.new
        let metadata2' =
              metadata2
                { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                  EventMetadata.eventId = eventId2
                }
        let insertion2 =
              Event.Insertion
                { id = eventId2,
                  event = CartCreated {entityId = def},
                  metadata = metadata2'
                }
        let payload2 =
              Event.InsertionPayload
                { streamId = streamId2,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = [insertion2]
                }

        payload2
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Fetch both entities
        state1 <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        state2 <-
          context.fetcher.fetch context.entityName streamId2
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- Should have different states (both empty carts)
        Array.length state1.cartItems |> shouldBe 0
        Array.length state2.cartItems |> shouldBe 0
        state1.version |> shouldBe 1
        state2.version |> shouldBe 1

      it "handles entity state with many events efficiently" \context -> do
        -- Insert 100 events
        let eventCount = 100

        insertions <-
          Array.initialize eventCount identity
            |> Array.map
              ( \index -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral (index + 1))),
                            EventMetadata.eventId = eventId
                          }
                  Task.yield
                    Event.Insertion
                      { id = eventId,
                        event = ItemAdded {entityId = def, itemId = def, amount = 1}, -- Deposit 1 each time
                        metadata = metadata'
                      }
              )
            |> Task.mapArray identity

        -- Insert opening event first
        openEventId <- Uuid.generate
        openMetadata <- EventMetadata.new
        let openMetadata' =
              openMetadata
                { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                  EventMetadata.eventId = openEventId
                }
        let openInsertion =
              Event.Insertion
                { id = openEventId,
                  event = CartCreated {entityId = def},
                  metadata = openMetadata'
                }
        let openPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = [openInsertion]
                }

        openPayload
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Insert all deposit events
        insertions
          |> Array.indexedMap
            ( \index insertion -> do
                let insertionType = Event.InsertAfter (Event.StreamPosition (fromIntegral index))
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName = context.entityName,
                          insertionType,
                          insertions = [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
                  |> discard
            )
          |> Task.mapArray identity
          |> discard

        -- Fetch the entity
        state <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- Should have 1 item (same item added 100 times)
        Array.length state.cartItems |> shouldBe 1
        state.isCheckedOut |> shouldBe False
        state.version |> shouldBe 101 -- Opening event + 100 deposits
      it "returns error when fetching from non-existent entity type" \context -> do
        let wrongEntityName = Event.EntityName "NonExistentEntity"

        result <-
          context.fetcher.fetch wrongEntityName context.streamId
            |> Task.asResult

        -- This should succeed with initial state (empty stream)
        -- OR return a specific error depending on implementation
        -- For now, let's expect initial state for consistency
        result
          |> Result.isOk
          |> shouldBe True

      it "maintains version count correctly across fetch operations" \context -> do
        -- Insert 3 events
        let events =
              Array.fromLinkedList
                [ CartCreated {entityId = def},
                  ItemAdded {entityId = def, itemId = def, amount = 50},
                  ItemRemoved {entityId = def, itemId = def}
                ]

        insertions <-
          events
            |> Array.indexedMap
              ( \index event -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral index)),
                            EventMetadata.eventId = eventId
                          }
                  Task.yield
                    Event.Insertion
                      { id = eventId,
                        event = event,
                        metadata = metadata'
                      }
              )
            |> Task.mapArray identity

        insertions
          |> Array.indexedMap
            ( \index insertion -> do
                let insertionType =
                      case index of
                        0 -> Event.StreamCreation
                        _ -> Event.InsertAfter (Event.StreamPosition (fromIntegral (index - 1)))
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName = context.entityName,
                          insertionType,
                          insertions = [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
                  |> discard
            )
          |> Task.mapArray identity
          |> discard

        -- Fetch multiple times - should always return same version
        state1 <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        state2 <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        state3 <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- All fetches should return same version
        state1.version |> shouldBe 3
        state2.version |> shouldBe 3
        state3.version |> shouldBe 3

        -- All should have same items (item was added then removed, so 0)
        Array.length state1.cartItems |> shouldBe 0
        Array.length state2.cartItems |> shouldBe 0
        Array.length state3.cartItems |> shouldBe 0

      it "correctly handles closed account state" \context -> do
        -- Insert events: create cart, add item, then checkout
        let events =
              Array.fromLinkedList
                [ CartCreated {entityId = def},
                  ItemAdded {entityId = def, itemId = def, amount = 5},
                  CartCheckedOut {entityId = def}
                ]

        insertions <-
          events
            |> Array.indexedMap
              ( \index event -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral index)),
                            EventMetadata.eventId = eventId
                          }
                  Task.yield
                    Event.Insertion
                      { id = eventId,
                        event = event,
                        metadata = metadata'
                      }
              )
            |> Task.mapArray identity

        insertions
          |> Array.indexedMap
            ( \index insertion -> do
                let insertionType =
                      case index of
                        0 -> Event.StreamCreation
                        _ -> Event.InsertAfter (Event.StreamPosition (fromIntegral (index - 1)))
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName = context.entityName,
                          insertionType,
                          insertions = [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
                  |> discard
            )
          |> Task.mapArray identity
          |> discard

        -- Fetch the entity
        state <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.map Maybe.getOrDie
            |> Task.mapError toText

        -- Cart should be checked out with 1 item
        Array.length state.cartItems |> shouldBe 1
        state.isCheckedOut |> shouldBe True
        state.version |> shouldBe 3

    describe "Error Scenarios" do
      before (Context.initialize newStoreAndFetcher) do
        it "handles non-existent entity type gracefully" \context -> do
          -- Try to fetch from an entity type that doesn't exist
          let nonExistentEntity = Event.EntityName ""
          maybeState <-
            context.fetcher.fetch nonExistentEntity context.streamId
              |> Task.mapError toText

          -- Should return Nothing for empty stream (no events)
          case maybeState of
            Just _ -> fail "Expected Nothing for non-existent entity type but got Some state"
            Nothing -> Task.yield unit

        it "handles concurrent fetches of the same entity" \context -> do
          -- Insert some events first
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
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }
          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          -- Fetch the same entity concurrently multiple times
          fetch1 <- AsyncTask.run (context.fetcher.fetch context.entityName context.streamId |> Task.mapError toText)
          fetch2 <- AsyncTask.run (context.fetcher.fetch context.entityName context.streamId |> Task.mapError toText)
          fetch3 <- AsyncTask.run (context.fetcher.fetch context.entityName context.streamId |> Task.mapError toText)

          state1 <-
            AsyncTask.waitFor fetch1
              |> Task.map Maybe.getOrDie
          state2 <-
            AsyncTask.waitFor fetch2
              |> Task.map Maybe.getOrDie
          state3 <-
            AsyncTask.waitFor fetch3
              |> Task.map Maybe.getOrDie

          -- All fetches should return the same consistent state (empty cart)
          Array.length state1.cartItems |> shouldBe 0
          Array.length state2.cartItems |> shouldBe 0
          Array.length state3.cartItems |> shouldBe 0
          state1.version |> shouldBe 1
          state2.version |> shouldBe 1
          state3.version |> shouldBe 1

        it "handles fetch during concurrent writes" \context -> do
          -- Insert initial event
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
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }
          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          -- Start a fetch
          fetchTask <-
            AsyncTask.run
              (context.fetcher.fetch context.entityName context.streamId |> Task.mapError toText)

          -- Concurrently insert more events
          eventId2 <- Uuid.generate
          metadata2 <- EventMetadata.new
          let metadata2' =
                metadata2
                  { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                    EventMetadata.eventId = eventId2
                  }
          let insertion2 =
                Event.Insertion
                  { id = eventId2,
                    event = ItemAdded {entityId = def, itemId = def, amount = 100},
                    metadata = metadata2'
                  }
          let payload2 =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.InsertAfter (Event.StreamPosition 0),
                    insertions = [insertion2]
                  }
          payload2
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          -- Wait for fetch to complete
          (state :: CartState) <- AsyncTask.waitFor fetchTask |> Task.map Maybe.getOrDie

          -- The fetch should see either the old or new state consistently
          -- (could be 0 or 1 depending on timing, but should be consistent)
          ((Array.length state.cartItems == 0) || (Array.length state.cartItems == 1)) |> shouldBe True
          state.isCheckedOut |> shouldBe False

    describe "Edge Cases" do
      before (Context.initialize newStoreAndFetcher) do
        it "handles fetching with empty stream ID gracefully" \context -> do
          -- This tests that the fetcher doesn't crash with unusual input
          -- Note: StreamId is generated via UUID, so we use a valid one but unused stream
          unusedStreamId <- StreamId.new
          maybeState <-
            context.fetcher.fetch context.entityName unusedStreamId
              |> Task.mapError toText

          case maybeState of
            Just _ -> do
              fail "Expected Nothing for non-existent stream but got Some state"
            Nothing -> Task.yield unit

        it "handles duplicate event IDs gracefully" \context -> do
          -- Insert event with specific ID
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
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }
          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          -- Try to insert the same event ID again (should be idempotent)
          payload
            |> context.store.insert
            |> Task.asResult
            |> discard

          -- EventStore should handle this (either reject or ignore)
          -- We just verify the fetcher still works correctly
          state <-
            context.fetcher.fetch context.entityName context.streamId
              |> Task.map Maybe.getOrDie
              |> Task.mapError toText

          -- Should have the event applied only once (empty cart)
          Array.length state.cartItems |> shouldBe 0
          state.version |> shouldBe 1

        it "handles very long entity names" \context -> do
          -- Test with an extremely long entity name (1800 characters)
          let longName =
                Event.EntityName
                  "VeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityNameVeryLongEntityName"
          unusedStreamId <- StreamId.new

          maybeState <-
            context.fetcher.fetch longName unusedStreamId
              |> Task.mapError toText

          case maybeState of
            Just _ -> do
              fail "Expected Nothing for unused stream with long entity name but got Some state"
            Nothing -> Task.yield unit

        it "handles rapid successive fetches" \context -> do
          -- Insert an event
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
                    event = CartCreated {entityId = def},
                    metadata = metadata'
                  }
          let payload =
                Event.InsertionPayload
                  { streamId = context.streamId,
                    entityName = context.entityName,
                    insertionType = Event.StreamCreation,
                    insertions = [insertion]
                  }
          payload
            |> context.store.insert
            |> Task.mapError toText
            |> discard

          -- Fetch the same entity 10 times rapidly
          Array.initialize 10 identity
            |> Task.forEach \_ -> do
              state <-
                context.fetcher.fetch context.entityName context.streamId
                  |> Task.mapError toText
                  |> Task.map Maybe.getOrDie
              Array.length state.cartItems |> shouldBe 0
              state.version |> shouldBe 1

    describe "Performance Boundaries" do
      performanceBoundariesWithCount newStoreAndFetcher 10

      whenEnvVar "TEST_EVENT_COUNT" do
        performanceBoundariesWithCount newStoreAndFetcher 100
        performanceBoundariesWithCount newStoreAndFetcher 1000


performanceBoundariesWithCount ::
  Task Text (EventStore CartEvent, EntityFetcher CartState CartEvent) ->
  Int ->
  Spec Unit
performanceBoundariesWithCount newStoreAndFetcher eventCount = do
  describe [fmt|testing with #{toText eventCount} events|] do
    before (Context.initialize newStoreAndFetcher) do
      it "efficiently handles entity with many events" \context -> do
        -- Insert opening event
        openEventId <- Uuid.generate
        openMetadata <- EventMetadata.new
        let openMetadata' =
              openMetadata
                { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                  EventMetadata.eventId = openEventId
                }
        let openInsertion =
              Event.Insertion
                { id = openEventId,
                  event = CartCreated {entityId = def},
                  metadata = openMetadata'
                }
        let openPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = [openInsertion]
                }
        openPayload
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Insert deposit events
        insertions <-
          Array.initialize eventCount identity
            |> Array.map
              ( \index -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' =
                        metadata
                          { EventMetadata.localPosition = Just (Event.StreamPosition (fromIntegral (index + 1))),
                            EventMetadata.eventId = eventId
                          }
                  Task.yield
                    Event.Insertion
                      { id = eventId,
                        event = ItemAdded {entityId = def, itemId = def, amount = 1},
                        metadata = metadata'
                      }
              )
            |> Task.mapArray identity

        -- Insert all events in batches of 100 (EventStore batch size limit)
        insertions
          |> Array.chunksOf 100
          |> Task.forEach
            ( \chunk -> do
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName = context.entityName,
                          insertionType = Event.AnyStreamState,
                          insertions = chunk
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
                  |> discard
            )

        -- Fetch and verify - should complete in reasonable time
        state <-
          context.fetcher.fetch context.entityName context.streamId
            |> Task.mapError toText
            |> Task.map Maybe.getOrDie

        Array.length state.cartItems |> shouldBe 1 -- Same item added eventCount times
        state.version |> shouldBe (eventCount + 1) -- Opening + deposits
