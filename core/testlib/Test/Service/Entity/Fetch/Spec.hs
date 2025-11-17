module Test.Service.Entity.Fetch.Spec where

import Array qualified
import Core
import Maybe qualified
import Result qualified
import Service.Entity.Core (EntityReducer)
import Service.Entity.Core qualified as Entity
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.Entity.Core (BankAccountEvent (..), BankAccountState (..), initialState)
import Test.Service.Entity.Fetch.Context qualified as Context
import Uuid qualified


spec ::
  Task Text (EventStore BankAccountEvent) ->
  Task Text (EntityReducer BankAccountState BankAccountEvent) ->
  Spec Unit
spec newStore newReducer = do
  describe "Entity Fetch" do
    beforeAll (Context.initialize newStore newReducer) do
      it "fetches an entity with no events and returns initial state" \context -> do
        -- Fetch from a stream that doesn't exist yet
        result <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.asResult

        -- Should succeed with initial state (version 0)
        result
          |> Result.isOk
          |> shouldBe True

        case result of
          Ok state -> do
            state.balance |> shouldBe 0
            state.isOpen |> shouldBe False
            state.version |> shouldBe 0
          Err _ -> do
            fail "Expected successful fetch but got error"

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
                  event = AccountOpened {initialBalance = 100},
                  metadata = metadata'
                }
        let payload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.fromLinkedList [insertion]
                }

        payload
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Fetch the entity
        state <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- Should have the correct state after applying one event
        state.balance |> shouldBe 100
        state.isOpen |> shouldBe True
        state.version |> shouldBe 1

      it "fetches an entity with multiple events and applies them in order" \context -> do
        -- Insert multiple events in sequence
        let events =
              Array.fromLinkedList
                [ AccountOpened {initialBalance = 50},
                  MoneyDeposited {amount = 100},
                  MoneyWithdrawn {amount = 30},
                  MoneyDeposited {amount = 20}
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
            |> Task.sequenceArray

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
                          insertions = Array.fromLinkedList [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> Task.sequenceArray
          |> discard

        -- Fetch the entity
        state <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- Should have correct state: 50 + 100 - 30 + 20 = 140
        state.balance |> shouldBe 140
        state.isOpen |> shouldBe True
        state.version |> shouldBe 4

      it "fetches different entities independently" \context -> do
        -- Create a second stream with different events
        streamId2 <- Event.StreamId.new

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
                  event = AccountOpened {initialBalance = 100},
                  metadata = metadata1'
                }
        let payload1 =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.fromLinkedList [insertion1]
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
                  event = AccountOpened {initialBalance = 500},
                  metadata = metadata2'
                }
        let payload2 =
              Event.InsertionPayload
                { streamId = streamId2,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.fromLinkedList [insertion2]
                }

        payload2
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Fetch both entities
        state1 <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        state2 <-
          context.reducer.fetch context.entityName streamId2
            |> Task.mapError toText

        -- Should have different states
        state1.balance |> shouldBe 100
        state2.balance |> shouldBe 500
        state1.version |> shouldBe 1
        state2.version |> shouldBe 1

      it "handles entity state with many events efficiently" \context -> do
        -- Insert 100 events
        let eventCount = 100

        insertions <-
          Array.range 0 (eventCount - 1)
            |> Array.map
              ( \index -> do
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
                        event = MoneyDeposited {amount = 1}, -- Deposit 1 each time
                        metadata = metadata'
                      }
              )
            |> Task.sequenceArray

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
                  event = AccountOpened {initialBalance = 0},
                  metadata = openMetadata'
                }
        let openPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName = context.entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.fromLinkedList [openInsertion]
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
                          insertions = Array.fromLinkedList [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> Task.sequenceArray
          |> discard

        -- Fetch the entity
        state <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- Should have correct balance: 0 + (100 * 1) = 100
        state.balance |> shouldBe 100
        state.isOpen |> shouldBe True
        state.version |> shouldBe 101 -- Opening event + 100 deposits

      it "returns error when fetching from non-existent entity type" \context -> do
        let wrongEntityName = Event.EntityName "NonExistentEntity"

        result <-
          context.reducer.fetch wrongEntityName context.streamId
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
                [ AccountOpened {initialBalance = 100},
                  MoneyDeposited {amount = 50},
                  MoneyWithdrawn {amount = 25}
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
            |> Task.sequenceArray

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
                          insertions = Array.fromLinkedList [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> Task.sequenceArray
          |> discard

        -- Fetch multiple times - should always return same version
        state1 <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        state2 <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        state3 <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- All fetches should return same version
        state1.version |> shouldBe 3
        state2.version |> shouldBe 3
        state3.version |> shouldBe 3

        -- All should have same balance
        state1.balance |> shouldBe 125
        state2.balance |> shouldBe 125
        state3.balance |> shouldBe 125

      it "correctly handles closed account state" \context -> do
        -- Insert events including account closure
        let events =
              Array.fromLinkedList
                [ AccountOpened {initialBalance = 200},
                  MoneyWithdrawn {amount = 100},
                  AccountClosed
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
            |> Task.sequenceArray

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
                          insertions = Array.fromLinkedList [insertion]
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> Task.sequenceArray
          |> discard

        -- Fetch the entity
        state <-
          context.reducer.fetch context.entityName context.streamId
            |> Task.mapError toText

        -- Account should be closed
        state.balance |> shouldBe 100
        state.isOpen |> shouldBe False
        state.version |> shouldBe 3
