module Test.Service.EventStore.OptimisticConcurrency.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Result qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EventMetadata qualified as EventMetadata
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent (..))
import Test.Service.EventStore.OptimisticConcurrency.Context qualified as Context
import Uuid qualified


spec :: Task Text (EventStore MyEvent) -> Spec Unit
spec newStore = do
  describe "Optimistic Concurrency" do
    beforeAll (Context.initialize newStore) do
      it "will only allow one event to be appended, when two writers try to append at the same time" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- First, append an initial event to position 0
        initialEventId <- Uuid.generate
        initialMetadata <- EventMetadata.new
        let initialMetadata' =
              initialMetadata
                { EventMetadata.localPosition = Just (Event.StreamPosition 0),
                  EventMetadata.eventId = initialEventId
                }
        let initialInsertion =
              Event.Insertion
                { id = initialEventId,
                  event = MyEvent,
                  metadata = initialMetadata'
                }
        let initialPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.fromLinkedList [initialInsertion]
                }

        initialPayload
          |> context.store.insert
          |> Task.mapError toText
          |> discard

        -- Create two identical events both expecting to append at position 1
        event1Id <- Uuid.generate
        event1Metadata <- EventMetadata.new
        let event1Metadata' =
              event1Metadata
                { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                  EventMetadata.eventId = event1Id
                }
        let event1Insertion =
              Event.Insertion
                { id = event1Id,
                  event = MyEvent,
                  metadata = event1Metadata'
                }
        let event1Payload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.InsertAfter (Event.StreamPosition 0),
                  insertions = Array.fromLinkedList [event1Insertion]
                }

        event2Id <- Uuid.generate
        event2Metadata <- EventMetadata.new
        let event2Metadata' =
              event2Metadata
                { EventMetadata.localPosition = Just (Event.StreamPosition 1),
                  EventMetadata.eventId = event2Id
                }
        let event2Insertion =
              Event.Insertion
                { id = event2Id,
                  event = MyEvent,
                  metadata = event2Metadata'
                }
        let event2Payload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.InsertAfter (Event.StreamPosition 0),
                  insertions = Array.fromLinkedList [event2Insertion]
                }

        let event1Task =
              event1Payload
                |> context.store.insert
                |> discard
                |> Task.asResult

        let event2Task =
              event2Payload
                |> context.store.insert
                |> discard
                |> Task.asResult

        (result1, result2) <- AsyncTask.runConcurrently (event1Task, event2Task)

        Array.fromLinkedList [result1, result2]
          |> Array.map (\x -> if Result.isOk x then 1 else 0)
          |> Array.sumIntegers
          |> shouldBe 1

        -- Read back all events to verify
        events <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit (10))
            |> Task.mapError toText

        -- We should have exactly 2 events (initial + one successful append)
        events
          |> Array.length
          |> shouldBe 2

        -- The first event should be our initial event
        events
          |> Array.get 0
          |> Maybe.map (\event -> event.metadata.eventId)
          |> shouldBe (Just initialEventId)

        -- The second event should be one of our concurrent events
        events
          |> Array.get 1
          |> Maybe.map (\event -> event.metadata.eventId)
          |> shouldSatisfy (\id -> id == Just event1Id || id == Just event2Id)

      it "gives consistency error when stream position is not up to date" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Insert 5 events to get the stream to position 4 (0-indexed)
        let insertEventAtPosition position = do
              eventId <- Uuid.generate
              metadata <- EventMetadata.new
              let metadata' = metadata {EventMetadata.localPosition = Just (Event.StreamPosition position)}
              let insertion =
                    Event.Insertion
                      { id = eventId,
                        event = MyEvent,
                        metadata = metadata'
                      }
              let insertionType =
                    case position of
                      0 -> Event.StreamCreation
                      _ -> Event.InsertAfter (Event.StreamPosition (position - 1))
              let payload =
                    Event.InsertionPayload
                      { streamId = context.streamId,
                        entityName,
                        insertionType,
                        insertions = Array.fromLinkedList [insertion]
                      }
              payload
                |> context.store.insert
                |> Task.mapError toText

        -- Insert events at positions 0, 1, 2, 3, 4
        Array.fromLinkedList [0, 1, 2, 3, 4]
          |> Task.mapArray insertEventAtPosition
          |> discard

        -- Try to insert at an outdated position (position 2, when current is at 4)
        -- This should fail with ConcurrencyConflict
        staleEventId <- Uuid.generate
        staleMetadata <- EventMetadata.new
        let staleMetadata' = staleMetadata {EventMetadata.localPosition = Just (Event.StreamPosition 2)}
        let staleInsertion =
              Event.Insertion
                { id = staleEventId,
                  event = MyEvent,
                  metadata = staleMetadata'
                }
        let stalePayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.InsertAfter (Event.StreamPosition 1),
                  insertions = Array.fromLinkedList [staleInsertion]
                }

        staleResult <-
          stalePayload
            |> context.store.insert
            |> Task.asResult

        -- Should fail with ConcurrencyConflict
        staleResult
          |> Result.isErr
          |> shouldBe True

        -- Try to insert at the correct position (position 5)
        -- This should succeed
        correctEventId <- Uuid.generate
        correctMetadata <- EventMetadata.new
        let correctMetadata' = correctMetadata {EventMetadata.localPosition = Just (Event.StreamPosition 5)}
        let correctInsertion =
              Event.Insertion
                { id = correctEventId,
                  event = MyEvent,
                  metadata = correctMetadata'
                }
        let correctPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.InsertAfter (Event.StreamPosition 4),
                  insertions = Array.fromLinkedList [correctInsertion]
                }

        correctResult <-
          correctPayload
            |> context.store.insert
            |> Task.mapError toText

        -- Should succeed and have the correct local position
        correctResult.localPosition
          |> shouldBe (Event.StreamPosition 5)

        -- Verify final stream state has 6 events
        finalEvents <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 10)
            |> Task.mapError toText

        finalEvents
          |> Array.length
          |> shouldBe 6

        -- Last event should be our correctly inserted event
        finalEvents
          |> Array.get 5
          |> Maybe.map (\event -> event.metadata.eventId)
          |> shouldBe (Just correctEventId)

      it "insertion is idempotent by event id" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Create 10 events with specific IDs (same as C# EventCount)
        eventsToInsert <-
          Array.fromLinkedList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            |> Task.mapArray
              ( \position -> do
                  eventId <- Uuid.generate
                  metadata <- EventMetadata.new
                  let metadata' = metadata {EventMetadata.localPosition = Just (Event.StreamPosition position)}
                  let insertion =
                        Event.Insertion
                          { id = eventId,
                            event = MyEvent,
                            metadata = metadata'
                          }
                  let insertionType =
                        case position of
                          0 -> Event.StreamCreation
                          _ -> Event.InsertAfter (Event.StreamPosition (position - 1))
                  Task.yield
                    Event.InsertionPayload
                      { streamId = context.streamId,
                        entityName,
                        insertionType,
                        insertions = Array.fromLinkedList [insertion]
                      }
              )

        -- Insert all events the first time - should succeed
        eventsToInsert
          |> Task.mapArray (\payload -> context.store.insert payload |> Task.mapError toText)
          |> discard

        -- Insert the exact same events again (same IDs, same everything)
        -- This should be idempotent - either skip duplicates or succeed without duplicating
        eventsToInsert
          |> Task.mapArray (\payload -> context.store.insert payload |> Task.asResult)
          |> discard

        -- Read the stream - should have exactly 10 events (not 20)
        finalEvents <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText

        -- Should have exactly 10 events (idempotency - no duplicates)
        finalEvents
          |> Array.length
          |> shouldBe 10
