module Test.Service.EventStore.OptimisticConcurrency.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Result qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.OptimisticConcurrency.Context qualified as Context
import Uuid qualified


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Optimistic Concurrency" do
    beforeAll (Context.initialize newStore) do
      it "will only allow one event to be appended, when two writers try to append at the same time" \context -> do
        entityId <- Uuid.generate |> Task.map Event.EntityId

        initialEventId <- Uuid.generate
        -- First, append an initial event to position 0
        let initialEvent =
              Event.InsertionEvent
                { id = initialEventId,
                  streamId = context.streamId,
                  entityId,
                  localPosition = Event.StreamPosition 0
                }

        initialEvent
          |> context.store.appendToStream
          |> Task.mapError toText
          |> discard

        -- Create two identical events both expecting to append at position 1
        event1Id <- Uuid.generate
        let event1 =
              Event.InsertionEvent
                { id = event1Id,
                  streamId = context.streamId,
                  entityId,
                  localPosition = Event.StreamPosition 1
                }

        event2Id <- Uuid.generate
        let event2 =
              Event.InsertionEvent
                { id = event2Id,
                  streamId = context.streamId,
                  entityId,
                  localPosition = Event.StreamPosition 1
                }

        let event1Task =
              event1
                |> context.store.appendToStream
                |> discard
                |> Task.asResult

        let event2Task =
              event2
                |> context.store.appendToStream
                |> discard
                |> Task.asResult

        (result1, result2) <- AsyncTask.runConcurrently (event1Task, event2Task)

        Array.fromLinkedList [result1, result2]
          |> Array.map (\x -> if Result.isOk x then 1 else 0)
          |> Array.sumIntegers
          |> shouldBe 1

        -- Read back all events to verify
        events <-
          context.store.readStreamForwardFrom entityId context.streamId (Event.StreamPosition 0) (EventStore.Limit (10))
            |> Task.mapError toText

        -- We should have exactly 2 events (initial + one successful append)
        events
          |> Array.length
          |> shouldBe 2

        -- The first event should be our initial event
        events
          |> Array.get 0
          |> Maybe.map (\event -> event.id)
          |> shouldBe (Just initialEventId)

        -- The second event should be one of our concurrent events
        events
          |> Array.get 1
          |> Maybe.map (\event -> event.id)
          |> shouldSatisfy (\id -> id == Just event1Id || id == Just event2Id)

      it "gives consistency error when stream position is not up to date" \context -> do
        entityId <- Uuid.generate |> Task.map Event.EntityId

        -- Insert 5 events to get the stream to position 5
        let insertEventAtPosition position = do
              eventId <- Uuid.generate
              let event =
                    Event.InsertionEvent
                      { id = eventId,
                        streamId = context.streamId,
                        entityId,
                        localPosition = Event.StreamPosition position
                      }
              event
                |> context.store.appendToStream
                |> Task.mapError toText

        -- Insert events at positions 0, 1, 2, 3, 4
        Array.fromLinkedList [0, 1, 2, 3, 4]
          |> Task.mapArray insertEventAtPosition
          |> discard

        -- Try to insert at an outdated position (position 2, when current is 5)
        -- This should fail with ConcurrencyConflict
        staleEventId <- Uuid.generate
        let staleEvent =
              Event.InsertionEvent
                { id = staleEventId,
                  streamId = context.streamId,
                  entityId,
                  localPosition = Event.StreamPosition 2
                }

        staleResult <-
          staleEvent
            |> context.store.appendToStream
            |> Task.asResult

        -- Should fail with ConcurrencyConflict
        staleResult
          |> Result.isErr
          |> shouldBe True

        -- Try to insert at the correct position (position 5)
        -- This should succeed
        correctEventId <- Uuid.generate
        let correctEvent =
              Event.InsertionEvent
                { id = correctEventId,
                  streamId = context.streamId,
                  entityId,
                  localPosition = Event.StreamPosition 5
                }

        correctResult <-
          correctEvent
            |> context.store.appendToStream
            |> Task.mapError toText

        -- Should succeed and have the correct local position
        correctResult.localPosition
          |> shouldBe (Event.StreamPosition 5)

        -- Verify final stream state has 6 events
        finalEvents <-
          context.store.readStreamForwardFrom entityId context.streamId (Event.StreamPosition 0) (EventStore.Limit 10)
            |> Task.mapError toText

        finalEvents
          |> Array.length
          |> shouldBe 6

        -- Last event should be our correctly inserted event
        finalEvents
          |> Array.get 5
          |> Maybe.map (\event -> event.id)
          |> shouldBe (Just correctEventId)

      it "insertion is idempotent by event id" \context -> do
        entityId <- Uuid.generate |> Task.map Event.EntityId

        -- Create 10 events with specific IDs (same as C# EventCount)
        eventsToInsert <-
          Array.fromLinkedList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
            |> Task.mapArray
              ( \position -> do
                  eventId <- Uuid.generate
                  Task.yield
                    Event.InsertionEvent
                      { id = eventId,
                        streamId = context.streamId,
                        entityId,
                        localPosition = Event.StreamPosition position
                      }
              )

        -- Insert all events the first time - should succeed
        eventsToInsert
          |> Task.mapArray (\event -> context.store.appendToStream event |> Task.mapError toText)
          |> discard

        -- Insert the exact same events again (same IDs, same everything)
        -- This should be idempotent - either skip duplicates or succeed without duplicating
        eventsToInsert
          |> Task.mapArray (\event -> context.store.appendToStream event |> Task.asResult)
          |> discard

        -- Read the stream - should have exactly 10 events (not 20)
        finalEvents <-
          context.store.readStreamForwardFrom entityId context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText

        -- Should have exactly 10 events (idempotency - no duplicates)
        finalEvents
          |> Array.length
          |> shouldBe 10
