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
