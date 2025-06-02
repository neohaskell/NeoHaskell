module Test.Service.EventStore.OptimisticConcurrency.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import Test
import Test.Service.EventStore.OptimisticConcurrency.Context qualified as Context
import ToText (toText)


spec :: Task Text EventStore -> Spec Unit
spec newStore = do
  describe "Optimistic Concurrency" do
    beforeAll (Context.initialize newStore) do
      it "will only allow one event to be appended, when two writers try to append at the same time" \ctx -> do
        -- First, append an initial event to position 0
        let initialEvent =
              Event
                { id = "event-0",
                  streamId = ctx.streamId,
                  position = Event.StreamPosition 0,
                  globalPosition = Nothing
                }

        initialEvent
          |> ctx.store.appendToStream ctx.streamId (Event.StreamPosition 0)
          |> Task.mapError toText

        -- Create two identical events both expecting to append at position 1
        let event1 =
              Event
                { id = "event-1-writer1",
                  streamId = ctx.streamId,
                  position = Event.StreamPosition 1,
                  globalPosition = Nothing
                }

        let event2 =
              Event
                { id = "event-1-writer2",
                  streamId = ctx.streamId,
                  position = Event.StreamPosition 1,
                  globalPosition = Nothing
                }

        -- Prepare concurrent operations
        let append1 =
              event1
                |> ctx.store.appendToStream ctx.streamId (Event.StreamPosition 1)
                |> Task.mapError toText
                |> AsyncTask.run

        let append2 =
              event2
                |> ctx.store.appendToStream ctx.streamId (Event.StreamPosition 1)
                |> Task.mapError toText
                |> AsyncTask.run

        -- Run both operations concurrently
        thread1 <- append1
        thread2 <- append2

        [thread1, thread2]
          |> Array.fromLinkedList
          |> AsyncTask.waitAnyCancel
          |> discard

        -- Read back all events to verify
        events <-
          ctx.store.readStreamForwardFrom ctx.streamId (Event.StreamPosition 0) (EventStore.Limit (Positive 10))
            |> Task.mapError toText

        -- We should have exactly 2 events (initial + one successful append)
        events
          |> Array.length
          |> shouldBe 2

        -- The first event should be our initial event
        events
          |> Array.get 0
          |> Maybe.map (\event -> event.id)
          |> Maybe.withDefault ("No event found")
          |> shouldBe "event-0"

        -- The second event should be one of our concurrent events
        events
          |> Array.get 1
          |> Maybe.map (\event -> event.id)
          |> Maybe.withDefault ("No event found")
          |> shouldStartWith "event-1-writer"
