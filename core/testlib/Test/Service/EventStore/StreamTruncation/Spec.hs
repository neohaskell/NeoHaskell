module Test.Service.EventStore.StreamTruncation.Spec where

import Array qualified
import Core
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata (EventMetadata (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent, newInsertion)
import Test.Service.EventStore.StreamTruncation.Context qualified as Context
import Uuid qualified


spec :: Task Text (EventStore MyEvent) -> Spec Unit
spec newStore = do
  describe "Stream Truncation" do
    before (Context.initialize newStore) do
      it "truncates stream keeping events from position onwards" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Insert 10 events at positions 0-9
        let eventCount = 10
        Array.fromLinkedList [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
          |> Task.mapArray
            ( \position -> do
                insertions <- Array.fromLinkedList [position] |> Task.mapArray newInsertion
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName,
                          insertionType = Event.AnyStreamState,
                          insertions
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> discard

        -- Verify all 10 events are there
        eventsBeforeTruncateMessages <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText
            |> Task.andThen Stream.toArray

        let eventsBeforeTruncate = EventStore.collectStreamEvents eventsBeforeTruncateMessages

        eventsBeforeTruncate
          |> Array.length
          |> shouldBe eventCount

        -- Truncate at position 5 (eventCount / 2 = 10 / 2 = 5)
        let truncatePosition = Event.StreamPosition (fromIntegral (eventCount // 2))
        context.store.truncateStream entityName context.streamId truncatePosition
          |> Task.mapError toText

        -- Read after truncation
        eventsAfterTruncateMessages <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText
            |> Task.andThen Stream.toArray

        let eventsAfterTruncate = EventStore.collectStreamEvents eventsAfterTruncateMessages

        -- Should have 5 events remaining (positions 5, 6, 7, 8, 9)
        let expectedCount = eventCount - (eventCount // 2)
        eventsAfterTruncate
          |> Array.length
          |> shouldBe expectedCount

        -- Verify the remaining events start at position 5
        eventsAfterTruncate
          |> Array.get 0
          |> Maybe.map (\event -> event.metadata.localPosition |> Maybe.getOrDie)
          |> shouldBe (Just (Event.StreamPosition 5))

        -- Verify the last event is at position 9
        eventsAfterTruncate
          |> Array.get (expectedCount - 1)
          |> Maybe.map (\event -> event.metadata.localPosition |> Maybe.getOrDie)
          |> shouldBe (Just (Event.StreamPosition 9))

      it "truncating at position 0 keeps all events" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Insert 5 events
        Array.fromLinkedList [0, 1, 2, 3, 4]
          |> Task.mapArray
            ( \position -> do
                insertions <- Array.fromLinkedList [position] |> Task.mapArray newInsertion
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName,
                          insertionType = Event.AnyStreamState,
                          insertions
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> discard

        -- Truncate at position 0 (keep nothing before position 0)
        context.store.truncateStream entityName context.streamId (Event.StreamPosition 0)
          |> Task.mapError toText

        -- Read after truncation - should still have all events (nothing before 0)
        eventsAfterTruncateMessages <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText
            |> Task.andThen Stream.toArray

        let eventsAfterTruncate = EventStore.collectStreamEvents eventsAfterTruncateMessages

        eventsAfterTruncate
          |> Array.length
          |> shouldBe 5

      it "truncating at position beyond stream length removes all events" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Insert 5 events
        Array.fromLinkedList [0, 1, 2, 3, 4]
          |> Task.mapArray
            ( \position -> do
                insertions <- Array.fromLinkedList [position] |> Task.mapArray newInsertion
                let payload =
                      Event.InsertionPayload
                        { streamId = context.streamId,
                          entityName,
                          insertionType = Event.AnyStreamState,
                          insertions
                        }
                payload
                  |> context.store.insert
                  |> Task.mapError toText
            )
          |> discard

        -- Truncate at position 100 (way beyond stream length)
        context.store.truncateStream entityName context.streamId (Event.StreamPosition 100)
          |> Task.mapError toText

        -- Read after truncation - should have no events (all removed)
        eventsAfterTruncateMessages <-
          context.store.readStreamForwardFrom entityName context.streamId (Event.StreamPosition 0) (EventStore.Limit 20)
            |> Task.mapError toText
            |> Task.andThen Stream.toArray

        let eventsAfterTruncate = EventStore.collectStreamEvents eventsAfterTruncateMessages

        eventsAfterTruncate
          |> Array.length
          |> shouldBe 0
