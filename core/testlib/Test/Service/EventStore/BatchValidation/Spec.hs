module Test.Service.EventStore.BatchValidation.Spec where

import Array qualified
import Core
import Result qualified
import Service.Event qualified as Event
import Service.EventStore (EventStore, collectStreamEvents)
import Service.EventStore.Core qualified as EventStore
import Stream qualified
import Task qualified
import Test
import Test.Service.EventStore.BatchValidation.Context qualified as Context
import Test.Service.EventStore.Core (CartEvent (..), newInsertion)
import Uuid qualified


spec :: Task Text (EventStore CartEvent) -> Spec Unit
spec newStore = do
  describe "Batch Operation Validation" do
    beforeAll (Context.initialize newStore) do
      it "should reject empty insertion arrays" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Create a payload with empty insertions array
        let emptyPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.empty
                }

        -- This should fail - empty insertions should not be allowed
        result <-
          emptyPayload
            |> context.store.insert
            |> Task.asResult

        -- Verify it failed
        result
          |> Result.isErr
          |> shouldBe True

      it "should reject batch operations with more than 100 events" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Create 101 insertions (exceeding the limit of 100)
        insertions <-
          Array.fromLinkedList [0 .. 100]
            |> Task.mapArray newInsertion

        let oversizedPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions
                }

        -- This should fail - batch size exceeds limit
        result <-
          oversizedPayload
            |> context.store.insert
            |> Task.asResult

        -- Verify it failed
        result
          |> Result.isErr
          |> shouldBe True

      it "should accept batch operations with exactly 100 events" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Create exactly 100 insertions (at the limit)
        insertions <-
          Array.fromLinkedList [0 .. 99]
            |> Task.mapArray newInsertion

        let maxSizePayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions
                }

        -- This should succeed - exactly at the limit
        result <-
          maxSizePayload
            |> context.store.insert
            |> Task.mapError toText

        -- Verify success
        result.localPosition
          |> shouldBe (Event.StreamPosition 99)

        -- Verify all 100 events were inserted
        stream <-
          context.store.readAllStreamEvents entityName context.streamId
            |> Task.mapError toText
        allEvents <- Stream.toArray stream

        allEvents
          |> collectStreamEvents
          |> Array.length
          |> shouldBe 100

      it "should handle large batch near the limit without memory issues" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        -- Create 99 insertions (just under the limit)
        insertions <-
          Array.fromLinkedList [0 .. 98]
            |> Task.mapArray newInsertion

        let largeBatchPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions
                }

        -- This should succeed without memory issues
        result <-
          largeBatchPayload
            |> context.store.insert
            |> Task.mapError toText

        -- Verify success
        result.localPosition
          |> shouldBe (Event.StreamPosition 98)

        -- Verify all 99 events were inserted
        stream <-
          context.store.readAllStreamEvents entityName context.streamId
            |> Task.mapError toText
        allEvents <- Stream.toArray stream

        allEvents
          |> collectStreamEvents
          |> Array.length
          |> shouldBe 99

      it "should provide meaningful error message for empty batch" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        let emptyPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions = Array.empty
                }

        result <-
          emptyPayload
            |> context.store.insert
            |> Task.asResult

        -- Should have a meaningful error
        case result of
          Err error -> do
            -- Error should be some form of validation error
            -- This will fail until proper validation is implemented
            error |> toText |> shouldNotBe ""
          Ok _ -> do
            -- Should not succeed with empty batch
            True |> shouldBe False

      it "should provide meaningful error message for oversized batch" \context -> do
        entityNameText <- Uuid.generate |> Task.map toText
        let entityName = Event.EntityName entityNameText

        insertions <-
          Array.fromLinkedList [0 .. 100]
            |> Task.mapArray newInsertion

        let oversizedPayload =
              Event.InsertionPayload
                { streamId = context.streamId,
                  entityName,
                  insertionType = Event.StreamCreation,
                  insertions
                }

        result <-
          oversizedPayload
            |> context.store.insert
            |> Task.asResult

        -- Should have a meaningful error
        case result of
          Err error -> do
            -- Error should be some form of validation error
            error |> toText |> shouldNotBe ""
          Ok _ -> do
            -- Should not succeed with oversized batch
            True |> shouldBe False
