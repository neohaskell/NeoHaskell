module Service.EventStore.Postgres.SubscriptionStoreSpec (spec) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Json qualified
import Map qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Postgres.SubscriptionStore (Error (..), SubscriptionStore (..))
import Service.EventStore.Postgres.SubscriptionStore qualified as SubscriptionStore
import Service.EventStore.Core (SubscriptionId (..))
import Task qualified
import Test
import Test.Service.EventStore.Core (CartEvent (..))


spec :: Spec Unit
spec = do
  describe "SubscriptionStore" do
    describe "Basic Storage Operations" do
      it "creates a new empty subscription store" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        streamSubs <- store.streamSubscriptions |> ConcurrentVar.peek

        globalSubs |> Map.length |> shouldBe 0
        streamSubs |> Map.length |> shouldBe 0

      it "adds a global subscription" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback _msg = Task.yield unit

        store |> SubscriptionStore.addGlobalSubscription callback |> discard |> Task.mapError toText

        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Map.length |> shouldBe 1

      it "adds multiple global subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback1 _msg = Task.yield unit
        let callback2 _msg = Task.yield unit
        let callback3 _msg = Task.yield unit

        store |> SubscriptionStore.addGlobalSubscription callback1 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription callback2 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription callback3 |> discard |> Task.mapError toText

        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Map.length |> shouldBe 3

      it "adds a stream subscription" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "test-entity"

        let callback _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 1

      it "adds multiple subscriptions to the same stream" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "test-entity"

        let callback1 _msg = Task.yield unit
        let callback2 _msg = Task.yield unit
        let callback3 _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription entityName streamId callback1 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback2 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback3 |> discard |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 3

      it "keeps subscriptions for different streams separate" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        streamId3 <- StreamId.new
        let entityName = Event.EntityName "test-entity"

        let callback _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription entityName streamId1 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId1 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId2 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId3 callback |> discard |> Task.mapError toText

        subs1 <- store |> SubscriptionStore.getStreamSubscriptions streamId1 |> Task.mapError toText
        subs2 <- store |> SubscriptionStore.getStreamSubscriptions streamId2 |> Task.mapError toText
        subs3 <- store |> SubscriptionStore.getStreamSubscriptions streamId3 |> Task.mapError toText

        subs1 |> Map.length |> shouldBe 2
        subs2 |> Map.length |> shouldBe 1
        subs3 |> Map.length |> shouldBe 1

      it "returns empty array for stream with no subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 0

    describe "Concurrent Modifications" do
      it "handles concurrent additions to different streams" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        streamId3 <- StreamId.new
        let entityName = Event.EntityName "test-entity"

        let callback _msg = Task.yield unit

        -- Add subscriptions to all streams concurrently
        let addToStream1 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription entityName streamId1 callback |> discard |> Task.mapError toText)

        let addToStream2 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription entityName streamId2 callback |> discard |> Task.mapError toText)

        let addToStream3 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription entityName streamId3 callback |> discard |> Task.mapError toText)

        (_, (_, _)) <- AsyncTask.runConcurrently (addToStream1, AsyncTask.runConcurrently (addToStream2, addToStream3))

        -- Verify each stream has the correct number of subscriptions
        subs1 <- store |> SubscriptionStore.getStreamSubscriptions streamId1 |> Task.mapError toText
        subs2 <- store |> SubscriptionStore.getStreamSubscriptions streamId2 |> Task.mapError toText
        subs3 <- store |> SubscriptionStore.getStreamSubscriptions streamId3 |> Task.mapError toText

        subs1 |> Map.length |> shouldBe 5
        subs2 |> Map.length |> shouldBe 5
        subs3 |> Map.length |> shouldBe 5

      it "handles large numbers of subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback _msg = Task.yield unit
        let count = 100

        -- Add many global subscriptions
        Array.fromLinkedList ([1 .. count] :: [Int])
          |> Task.forEach (\_ -> store |> SubscriptionStore.addGlobalSubscription callback |> discard |> Task.mapError toText)

        -- Verify count
        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Map.length |> shouldBe count

        -- Add many stream subscriptions
        streamId <- StreamId.new
        let entityName = Event.EntityName "test-entity"
        Array.fromLinkedList ([1 .. count] :: [Int])
          |> Task.forEach
            (\_ -> store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText)

        streamSubs <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        streamSubs |> Map.length |> shouldBe count

    describe "Dispatch Functionality" do
      it "dispatches messages to stream subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        executionCount <- ConcurrentVar.containing (0 :: Int)

        let callback _msg = do
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add subscriptions
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message
        store |> SubscriptionStore.dispatch streamId (event) |> Task.mapError toText

        -- Verify all callbacks were executed
        count <- ConcurrentVar.get executionCount
        count |> shouldBe 3

      it "dispatches messages to both stream and global subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        streamExecutionCount <- ConcurrentVar.containing (0 :: Int)
        globalExecutionCount <- ConcurrentVar.containing (0 :: Int)

        let streamCallback _msg = do
              streamExecutionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        let globalCallback _msg = do
              globalExecutionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add 2 stream subscriptions and 2 global subscriptions
        store |> SubscriptionStore.addStreamSubscription entityName streamId streamCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId streamCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription globalCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription globalCallback |> discard |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message
        store |> SubscriptionStore.dispatch streamId (event) |> Task.mapError toText

        -- Verify all callbacks were executed
        streamCount <- ConcurrentVar.get streamExecutionCount
        globalCount <- ConcurrentVar.get globalExecutionCount
        streamCount |> shouldBe 2
        globalCount |> shouldBe 2

      it "handles callback failures gracefully" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        successCount <- ConcurrentVar.containing (0 :: Int)

        let failingCallback _msg = Task.throw "failed on purpose"

        let successCallback _msg = do
              successCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add a mix of failing and successful callbacks
        store |> SubscriptionStore.addStreamSubscription entityName streamId failingCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId successCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId failingCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId successCallback |> discard |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message - should not fail even though some callbacks fail
        store |> SubscriptionStore.dispatch streamId (event) |> Task.mapError toText

        -- Verify successful callbacks were executed despite failures
        count <- ConcurrentVar.get successCount
        count |> shouldBe 2

      it "dispatches to subscription at starting position (regression test)" \_ -> do
        -- Regression test: subscriptions starting at position N should receive events at position N
        -- Previously the check was `eventPos > startPos` which failed when both were 0
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        executionCount <- ConcurrentVar.containing (0 :: Int)

        let callback _msg = do
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add subscription starting at position 0
        store
          |> SubscriptionStore.addGlobalSubscriptionFromPosition (Just (Event.StreamPosition 0)) callback
          |> discard
          |> Task.mapError toText

        -- Create a test event with globalPosition = 0
        metadata <- EventMetadata.new
        let metadataWithPosition = metadata {EventMetadata.globalPosition = Just (Event.StreamPosition 0)}
        let entityNameForEvent = Event.EntityName "TestEntity"
        let event = ItemAdded {entityId = def, itemId = def, amount = 100}
        let testEvent =
              Event
                { entityName = entityNameForEvent,
                  streamId = streamId,
                  event = Json.encode event,
                  metadata = metadataWithPosition
                }

        -- Dispatch - should trigger the callback since event position (0) >= start position (0)
        store |> SubscriptionStore.dispatch streamId testEvent |> Task.mapError toText

        -- Verify callback was executed
        count <- ConcurrentVar.get executionCount
        count |> shouldBe 1

      it "executes callbacks in parallel (not serially)" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        executionCount <- ConcurrentVar.containing (0 :: Int)

        -- Each callback sleeps for 100ms
        let callback _msg = do
              AsyncTask.sleep 100
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add 5 callbacks
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription entityName streamId callback |> discard |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch - if parallel, should take ~100ms; if serial, would take ~500ms
        -- We allow up to 250ms (generous buffer for parallel execution)
        timedOut <- ConcurrentVar.containing False

        let dispatchTask = do
              store |> SubscriptionStore.dispatch streamId (event) |> Task.mapError toText

        let timeoutTask = do
              AsyncTask.sleep 250
              timedOut |> ConcurrentVar.set True

        -- Start both tasks
        dispatchAsync <- AsyncTask.run dispatchTask
        _ <- AsyncTask.run timeoutTask

        -- Wait for dispatch to complete
        AsyncTask.waitFor dispatchAsync

        -- Check if we timed out
        didTimeout <- ConcurrentVar.get timedOut
        if didTimeout
          then Test.fail "Test timed out - callbacks appear to be running serially instead of in parallel"
          else do
            -- Dispatch completed successfully within timeout
            count <- ConcurrentVar.get executionCount
            count |> shouldBe 5


    describe "addStreamSubscriptionWithCleanup (ADR-0063)" do
      it "registers a stream subscription that is dispatched to like any other stream sub" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        executionCount <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = do
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit
        let noopCleanup = Task.yield unit

        store
          |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing noopCleanup callback
          |> discard
          |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 1

        event <- createTestEvent |> Task.mapError toText
        store |> SubscriptionStore.dispatch streamId event |> Task.mapError toText
        count <- ConcurrentVar.get executionCount
        count |> shouldBe 1

      it "stores the supplied onRemove so it runs on removeSubscription" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = Task.yield unit
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        subId <-
          store
            |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
            |> Task.mapError toText

        store |> SubscriptionStore.removeSubscription subId |> Task.mapError toText

        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 1

      it "does not run onRemove at registration time" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = Task.yield unit
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        store
          |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
          |> discard
          |> Task.mapError toText

        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 0

      it "keeps subscriptions for different streams separate" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        let entityName = Event.EntityName "TestEntity"
        let callback _msg = Task.yield unit
        let noopCleanup = Task.yield unit

        store |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId1 Nothing noopCleanup callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId1 Nothing noopCleanup callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId2 Nothing noopCleanup callback |> discard |> Task.mapError toText

        subs1 <- store |> SubscriptionStore.getStreamSubscriptions streamId1 |> Task.mapError toText
        subs2 <- store |> SubscriptionStore.getStreamSubscriptions streamId2 |> Task.mapError toText
        subs1 |> Map.length |> shouldBe 2
        subs2 |> Map.length |> shouldBe 1

    describe "removeSubscription cleanup (ADR-0063)" do
      it "runs the stored onRemove exactly once on remove" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = Task.yield unit
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        subId <-
          store
            |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
            |> Task.mapError toText
        store |> SubscriptionStore.removeSubscription subId |> Task.mapError toText

        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 1
        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 0

      it "stays total when onRemove throws (unsubscribe never fails)" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        let callback _msg = Task.yield unit
        let failingCleanup = Task.throw "release failed on purpose"

        subId <-
          store
            |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing failingCleanup callback
            |> Task.mapError toText

        result <- store |> SubscriptionStore.removeSubscription subId |> Task.asResult
        case result of
          Err err -> Test.fail [fmt|removeSubscription must stay total, but failed: #{toText err}|]
          Ok _ -> Task.yield unit

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 0

      it "is a no-op for a never-registered id (runs no cleanup)" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"
        let unknownId = SubscriptionId "never-registered-id"

        -- Register a real cleanup-bearing subscription, then remove a DIFFERENT
        -- (unknown) id: the unknown remove must be a no-op AND must not fire the
        -- registered subscription's cleanup.
        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = Task.yield unit
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit
        store
          |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
          |> discard
          |> Task.mapError toText

        result <- store |> SubscriptionStore.removeSubscription unknownId |> Task.asResult
        case result of
          Err err -> Test.fail [fmt|removing an unknown id must be a no-op, but failed: #{toText err}|]
          Ok _ -> Task.yield unit

        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 0
        -- The real subscription is still registered (unknown remove touched nothing).
        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 1

      it "is a no-op (and does not re-run cleanup) on a double remove" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"

        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let callback _msg = Task.yield unit
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        subId <-
          store
            |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
            |> Task.mapError toText

        store |> SubscriptionStore.removeSubscription subId |> Task.mapError toText
        result <- store |> SubscriptionStore.removeSubscription subId |> Task.asResult
        case result of
          Err err -> Test.fail [fmt|second removeSubscription must be a no-op, but failed: #{toText err}|]
          Ok _ -> Task.yield unit

        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 1

      it "leaves global and entity subscriptions no-op cleanup unaffected" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new
        let entityName = Event.EntityName "TestEntity"
        let callback _msg = Task.yield unit

        -- A stream subscription with a real cleanup coexists with global/entity
        -- subscriptions; removing the global and entity subs must run NO cleanup
        -- (their onRemove is a no-op) and must not touch the stream cleanup.
        cleanupRuns <- ConcurrentVar.containing (0 :: Int)
        let cleanup = do
              cleanupRuns |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit
        store
          |> SubscriptionStore.addStreamSubscriptionWithCleanup entityName streamId Nothing cleanup callback
          |> discard
          |> Task.mapError toText

        globalId <- store |> SubscriptionStore.addGlobalSubscription callback |> Task.mapError toText
        entityId <- store |> SubscriptionStore.addEntitySubscription entityName callback |> Task.mapError toText

        globalResult <- store |> SubscriptionStore.removeSubscription globalId |> Task.asResult
        case globalResult of
          Err err -> Test.fail [fmt|removing a global subscription must be total: #{toText err}|]
          Ok _ -> Task.yield unit

        entityResult <- store |> SubscriptionStore.removeSubscription entityId |> Task.asResult
        case entityResult of
          Err err -> Test.fail [fmt|removing an entity subscription must be total: #{toText err}|]
          Ok _ -> Task.yield unit

        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        entitySubs <- store.entitySubscriptions |> ConcurrentVar.peek
        globalSubs |> Map.length |> shouldBe 0
        entitySubs |> Map.getOrElse entityName Map.empty |> Map.length |> shouldBe 0
        -- The stream subscription and its cleanup are untouched by the removals.
        runs <- ConcurrentVar.get cleanupRuns
        runs |> shouldBe 0
        streamSubs <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        streamSubs |> Map.length |> shouldBe 1


-- Helper function to create a test event
createTestEvent :: Task Error (Event Json.Value)
createTestEvent = do
  metadata <- EventMetadata.new
  streamId <- StreamId.new
  let entityName = Event.EntityName "TestEntity"
  let event = ItemAdded {entityId = def, itemId = def, amount = 100}
  Task.yield
    Event
      { entityName = entityName,
        streamId = streamId,
        event = Json.encode event,
        metadata = metadata
      }
