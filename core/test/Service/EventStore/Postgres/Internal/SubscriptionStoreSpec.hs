module Service.EventStore.Postgres.Internal.SubscriptionStoreSpec (spec) where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Map qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Postgres.Internal.SubscriptionStore (Error (..), SubscriptionStore (..))
import Service.EventStore.Postgres.Internal.SubscriptionStore qualified as SubscriptionStore
import Task qualified
import Test
import Test.Service.EventStore.Core (MyEvent (..))


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

        let callback _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 1

      it "adds multiple subscriptions to the same stream" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        let callback1 _msg = Task.yield unit
        let callback2 _msg = Task.yield unit
        let callback3 _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription streamId callback1 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback2 |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback3 |> discard |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Map.length |> shouldBe 3

      it "keeps subscriptions for different streams separate" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        streamId3 <- StreamId.new

        let callback _msg = Task.yield unit

        store |> SubscriptionStore.addStreamSubscription streamId1 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId1 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId2 callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId3 callback |> discard |> Task.mapError toText

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

        let callback _msg = Task.yield unit

        -- Add subscriptions to all streams concurrently
        let addToStream1 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription streamId1 callback |> discard |> Task.mapError toText)

        let addToStream2 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription streamId2 callback |> discard |> Task.mapError toText)

        let addToStream3 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach
                  (\_ -> store |> SubscriptionStore.addStreamSubscription streamId3 callback |> discard |> Task.mapError toText)

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
        Array.fromLinkedList ([1 .. count] :: [Int])
          |> Task.forEach
            (\_ -> store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText)

        streamSubs <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        streamSubs |> Map.length |> shouldBe count

    describe "Dispatch Functionality" do
      it "dispatches messages to stream subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        executionCount <- ConcurrentVar.containing (0 :: Int)

        let callback _msg = do
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add subscriptions
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText

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

        streamExecutionCount <- ConcurrentVar.containing (0 :: Int)
        globalExecutionCount <- ConcurrentVar.containing (0 :: Int)

        let streamCallback _msg = do
              streamExecutionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        let globalCallback _msg = do
              globalExecutionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add 2 stream subscriptions and 2 global subscriptions
        store |> SubscriptionStore.addStreamSubscription streamId streamCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId streamCallback |> discard |> Task.mapError toText
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

        successCount <- ConcurrentVar.containing (0 :: Int)

        let failingCallback _msg = Task.throw OtherError

        let successCallback _msg = do
              successCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add a mix of failing and successful callbacks
        store |> SubscriptionStore.addStreamSubscription streamId failingCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId successCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId failingCallback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId successCallback |> discard |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message - should not fail even though some callbacks fail
        store |> SubscriptionStore.dispatch streamId (event) |> Task.mapError toText

        -- Verify successful callbacks were executed despite failures
        count <- ConcurrentVar.get successCount
        count |> shouldBe 2

      it "executes callbacks in parallel (not serially)" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        executionCount <- ConcurrentVar.containing (0 :: Int)

        -- Each callback sleeps for 100ms
        let callback _msg = do
              AsyncTask.sleep 100
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield unit

        -- Add 5 callbacks
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> discard |> Task.mapError toText

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


-- Helper function to create a test event
createTestEvent :: Task Error (Event MyEvent)
createTestEvent = do
  metadata <- EventMetadata.new
  streamId <- StreamId.new
  let entityName = Event.EntityName "TestEntity"
  let event = MyEvent
  Task.yield
    Event
      { entityName = entityName,
        streamId = streamId,
        event = event,
        metadata = metadata
      }
