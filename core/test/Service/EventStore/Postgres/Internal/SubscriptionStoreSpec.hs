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
import Service.EventStore.Core (ReadStreamMessage (..))
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

        globalSubs |> Array.length |> shouldBe 0
        streamSubs |> Map.size |> shouldBe 0

      it "adds a global subscription" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback _msg = Task.yield ()

        store |> SubscriptionStore.addGlobalSubscription callback |> Task.mapError toText

        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Array.length |> shouldBe 1

      it "adds multiple global subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback1 _msg = Task.yield ()
        let callback2 _msg = Task.yield ()
        let callback3 _msg = Task.yield ()

        store |> SubscriptionStore.addGlobalSubscription callback1 |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription callback2 |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription callback3 |> Task.mapError toText

        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Array.length |> shouldBe 3

      it "adds a stream subscription" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        let callback _msg = Task.yield ()

        store |> SubscriptionStore.addStreamSubscription streamId callback |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Array.length |> shouldBe 1

      it "adds multiple subscriptions to the same stream" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        let callback1 _msg = Task.yield ()
        let callback2 _msg = Task.yield ()
        let callback3 _msg = Task.yield ()

        store |> SubscriptionStore.addStreamSubscription streamId callback1 |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback2 |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback3 |> Task.mapError toText

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Array.length |> shouldBe 3

      it "keeps subscriptions for different streams separate" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        streamId3 <- StreamId.new

        let callback _msg = Task.yield ()

        store |> SubscriptionStore.addStreamSubscription streamId1 callback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId1 callback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId2 callback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId3 callback |> Task.mapError toText

        subs1 <- store |> SubscriptionStore.getStreamSubscriptions streamId1 |> Task.mapError toText
        subs2 <- store |> SubscriptionStore.getStreamSubscriptions streamId2 |> Task.mapError toText
        subs3 <- store |> SubscriptionStore.getStreamSubscriptions streamId3 |> Task.mapError toText

        subs1 |> Array.length |> shouldBe 2
        subs2 |> Array.length |> shouldBe 1
        subs3 |> Array.length |> shouldBe 1

      it "returns empty array for stream with no subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        subscriptions <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        subscriptions |> Array.length |> shouldBe 0

    describe "Concurrent Modifications" do
      it "handles concurrent additions to different streams" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId1 <- StreamId.new
        streamId2 <- StreamId.new
        streamId3 <- StreamId.new

        let callback _msg = Task.yield ()

        -- Add subscriptions to all streams concurrently
        let addToStream1 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach (\_ -> store |> SubscriptionStore.addStreamSubscription streamId1 callback |> Task.mapError toText)

        let addToStream2 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach (\_ -> store |> SubscriptionStore.addStreamSubscription streamId2 callback |> Task.mapError toText)

        let addToStream3 = do
              Array.fromLinkedList ([1 .. 5] :: [Int])
                |> Task.forEach (\_ -> store |> SubscriptionStore.addStreamSubscription streamId3 callback |> Task.mapError toText)

        (_, (_, _)) <- AsyncTask.runConcurrently (addToStream1, AsyncTask.runConcurrently (addToStream2, addToStream3))

        -- Verify each stream has the correct number of subscriptions
        subs1 <- store |> SubscriptionStore.getStreamSubscriptions streamId1 |> Task.mapError toText
        subs2 <- store |> SubscriptionStore.getStreamSubscriptions streamId2 |> Task.mapError toText
        subs3 <- store |> SubscriptionStore.getStreamSubscriptions streamId3 |> Task.mapError toText

        subs1 |> Array.length |> shouldBe 5
        subs2 |> Array.length |> shouldBe 5
        subs3 |> Array.length |> shouldBe 5

      it "handles large numbers of subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText

        let callback _msg = Task.yield ()
        let count = 100

        -- Add many global subscriptions
        Array.fromLinkedList ([1 .. count] :: [Int])
          |> Task.forEach (\_ -> store |> SubscriptionStore.addGlobalSubscription callback |> Task.mapError toText)

        -- Verify count
        globalSubs <- store.globalSubscriptions |> ConcurrentVar.peek
        globalSubs |> Array.length |> shouldBe count

        -- Add many stream subscriptions
        streamId <- StreamId.new
        Array.fromLinkedList ([1 .. count] :: [Int])
          |> Task.forEach (\_ -> store |> SubscriptionStore.addStreamSubscription streamId callback |> Task.mapError toText)

        streamSubs <- store |> SubscriptionStore.getStreamSubscriptions streamId |> Task.mapError toText
        streamSubs |> Array.length |> shouldBe count

    describe "Dispatch Functionality" do
      it "dispatches messages to stream subscriptions" \_ -> do
        store <- SubscriptionStore.new |> Task.mapError toText
        streamId <- StreamId.new

        executionCount <- ConcurrentVar.containing (0 :: Int)

        let callback _msg = do
              executionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield ()

        -- Add subscriptions
        store |> SubscriptionStore.addStreamSubscription streamId callback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId callback |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message
        store |> SubscriptionStore.dispatch streamId (StreamEvent event) |> Task.mapError toText

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
              Task.yield ()

        let globalCallback _msg = do
              globalExecutionCount |> ConcurrentVar.modify (\n -> n + 1)
              Task.yield ()

        -- Add 2 stream subscriptions and 2 global subscriptions
        store |> SubscriptionStore.addStreamSubscription streamId streamCallback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId streamCallback |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription globalCallback |> Task.mapError toText
        store |> SubscriptionStore.addGlobalSubscription globalCallback |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message
        store |> SubscriptionStore.dispatch streamId (StreamEvent event) |> Task.mapError toText

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
              Task.yield ()

        -- Add a mix of failing and successful callbacks
        store |> SubscriptionStore.addStreamSubscription streamId failingCallback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId successCallback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId failingCallback |> Task.mapError toText
        store |> SubscriptionStore.addStreamSubscription streamId successCallback |> Task.mapError toText

        -- Create a test event
        event <- createTestEvent |> Task.mapError toText

        -- Dispatch the message - should not fail even though some callbacks fail
        store |> SubscriptionStore.dispatch streamId (StreamEvent event) |> Task.mapError toText

        -- Verify successful callbacks were executed despite failures
        count <- ConcurrentVar.get successCount
        count |> shouldBe 2


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
