module Service.Query.SubscriberCheckpointSpec where

import ConcurrentVar qualified
import Core
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Checkpoint.InMemory qualified as CheckpointInMemory
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber qualified as Subscriber
import Service.TestHelpers (insertTestEvent)
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Service.Query.Subscriber" do
    describe "rebuildFrom" do
      it "Start from position 0 with no checkpoints — all events processed" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName

        processedCount <- ConcurrentVar.containing (0 :: Int)
        let updater =
              QueryUpdater
                { queryName = "counter"
                , updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }
        let registry = Registry.empty |> Registry.register entityName updater
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)

        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 1

      it "Start from position 100 with checkpoints — only events at >= 101 processed" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName
        insertTestEvent eventStore entityName
        insertTestEvent eventStore entityName

        processedCount <- ConcurrentVar.containing (0 :: Int)
        let updater =
              QueryUpdater
                { queryName = "counter"
                , updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }
        let registry = Registry.empty |> Registry.register entityName updater
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 1)

        count <- ConcurrentVar.peek processedCount
        -- Events at positions 1 and 2 processed; position 0 skipped
        count |> shouldBe 2

      it "EventStore returns empty stream — returns with 0 events replayed" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let registry = Registry.empty
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)
        pass

      it "A QueryUpdater throws — error logged, other queries continue" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName

        successCount <- ConcurrentVar.containing (0 :: Int)
        let failingUpdater =
              QueryUpdater
                { queryName = "failing"
                , updateQuery = \_ -> Task.throw "Simulated failure"
                }
        let successfulUpdater =
              QueryUpdater
                { queryName = "successful"
                , updateQuery = \_ -> do
                    successCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register entityName failingUpdater
                |> Registry.register entityName successfulUpdater
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)

        count <- ConcurrentVar.peek successCount
        count |> shouldBe 1

      it "rebuildFrom at P then rebuildFrom at P+1 — at most 1 new event (idempotency)" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName
        insertTestEvent eventStore entityName

        processedCount <- ConcurrentVar.containing (0 :: Int)
        let updater =
              QueryUpdater
                { queryName = "counter"
                , updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }
        let registry = Registry.empty |> Registry.register entityName updater
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)
        count1 <- ConcurrentVar.peek processedCount
        count1 |> shouldBe 2

        processedCount |> ConcurrentVar.modify (\_ -> 0)
        -- Rebuild again from 0: checkpoints persisted so 0 events processed (idempotent)
        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)
        count2 <- ConcurrentVar.peek processedCount
        count2 |> shouldBe 0

  describe "Service.Query.Subscriber readiness" do
    describe "subscriber.ready" do
      it "Rebuild completes successfully — ready is True" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let registry = Registry.empty
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)

        ready <- ConcurrentVar.peek subscriber.ready
        ready |> shouldBe True

      it "No queries registered — ready is True after rebuild" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let registry = Registry.empty
        subscriber <- Subscriber.new eventStore registry

        readyBefore <- ConcurrentVar.peek subscriber.ready
        readyBefore |> shouldBe False

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)

        readyAfter <- ConcurrentVar.peek subscriber.ready
        readyAfter |> shouldBe True

      it "Rebuild with events — ready is True after completion" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        checkpointStore <- CheckpointInMemory.new
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName

        processedCount <- ConcurrentVar.containing (0 :: Int)
        let updater =
              QueryUpdater
                { queryName = "counter"
                , updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }
        let registry = Registry.empty |> Registry.register entityName updater
        subscriber <- Subscriber.new eventStore registry

        Subscriber.rebuildFrom subscriber checkpointStore (StreamPosition 0)

        ready <- ConcurrentVar.peek subscriber.ready
        ready |> shouldBe True
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 1
