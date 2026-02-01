module Service.Query.SubscriberSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Limit (..), collectAllEvents)
import Service.EventStore.InMemory qualified as InMemory
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber qualified as Subscriber
import Service.TestHelpers (insertTestEvent)
import Stream qualified
import Task qualified
import Test


-- | Wait until a ConcurrentVar contains the expected value, or timeout.
-- Polls every 10ms up to the specified timeout.
waitUntilCount ::
  ConcurrentVar Int ->
  Int ->
  Int ->
  Task Text Unit
waitUntilCount var expectedValue timeoutMs = do
  let pollIntervalMs = 10
  let maxAttempts = timeoutMs / pollIntervalMs
  let waitLoop currentAttempt = do
        currentValue <- ConcurrentVar.peek var
        if currentValue == expectedValue
          then Task.yield unit
          else do
            if currentAttempt >= maxAttempts
              then Task.throw [fmt|Timeout waiting for expected count: expected #{toText expectedValue}, got #{toText currentValue}|]
              else do
                AsyncTask.sleep pollIntervalMs
                  |> Task.mapError (\_ -> "sleep error" :: Text)
                waitLoop (currentAttempt + 1)
  waitLoop 0


spec :: Spec Unit
spec = do
  describe "QuerySubscriber" do
    describe "new" do
      it "creates a subscriber with empty last processed position" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let registry = Registry.empty
        _subscriber <- Subscriber.new eventStore registry
        -- Verify subscriber was created (we can't easily inspect internal state,
        -- but we can verify it doesn't fail)
        pass

    describe "rebuildAll" do
      it "does nothing when event store is empty" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let registry = Registry.empty
        subscriber <- Subscriber.new eventStore registry

        -- Should complete without error
        Subscriber.rebuildAll subscriber
        pass

      it "readAllEventsForwardFrom returns inserted events" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        -- Insert 1 event
        insertTestEvent eventStore entityName

        -- Try to read it back directly
        stream <- eventStore.readAllEventsForwardFrom (StreamPosition 0) (Limit 100)
          |> Task.mapError toText
        events <- Stream.toArray stream
        let allEvents = collectAllEvents events

        Array.length allEvents |> shouldBe 1

      it "processes one existing event through registered updaters" \_ -> do
        -- Setup: Create event store with one event
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        -- Insert 1 event
        insertTestEvent eventStore entityName

        -- Create a counter to track how many events were processed
        processedCount <- ConcurrentVar.containing (0 :: Int)

        let updater =
              QueryUpdater
                { queryName = "counter",
                  updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register entityName updater

        subscriber <- Subscriber.new eventStore registry

        -- Run rebuild
        Subscriber.rebuildAll subscriber

        -- Verify 1 event was processed
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 1

      it "only processes events for registered entity types" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let registeredEntity = EntityName "RegisteredEntity"
        let unregisteredEntity = EntityName "UnregisteredEntity"

        -- Insert events for both entity types
        insertTestEvent eventStore registeredEntity
        insertTestEvent eventStore unregisteredEntity
        insertTestEvent eventStore registeredEntity

        processedCount <- ConcurrentVar.containing (0 :: Int)

        let updater =
              QueryUpdater
                { queryName = "counter",
                  updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        -- Only register updater for one entity type
        let registry =
              Registry.empty
                |> Registry.register registeredEntity updater

        subscriber <- Subscriber.new eventStore registry
        Subscriber.rebuildAll subscriber

        -- Should only process the 2 events for registered entity
        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 2

      it "calls multiple updaters for the same entity" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName

        updater1Count <- ConcurrentVar.containing (0 :: Int)
        updater2Count <- ConcurrentVar.containing (0 :: Int)

        let updater1 =
              QueryUpdater
                { queryName = "updater1",
                  updateQuery = \_ -> do
                    updater1Count |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let updater2 =
              QueryUpdater
                { queryName = "updater2",
                  updateQuery = \_ -> do
                    updater2Count |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register entityName updater1
                |> Registry.register entityName updater2

        subscriber <- Subscriber.new eventStore registry
        Subscriber.rebuildAll subscriber

        count1 <- ConcurrentVar.peek updater1Count
        count2 <- ConcurrentVar.peek updater2Count
        count1 |> shouldBe 1
        count2 |> shouldBe 1

      it "continues processing even if an updater fails" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        insertTestEvent eventStore entityName
        insertTestEvent eventStore entityName

        successCount <- ConcurrentVar.containing (0 :: Int)

        let failingUpdater =
              QueryUpdater
                { queryName = "failing",
                  updateQuery = \_ -> Task.throw "Simulated failure"
                }

        let successfulUpdater =
              QueryUpdater
                { queryName = "successful",
                  updateQuery = \_ -> do
                    successCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register entityName failingUpdater
                |> Registry.register entityName successfulUpdater

        subscriber <- Subscriber.new eventStore registry
        Subscriber.rebuildAll subscriber

        -- The successful updater should still be called for both events
        count <- ConcurrentVar.peek successCount
        count |> shouldBe 2

    describe "start" do
      it "receives events inserted after subscription starts" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        processedCount <- ConcurrentVar.containing (0 :: Int)

        let updater =
              QueryUpdater
                { queryName = "counter",
                  updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register entityName updater

        subscriber <- Subscriber.new eventStore registry

        -- Start subscription (no rebuild needed since store is empty)
        Subscriber.start subscriber

        -- Insert event after subscription started
        insertTestEvent eventStore entityName

        -- Wait for event to be processed (with timeout)
        waitUntilCount processedCount 1 5000

        count <- ConcurrentVar.peek processedCount
        count |> shouldBe 1

      it "starts from correct position after rebuild" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let entityName = EntityName "TestEntity"

        -- Insert initial events
        insertTestEvent eventStore entityName
        insertTestEvent eventStore entityName

        processedCount <- ConcurrentVar.containing (0 :: Int)

        let updater =
              QueryUpdater
                { queryName = "counter",
                  updateQuery = \_ -> do
                    processedCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register entityName updater

        subscriber <- Subscriber.new eventStore registry

        -- Rebuild first (processes 2 events)
        Subscriber.rebuildAll subscriber

        rebuildCount <- ConcurrentVar.peek processedCount
        rebuildCount |> shouldBe 2

        -- Reset counter
        processedCount |> ConcurrentVar.modify (\_ -> 0)

        -- Start live subscription
        Subscriber.start subscriber

        -- Insert new event
        insertTestEvent eventStore entityName

        -- Wait for new event to be processed (with timeout)
        waitUntilCount processedCount 1 5000

        -- Should only process the new event (not re-process old ones)
        liveCount <- ConcurrentVar.peek processedCount
        liveCount |> shouldBe 1
