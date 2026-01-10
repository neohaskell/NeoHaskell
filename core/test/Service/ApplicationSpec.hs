module Service.ApplicationSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Service.Application (Application (..), ServiceRunner (..))
import Service.Application qualified as Application
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.MockTransport qualified as MockTransport
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.TestHelpers (insertTestEvent)
import Task qualified
import Test


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Application" do
    describe "new" do
      it "creates an empty application" \_ -> do
        let app = Application.new
        Application.isEmpty app |> shouldBe True

    describe "withQueryRegistry" do
      it "adds a query registry to the application" \_ -> do
        -- Create a registry with one updater
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> withQueryRegistry registry
        Application.hasQueryRegistry app |> shouldBe True

      it "replaces existing registry" \_ -> do
        let updater1 =
              QueryUpdater
                { queryName = "Query1",
                  updateQuery = \_ -> Task.yield unit
                }
        let updater2 =
              QueryUpdater
                { queryName = "Query2",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry1 =
              Registry.empty
                |> Registry.register (EntityName "Entity1") updater1
        let registry2 =
              Registry.empty
                |> Registry.register (EntityName "Entity2") updater2

        let app =
              Application.new
                |> withQueryRegistry registry1
                |> withQueryRegistry registry2

        -- Should have registry2's updater (Entity2), not registry1's (Entity1)
        let entity1Updaters = app.queryRegistry |> Registry.getUpdatersForEntity (EntityName "Entity1")
        let entity2Updaters = app.queryRegistry |> Registry.getUpdatersForEntity (EntityName "Entity2")
        Array.isEmpty entity1Updaters |> shouldBe True
        Array.isEmpty entity2Updaters |> shouldBe False

    describe "isEmpty" do
      it "returns True for new application" \_ -> do
        Application.isEmpty Application.new |> shouldBe True

      it "returns False after adding registry" \_ -> do
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> withQueryRegistry registry
        Application.isEmpty app |> shouldBe False

    describe "hasQueryRegistry" do
      it "returns False for empty application" \_ -> do
        Application.hasQueryRegistry Application.new |> shouldBe False

      it "returns True when registry has updaters" \_ -> do
        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> Task.yield unit
                }
        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater
        let app =
              Application.new
                |> withQueryRegistry registry
        Application.hasQueryRegistry app |> shouldBe True

    describe "withServiceRunner" do
      it "adds a service runner to the application" \_ -> do
        let runner =
              ServiceRunner
                { runWithEventStore = \_ _ _ -> Task.yield unit
                }
        let app =
              Application.new
                |> Application.withServiceRunner runner
        Application.hasServiceRunners app |> shouldBe True

      it "accumulates multiple service runners" \_ -> do
        let runner1 = ServiceRunner {runWithEventStore = \_ _ _ -> Task.yield unit}
        let runner2 = ServiceRunner {runWithEventStore = \_ _ _ -> Task.yield unit}
        let app =
              Application.new
                |> Application.withServiceRunner runner1
                |> Application.withServiceRunner runner2
        Application.serviceRunnerCount app |> shouldBe 2

    describe "run" do
      it "throws error when no EventStore is configured" \_ -> do
        let app = Application.new
        result <- Application.run app |> Task.asResult
        case result of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "No EventStore configured. Use withEventStore to configure one."

    describe "runWith" do
      it "passes event store to service runners" \_ -> do
        -- Track whether service runner was called with event store
        calledRef <- ConcurrentVar.containing False

        let runner =
              ServiceRunner
                { runWithEventStore = \_ _ _ -> do
                    calledRef |> ConcurrentVar.modify (\_ -> True)
                    Task.yield unit
                }

        eventStore <- InMemory.new |> Task.mapError toText

        let app =
              Application.new
                |> Application.withServiceRunner runner

        Application.runWith eventStore app

        called <- ConcurrentVar.peek calledRef
        called |> shouldBe True

      it "rebuilds queries before starting services" \_ -> do
        -- Create event store with existing events
        eventStore <- InMemory.new |> Task.mapError toText

        -- Insert an event BEFORE running the app
        insertTestEvent eventStore (EntityName "TestEntity")

        -- Track query rebuild
        rebuildCalled <- ConcurrentVar.containing False

        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> do
                    rebuildCalled |> ConcurrentVar.modify (\_ -> True)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater

        let app =
              Application.new
                |> withQueryRegistry registry

        Application.runWith eventStore app

        rebuilt <- ConcurrentVar.peek rebuildCalled
        rebuilt |> shouldBe True

      it "starts query subscriber after rebuild" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText

        -- Track events processed by subscriber (live events after startup)
        liveEventCount <- ConcurrentVar.containing (0 :: Int)

        let updater =
              QueryUpdater
                { queryName = "TestQuery",
                  updateQuery = \_ -> do
                    liveEventCount |> ConcurrentVar.modify (\n -> n + 1)
                    Task.yield unit
                }

        let registry =
              Registry.empty
                |> Registry.register (EntityName "TestEntity") updater

        let app =
              Application.new
                |> withQueryRegistry registry

        -- Run app in background
        Application.runWithAsync eventStore app

        -- Give subscriber time to start
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Insert event AFTER app started
        insertTestEvent eventStore (EntityName "TestEntity")

        -- Give subscriber time to process
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        count <- ConcurrentVar.peek liveEventCount
        count |> shouldBe 1

    describe "isEmpty with service runners" do
      it "returns False when service runners are added" \_ -> do
        let runner = ServiceRunner {runWithEventStore = \_ _ _ -> Task.yield unit}
        let app =
              Application.new
                |> Application.withServiceRunner runner
        Application.isEmpty app |> shouldBe False

    describe "withTransport" do
      it "adds a transport to the application" \_ -> do
        let app =
              Application.new
                |> Application.withTransport MockTransport.server
        Application.hasTransports app |> shouldBe True

      it "accumulates multiple transports" \_ -> do
        let app =
              Application.new
                |> Application.withTransport MockTransport.server
                |> Application.withTransport MockTransport.server2
        Application.transportCount app |> shouldBe 2

      it "makes transports available to services" \_ -> do
        -- Track whether transport was used
        transportUsedRef <- ConcurrentVar.containing False

        eventStore <- InMemory.new |> Task.mapError toText

        -- Create a service runner that checks for transport
        let runner =
              ServiceRunner
                { runWithEventStore = \_ _ _ -> do
                    transportUsedRef |> ConcurrentVar.modify (\_ -> True)
                    Task.yield unit
                }

        let app =
              Application.new
                |> Application.withTransport MockTransport.server
                |> Application.withServiceRunner runner

        Application.runWith eventStore app

        used <- ConcurrentVar.peek transportUsedRef
        used |> shouldBe True


-- | Test helper to set the QueryRegistry for an Application.
-- This is only for testing - prefer Application.withQuery in production code.
withQueryRegistry ::
  Registry.QueryRegistry ->
  Application ->
  Application
withQueryRegistry registry (Application eventStoreCreator queryObjectStoreConfig queryDefinitions _ serviceRunners transports queryEndpoints) =
  Application eventStoreCreator queryObjectStoreConfig queryDefinitions registry serviceRunners transports queryEndpoints
