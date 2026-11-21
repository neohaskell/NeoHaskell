module Service.ApplicationSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Directory qualified
import Map qualified
import "nhcore" Path qualified
import Result qualified
import Service.Application (Application (..), ServiceRunner (..))
import Service.Application qualified as Application
import Service.Application.Transports qualified as Transports
import Service.AccessControl qualified as AccessControl
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.FileUpload.BlobStore (BlobStore (..), BlobStoreConfig (..), BlobStoreError (..))
import Service.FileUpload.BlobStore.Local (LocalBlobStoreConfig (..))
import Service.FileUpload.Core (FileStateStoreBackend (..), FileUploadConfig (..))
import Service.MockTransport qualified as MockTransport
import Service.Query.Pagination (QueryPageRequest (..))
import Service.Query.Registry (QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.Query.Subscriber qualified as Subscriber
import Service.TestHelpers (insertTestEvent)
import System.IO qualified as GhcIO
import Task qualified
import Test
import Text qualified


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Service.Application" do
    describe "query readiness guard" do
      it "rejects a page captured during rebuilding even when replay becomes ready before return" \_ -> do
        readiness <- ConcurrentVar.containing (Just Subscriber.Rebuilding)
        let readReadiness = ConcurrentVar.peek readiness
        let incompletePage = "{\"items\":[],\"total\":0,\"hasMore\":false,\"effectiveLimit\":1}"
        let completePage = "{\"items\":[\"protected\"],\"total\":1,\"hasMore\":false,\"effectiveLimit\":1}"
        page <- ConcurrentVar.containing incompletePage
        let handler _ _ _ = do
              snapshot <- ConcurrentVar.peek page
              page |> ConcurrentVar.modify (\_ -> completePage)
              readiness |> ConcurrentVar.modify (\_ -> Just Subscriber.Ready)
              Task.yield snapshot
        let guarded = Transports.guardQueryEndpoint readReadiness handler
        let pageRequest = QueryPageRequest {limit = 1, offset = 0}
        result <- guarded Nothing Nothing pageRequest |> Task.asResult
        result |> shouldBe (Err (AccessControl.StorageError "Query projection unavailable"))
        freshResult <- guarded Nothing Nothing pageRequest |> Task.asResult
        freshResult |> shouldBe (Ok completePage)

      it "preserves an authorization error when readiness cannot be read" \_ -> do
        let unavailable = Task.throw (AccessControl.StorageError "Query readiness unavailable")
        let handler _ _ _ = Task.throw (AccessControl.AuthorizationError AccessControl.Unauthenticated)
        let guarded = Transports.guardQueryEndpoint unavailable handler
        let pageRequest = QueryPageRequest {limit = 1, offset = 0}
        result <- guarded Nothing Nothing pageRequest |> Task.asResult
        result |> shouldBe (Err (AccessControl.AuthorizationError AccessControl.Unauthenticated))

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
        let app =
              Application.new
                |> Application.withServiceRunner mockServiceRunner
        Application.hasServiceRunners app |> shouldBe True

      it "accumulates multiple service runners" \_ -> do
        let app =
              Application.new
                |> Application.withServiceRunner mockServiceRunner
                |> Application.withServiceRunner mockServiceRunner
        Application.serviceRunnerCount app |> shouldBe 2

    describe "run" do
      it "throws error when no EventStore is configured" \_ -> do
        let app = Application.new
        result <- Application.run app |> Task.asResult
        case result of
          Ok _ -> fail "Expected error but got Ok"
          Err err -> err |> shouldBe "No EventStore configured. Use withEventStore."

      it "sets stdout and stderr to LineBuffering" \_ -> do
        -- Set to block buffering first to ensure run changes it
        Task.fromIO (GhcIO.hSetBuffering GhcIO.stdout (GhcIO.BlockBuffering Nothing))
        Task.fromIO (GhcIO.hSetBuffering GhcIO.stderr (GhcIO.BlockBuffering Nothing))

        -- Run the app (will fail because no EventStore, but buffering should be set first)
        _ <- Application.run Application.new |> Task.asResult

        -- Verify buffering was set before the error
        stdoutBuffering <- Task.fromIO (GhcIO.hGetBuffering GhcIO.stdout)
        stderrBuffering <- Task.fromIO (GhcIO.hGetBuffering GhcIO.stderr)
        stdoutBuffering |> shouldBe GhcIO.LineBuffering
        stderrBuffering |> shouldBe GhcIO.LineBuffering

    describe "runWith" do
      it "collects endpoints from service runners" \_ -> do
        -- With the new architecture, Application collects endpoints from services
        -- and runs transports itself. This test verifies the basic flow works.
        eventStore <- InMemory.new |> Task.mapError toText

        let app =
              Application.new
                |> Application.withServiceRunner mockServiceRunner

        -- runWith should complete without error even with mock runners
        Application.runWith eventStore app

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

    describe "file upload backend selection" do
      it "creates the selected blob store once per startup" \_ -> do
        count <- ConcurrentVar.containing (0 :: Int)
        let customBackend = CustomBlobStoreConfig \_ -> do
              ConcurrentVar.modify (\n -> n + 1) count
              Task.yield unusedBlobStore
        eventStore <- InMemory.new |> Task.mapError toText
        let app = Application.new
              |> Application.withFileUpload @() (\_ -> uploadConfig customBackend)
        Application.hasFileUpload app |> shouldBe True
        [(), ()] |> Task.forEach (\_ -> do
          before <- ConcurrentVar.peek count
          Application.runWith eventStore app
          after <- ConcurrentVar.peek count
          after |> shouldBe (before + 1))
        ConcurrentVar.peek count |> Task.andThen (shouldBe 2)

      it "validates every shared limit before creating the blob store" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let config = uploadConfig (CustomBlobStoreConfig (\_ -> panic "blob store created before validation"))
        let invalid =
              [ (config {maxFileSizeBytes = 0}, "maxFileSizeBytes")
              , (config {pendingTtlSeconds = 0}, "pendingTtlSeconds")
              , (config {cleanupIntervalSeconds = 0}, "cleanupIntervalSeconds")
              , (config {cleanupIntervalSeconds = 60}, "must be less")
              , (config {cleanupIntervalSeconds = 61}, "must be less")
              ]
        invalid |> Task.forEach (\(fileConfig, expected) -> do
          let app = Application.new
                |> Application.withFileUpload @() (\_ -> fileConfig)
          Application.hasFileUpload app |> shouldBe True
          result <- Application.runWith eventStore app |> Task.asResult
          case result of
            Err message -> Text.contains expected message |> shouldBe True
            Ok _ -> fail "Invalid upload limits accepted")

      it "propagates blob store failure without creating Local storage" \_ -> do
        Directory.withTemporaryFolder toText \parentDir -> do
          let uploadDir = parentDir |> Path.append [Path.path|uploads|]
          eventStore <- InMemory.new |> Task.mapError toText
          let app = Application.new
                |> Application.withFileUpload @() (\_ -> uploadConfig (CustomBlobStoreConfig (\_ -> Task.throw "custom backend unavailable")))
          result <- Application.runWith eventStore app |> Task.asResult
          result |> shouldBe (Err "custom backend unavailable")
          walked <- Directory.walk uploadDir |> Task.asResult
          Result.isErr walked |> shouldBe True

      it "lets the last builder select Local or custom storage" \_ -> do
        Directory.withTemporaryFolder toText \parentDir -> do
          let uploadDir = parentDir |> Path.append [Path.path|uploads|]
          eventStore <- InMemory.new |> Task.mapError toText
          let localConfig = uploadConfig LocalBlobStoreConfig {rootDir = uploadDir}
          let custom = Application.new
                |> Application.withFileUpload @() (\_ -> localConfig)
                |> Application.withFileUpload @() (\_ -> uploadConfig (CustomBlobStoreConfig (\_ -> Task.yield unusedBlobStore)))
          Application.runWith eventStore custom
          customWalked <- Directory.walk uploadDir |> Task.asResult
          Result.isErr customWalked |> shouldBe True
          let local = Application.new
                |> Application.withFileUpload @() (\_ -> uploadConfig (CustomBlobStoreConfig (\_ -> panic "replaced backend created")))
                |> Application.withFileUpload @() (\_ -> localConfig)
          Application.runWith eventStore local
          localWalked <- Directory.walk uploadDir |> Task.mapError toText
          localWalked |> shouldBe []

      it "rejects an empty or blank Local directory" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText
        let emptyApp = Application.new
              |> Application.withFileUpload @() (\_ -> uploadConfig LocalBlobStoreConfig {rootDir = [Path.path||]})
        emptyResult <- Application.runWith eventStore emptyApp |> Task.asResult
        emptyResult |> shouldBe (Err "Local blob store rootDir cannot be empty")
        let blankApp = Application.new
              |> Application.withFileUpload @() (\_ -> uploadConfig LocalBlobStoreConfig {rootDir = [Path.path|   |]})
        blankResult <- Application.runWith eventStore blankApp |> Task.asResult
        blankResult |> shouldBe (Err "Local blob store rootDir cannot be empty")

    describe "isEmpty with service runners" do
      it "returns False when service runners are added" \_ -> do
        let app =
              Application.new
                |> Application.withServiceRunner mockServiceRunner
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

      it "passes transports to service endpoint builders" \_ -> do
        -- Track whether the expected transport was found in the transports map
        transportFoundRef <- ConcurrentVar.containing False

        eventStore <- InMemory.new |> Task.mapError toText

        -- Create a service runner that inspects the transports parameter
        let runner =
              ServiceRunner
                { getEndpointsByTransport = \_ transports -> do
                    -- Check if MockTransport is present by looking for its name
                    let hasMockTransport = transports |> Map.contains "MockTransport"
                    transportFoundRef |> ConcurrentVar.modify (\_ -> hasMockTransport)
                    Task.yield (Map.empty, Map.empty, Map.empty)
                }

        let app =
              Application.new
                |> Application.withTransport MockTransport.server
                |> Application.withServiceRunner runner

        Application.runWith eventStore app

        found <- ConcurrentVar.peek transportFoundRef
        found |> shouldBe True


-- | Test helper to set the QueryRegistry for an Application.
-- This is only for testing - prefer Application.withQuery in production code.
withQueryRegistry ::
  Registry.QueryRegistry ->
  Application ->
  Application
withQueryRegistry registry app =
  app {queryRegistry = registry}


-- | Create a mock ServiceRunner for testing.
-- Returns empty endpoints since tests don't need actual command handlers.
mockServiceRunner :: ServiceRunner
mockServiceRunner =
  ServiceRunner
    { getEndpointsByTransport = \_ _ -> Task.yield (Map.empty, Map.empty, Map.empty)
    }


newtype CustomBlobStoreConfig = CustomBlobStoreConfig (() -> Task Text BlobStore)


instance BlobStoreConfig CustomBlobStoreConfig where
  createBlobStore (CustomBlobStoreConfig create) = create ()


uploadConfig :: forall blobStore. blobStore -> FileUploadConfig blobStore
uploadConfig blobStore = FileUploadConfig
  { blobStore
  , stateStoreBackend = InMemoryStateStore
  , maxFileSizeBytes = 4096
  , pendingTtlSeconds = 60
  , cleanupIntervalSeconds = 59
  , allowedContentTypes = Nothing
  , storeOriginalFilename = True
  }


unusedBlobStore :: BlobStore
unusedBlobStore = BlobStore
  { store = \_ _ -> Task.throw (StorageError "unexpected store")
  , retrieve = \_ -> Task.throw (StorageError "unexpected retrieve")
  , delete = \_ -> Task.throw (StorageError "unexpected delete")
  , exists = \_ -> Task.throw (StorageError "unexpected exists")
  }
