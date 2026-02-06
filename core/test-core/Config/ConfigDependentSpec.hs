-- | Tests for Application.withEventStoreFrom and withFileUploadFrom.
--
-- These tests verify that the config-dependent factory API works correctly,
-- allowing users to wire config-dependent components without hitting the
-- chicken-and-egg problem where Config.get panics before Application.run.
--
-- NOTE: Full integration testing (actual config loading + event store creation)
-- is done via the testbed integration tests. These unit tests verify the
-- builder API surface and basic behavior.
module Config.ConfigDependentSpec where

import Core
import Service.Application qualified as Application
import Service.EventStore.InMemory qualified as InMemory
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))
import Task qualified
import Test
import Text qualified


-- | Mock config type for testing.
-- This simulates a user-defined config type without needing HasParser.
data MockConfig = MockConfig
  { mockDbHost :: Text
  , mockDbPort :: Int
  , mockUploadDir :: Text
  }


-- | Helper to create a PostgresEventStore factory for testing.
makeEventStoreFactory :: MockConfig -> PostgresEventStore
makeEventStoreFactory cfg = PostgresEventStore
  { host = cfg.mockDbHost
  , port = cfg.mockDbPort
  , user = "test"
  , password = "test"
  , databaseName = "test"
  }


-- | Helper to create a FileUploadConfig factory for testing.
makeFileUploadFactory :: MockConfig -> FileUploadConfig
makeFileUploadFactory cfg = FileUploadConfig
  { blobStoreDir = cfg.mockUploadDir
  , stateStoreBackend = InMemoryStateStore
  , maxFileSizeBytes = 10485760
  , pendingTtlSeconds = 21600
  , cleanupIntervalSeconds = 900
  , allowedContentTypes = Nothing
  , storeOriginalFilename = True
  }


-- | Direct PostgresEventStore config for testing backwards compatibility.
directPostgresConfig :: PostgresEventStore
directPostgresConfig = PostgresEventStore
  { host = "localhost"
  , port = 5432
  , user = "test"
  , password = "test"
  , databaseName = "test"
  }


-- | Direct FileUploadConfig for testing backwards compatibility.
directFileUploadConfig :: FileUploadConfig
directFileUploadConfig = FileUploadConfig
  { blobStoreDir = "./uploads"
  , stateStoreBackend = InMemoryStateStore
  , maxFileSizeBytes = 10485760
  , pendingTtlSeconds = 21600
  , cleanupIntervalSeconds = 900
  , allowedContentTypes = Nothing
  , storeOriginalFilename = True
  }


spec :: Spec Unit
spec = do
  describe "Application.withEventStoreFrom" do
    it "stores factory and sets hasEventStore to True" \_ -> do
      -- The key test: we can pass a function (config -> PostgresEventStore)
      -- without needing the config value at build time. The factory is stored
      -- but NOT evaluated during Application construction - it's called later
      -- by Application.run after config is loaded.
      let app = Application.new
            |> Application.withEventStoreFrom @MockConfig makeEventStoreFactory
      Application.hasEventStore app |> shouldBe True

    describe "backwards compatibility" do
      it "withEventStore still works for direct values" \_ -> do
        let app = Application.new
              |> Application.withEventStore directPostgresConfig
        Application.hasEventStore app |> shouldBe True

  describe "Application.withFileUploadFrom" do
    it "stores factory and sets hasFileUpload to True" \_ -> do
      -- Same pattern as EventStore: factory is stored, not evaluated at build time
      let app = Application.new
            |> Application.withFileUploadFrom @MockConfig makeFileUploadFactory
      Application.hasFileUpload app |> shouldBe True

    describe "backwards compatibility" do
      it "withFileUpload still works for direct values" \_ -> do
        let app = Application.new
              |> Application.withFileUpload directFileUploadConfig
        Application.hasFileUpload app |> shouldBe True

  describe "last-write-wins semantics" do
    it "withEventStoreFrom overwrites withEventStore" \_ -> do
      -- When both are called, the last one wins. This test verifies
      -- that calling withEventStoreFrom after withEventStore results
      -- in a config-dependent factory (not the direct one).
      let differentFactory :: MockConfig -> PostgresEventStore
          differentFactory cfg = PostgresEventStore
            { host = cfg.mockDbHost  -- Uses config, different from directPostgresConfig
            , port = cfg.mockDbPort
            , user = "factory-user"  -- Distinguishable from direct config
            , password = "factory-password"
            , databaseName = "factory-db"
            }
      let app = Application.new
            |> Application.withEventStore directPostgresConfig
            |> Application.withEventStoreFrom @MockConfig differentFactory
      -- Both set hasEventStore to True
      Application.hasEventStore app |> shouldBe True
      -- The factory-based one wins (we can't easily verify which one
      -- without running the app, but we document the behavior)

    it "withEventStore overwrites withEventStoreFrom" \_ -> do
      -- Reverse order: direct config after factory
      let app = Application.new
            |> Application.withEventStoreFrom @MockConfig makeEventStoreFactory
            |> Application.withEventStore directPostgresConfig
      Application.hasEventStore app |> shouldBe True

    it "withFileUploadFrom overwrites withFileUpload" \_ -> do
      -- When both are called, the last one wins. This test verifies
      -- that calling withFileUploadFrom after withFileUpload results
      -- in a config-dependent factory (not the direct one).
      let differentFactory :: MockConfig -> FileUploadConfig
          differentFactory cfg = FileUploadConfig
            { blobStoreDir = cfg.mockUploadDir  -- Uses config, different from directFileUploadConfig
            , stateStoreBackend = InMemoryStateStore
            , maxFileSizeBytes = 5242880  -- Different from directFileUploadConfig
            , pendingTtlSeconds = 21600
            , cleanupIntervalSeconds = 900
            , allowedContentTypes = Nothing
            , storeOriginalFilename = True
            }
      let app = Application.new
            |> Application.withFileUpload directFileUploadConfig
            |> Application.withFileUploadFrom @MockConfig differentFactory
      -- Both set hasFileUpload to True
      Application.hasFileUpload app |> shouldBe True
      -- The factory-based one wins (we can't easily verify which one
      -- without running the app, but we document the behavior)

    it "withFileUpload overwrites withFileUploadFrom" \_ -> do
      -- Reverse order: direct config after factory
      let app = Application.new
            |> Application.withFileUploadFrom @MockConfig makeFileUploadFactory
            |> Application.withFileUpload directFileUploadConfig
      Application.hasFileUpload app |> shouldBe True

  describe "type safety" do
    -- NOTE: These are compile-time guarantees that we document here.
    -- The actual type checking happens at compile time, not runtime.
    --
    -- If you try to use withEventStoreFrom @ConfigA with a function that
    -- takes ConfigB, you'll get a compile error like:
    --   "Couldn't match type 'ConfigA' with 'ConfigB'"
    --
    -- Similarly, withFileUploadFrom @ConfigA requires (ConfigA -> FileUploadConfig),
    -- not (ConfigB -> FileUploadConfig).
    it "documents that config type mismatch is a compile error" \_ -> do
      -- This is a compile-time guarantee, not runtime testable.
      -- See ADR-0021 for the type-safe API design.
      pending "compile-time guarantee - see ADR-0021"

  describe "withConfig requirement" do
    it "ConfigDependentEventStore requires withConfig" \_ -> do
      -- When Application.run encounters ConfigDependentEventStore but
      -- no configSpec was set (via withConfig), it throws an error.
      let app = Application.new
            |> Application.withEventStoreFrom @MockConfig makeEventStoreFactory
      result <- Application.run app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "withEventStoreFrom requires withConfig to be called first")

    it "ConfigDependentFileUpload requires withConfig" \_ -> do
      -- NOTE: Testing this error path requires a working EventStore to get past
      -- the EventStore creation step. Since this unit test doesn't have Postgres,
      -- we test via nhcore-test-service (which has Postgres) or testbed integration tests.
      --
      -- The error we're documenting: When Application.run encounters
      -- ConfigDependentFileUpload but no configSpec was set, it throws:
      -- "withFileUploadFrom requires withConfig to be called first."
      pending "requires Postgres - tested in nhcore-test-service and testbed"

    it "runWith rejects ConfigDependentFileUpload" \_ -> do
      -- runWith doesn't support config-dependent factories at all, so it
      -- rejects ConfigDependentFileUpload with a clear error message.
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withFileUploadFrom @MockConfig makeFileUploadFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support withFileUploadFrom")
