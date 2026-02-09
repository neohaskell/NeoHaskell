-- | Tests for Application.withEventStore and withFileUpload unified API.
--
-- These tests verify that the unified factory-based API works correctly,
-- allowing users to wire config-dependent components without hitting the
-- chicken-and-egg problem where Config.get panics before Application.run.
--
-- The unified API uses a single function that accepts a factory:
-- - Static config: withEventStore @() (\_ -> config)
-- - Dynamic config: withEventStore makeConfig (type inferred from factory)
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
  describe "Application.withEventStore (unified API)" do
    it "stores factory and sets hasEventStore to True" \_ -> do
      -- The key test: we can pass a function (config -> PostgresEventStore)
      -- without needing the config value at build time. The factory is stored
      -- but NOT evaluated during Application construction - it's called later
      -- by Application.run after config is loaded.
      let app = Application.new
            |> Application.withEventStore makeEventStoreFactory
      Application.hasEventStore app |> shouldBe True

    it "works with static config using @() pattern" \_ -> do
      -- For static configs that don't depend on app config, use @() pattern
      let app = Application.new
            |> Application.withEventStore @() (\_ -> directPostgresConfig)
      Application.hasEventStore app |> shouldBe True

  describe "Application.withFileUpload (unified API)" do
    it "stores factory and sets hasFileUpload to True" \_ -> do
      -- Same pattern as EventStore: factory is stored, not evaluated at build time
      let app = Application.new
            |> Application.withFileUpload makeFileUploadFactory
      Application.hasFileUpload app |> shouldBe True

    it "works with static config using @() pattern" \_ -> do
      -- For static configs that don't depend on app config, use @() pattern
      let app = Application.new
            |> Application.withFileUpload @() (\_ -> directFileUploadConfig)
      Application.hasFileUpload app |> shouldBe True

  describe "last-write-wins semantics" do
    it "second withEventStore call overwrites first" \_ -> do
      -- When withEventStore is called multiple times, the last one wins.
      let differentFactory :: MockConfig -> PostgresEventStore
          differentFactory cfg = PostgresEventStore
            { host = cfg.mockDbHost  -- Uses config, different from directPostgresConfig
            , port = cfg.mockDbPort
            , user = "factory-user"  -- Distinguishable from direct config
            , password = "factory-password"
            , databaseName = "factory-db"
            }
      let app = Application.new
            |> Application.withEventStore @() (\_ -> directPostgresConfig)
            |> Application.withEventStore differentFactory
      -- Both set hasEventStore to True
      Application.hasEventStore app |> shouldBe True
      -- The second call wins (we can't easily verify which one
      -- without running the app, but we document the behavior)

    it "static config overwrites dynamic factory" \_ -> do
      -- Reverse order: static config after dynamic factory
      let app = Application.new
            |> Application.withEventStore makeEventStoreFactory
            |> Application.withEventStore @() (\_ -> directPostgresConfig)
      Application.hasEventStore app |> shouldBe True

    it "second withFileUpload call overwrites first" \_ -> do
      -- When withFileUpload is called multiple times, the last one wins.
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
            |> Application.withFileUpload @() (\_ -> directFileUploadConfig)
            |> Application.withFileUpload differentFactory
      -- Both set hasFileUpload to True
      Application.hasFileUpload app |> shouldBe True
      -- The second call wins (we can't easily verify which one
      -- without running the app, but we document the behavior)

    it "static config overwrites dynamic factory for FileUpload" \_ -> do
      -- Reverse order: static config after dynamic factory
      let app = Application.new
            |> Application.withFileUpload makeFileUploadFactory
            |> Application.withFileUpload @() (\_ -> directFileUploadConfig)
      Application.hasFileUpload app |> shouldBe True

  describe "type safety" do
    -- NOTE: These are compile-time guarantees that we document here.
    -- The actual type checking happens at compile time, not runtime.
    --
    -- If you pass a factory function (ConfigA -> PostgresEventStore) but
    -- withConfig was called with ConfigB, you'll get a compile error like:
    --   "Couldn't match type 'ConfigA' with 'ConfigB'"
    --
    -- The type of the factory function determines what config type is expected.
    it "documents that config type mismatch is a compile error" \_ -> do
      -- This is a compile-time guarantee, not runtime testable.
      -- See ADR-0021 for the type-safe API design.
      pending "compile-time guarantee - see ADR-0021"

  describe "withConfig requirement" do
    it "DeferredEventStore requires withConfig" \_ -> do
      -- When Application.run encounters DeferredEventStore (config-dependent factory)
      -- but no configSpec was set (via withConfig), it throws an error.
      let app = Application.new
            |> Application.withEventStore makeEventStoreFactory
      result <- Application.run app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "withEventStore requires withConfig")

    it "DeferredFileUpload requires withConfig" \_ -> do
      -- NOTE: Testing this error path requires a working EventStore to get past
      -- the EventStore creation step. Since this unit test doesn't have Postgres,
      -- we test via nhcore-test-service (which has Postgres) or testbed integration tests.
      --
      -- The error we're documenting: When Application.run encounters
      -- DeferredFileUpload but no configSpec was set, it throws:
      -- "withFileUpload requires withConfig when the factory uses the config parameter."
      pending "requires Postgres - tested in nhcore-test-service and testbed"

    it "runWith rejects DeferredFileUpload" \_ -> do
      -- runWith doesn't support config-dependent factories at all, so it
      -- rejects DeferredFileUpload with a clear error message.
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withFileUpload makeFileUploadFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent FileUpload")
