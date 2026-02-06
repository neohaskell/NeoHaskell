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
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))
import Test


-- | Mock config type for testing.
-- This simulates a user-defined config type without needing HasParser.
data MockConfig = MockConfig
  { mockDbHost :: Text
  , mockDbPort :: Int
  , mockUploadDir :: Text
  }


spec :: Spec Unit
spec = do
  describe "Application.withEventStoreFrom" do
    describe "builder pattern" do
      it "can be called with a config factory function" \_ -> do
        -- The key test: we can pass a function (config -> PostgresEventStore)
        -- without needing the config value at build time
        let makeEventStore :: MockConfig -> PostgresEventStore
            makeEventStore cfg = PostgresEventStore
              { host = cfg.mockDbHost
              , port = cfg.mockDbPort
              , user = "test"
              , password = "test"
              , databaseName = "test"
              }
        let app = Application.new
              |> Application.withEventStoreFrom @MockConfig makeEventStore
        -- Verify the application was modified (has event store factory)
        Application.hasEventStore app |> shouldBe True

      it "returns True for hasEventStore after withEventStoreFrom" \_ -> do
        let factory :: MockConfig -> PostgresEventStore
            factory _ = PostgresEventStore
              { host = "localhost"
              , port = 5432
              , user = "test"
              , password = "test"
              , databaseName = "test"
              }
        let app = Application.new
              |> Application.withEventStoreFrom @MockConfig factory
        Application.hasEventStore app |> shouldBe True

      it "does not evaluate the factory function at build time" \_ -> do
        -- This test documents the key behavior: the factory is NOT called
        -- during Application construction. It's stored and called later
        -- by Application.run after config is loaded.
        --
        -- We can't directly test "not evaluated" without unsafePerformIO,
        -- but we verify the pattern compiles and doesn't panic.
        let factory :: MockConfig -> PostgresEventStore
            factory cfg = PostgresEventStore
              { host = cfg.mockDbHost  -- Would panic if evaluated without config
              , port = cfg.mockDbPort
              , user = "test"
              , password = "test"
              , databaseName = "test"
              }
        -- Building the app should NOT call the factory
        let app = Application.new
              |> Application.withEventStoreFrom @MockConfig factory
        -- Just verify we can inspect the app without panicking
        Application.hasEventStore app |> shouldBe True

    describe "backwards compatibility" do
      it "withEventStore still works for direct values" \_ -> do
        -- The original withEventStore API should still work
        let directConfig = PostgresEventStore
              { host = "localhost"
              , port = 5432
              , user = "test"
              , password = "test"
              , databaseName = "test"
              }
        let app = Application.new
              |> Application.withEventStore directConfig
        Application.hasEventStore app |> shouldBe True

  describe "Application.withFileUploadFrom" do
    describe "builder pattern" do
      it "can be called with a config factory function" \_ -> do
        let makeFileUpload :: MockConfig -> FileUploadConfig
            makeFileUpload cfg = FileUploadConfig
              { blobStoreDir = cfg.mockUploadDir
              , stateStoreBackend = InMemoryStateStore
              , maxFileSizeBytes = 10485760
              , pendingTtlSeconds = 21600
              , cleanupIntervalSeconds = 900
              , allowedContentTypes = Nothing
              , storeOriginalFilename = True
              }
        let app = Application.new
              |> Application.withFileUploadFrom @MockConfig makeFileUpload
        -- Verify the application was modified (has file upload factory)
        Application.hasFileUpload app |> shouldBe True

      it "returns True for hasFileUpload after withFileUploadFrom" \_ -> do
        let factory :: MockConfig -> FileUploadConfig
            factory _ = FileUploadConfig
              { blobStoreDir = "./uploads"
              , stateStoreBackend = InMemoryStateStore
              , maxFileSizeBytes = 10485760
              , pendingTtlSeconds = 21600
              , cleanupIntervalSeconds = 900
              , allowedContentTypes = Nothing
              , storeOriginalFilename = True
              }
        let app = Application.new
              |> Application.withFileUploadFrom @MockConfig factory
        Application.hasFileUpload app |> shouldBe True

    describe "backwards compatibility" do
      it "withFileUpload still works for direct values" \_ -> do
        let directConfig = FileUploadConfig
              { blobStoreDir = "./uploads"
              , stateStoreBackend = InMemoryStateStore
              , maxFileSizeBytes = 10485760
              , pendingTtlSeconds = 21600
              , cleanupIntervalSeconds = 900
              , allowedContentTypes = Nothing
              , storeOriginalFilename = True
              }
        let app = Application.new
              |> Application.withFileUpload directConfig
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
      -- This test just documents the behavior. The compile-time check
      -- is inherent in the type signature:
      --   withEventStoreFrom :: forall config. (Typeable config, ...) =>
      --     (config -> eventStoreConfig) -> Application -> Application
      --
      -- The @config type application must match the function's input type.
      pass
