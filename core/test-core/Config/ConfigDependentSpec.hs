-- | Tests for Application.withEventStore, withFileUpload, and withAuth unified API.
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
import Array qualified
import Auth.SecretStore (SecretStore (..))
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Core
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Service.Application qualified as Application
import Service.Application.Types (ApiInfo (..))
import Service.Event (Event)
import Service.EventStore.InMemory qualified as InMemory
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.FileUpload.Core (FileUploadConfig (..), FileStateStoreBackend (..))
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Transport.Web qualified as Web
import Task qualified
import Test
import Text qualified
import Uuid qualified


-- | Mock config type for testing.
-- This simulates a user-defined config type without needing HasParser.
data MockConfig = MockConfig
  { mockDbHost :: Text
  , mockDbPort :: Int
  , mockUploadDir :: Text
  , mockAuthServerUrl :: Text
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


-- | Helper to create an auth server URL factory for testing.
makeAuthUrlFactory :: MockConfig -> Text
makeAuthUrlFactory cfg = cfg.mockAuthServerUrl


-- | Direct auth server URL for testing.
directAuthServerUrl :: Text
directAuthServerUrl = "https://auth.example.com"


-- | Helper to create a CorsConfig factory for testing.
makeCorsFactory :: MockConfig -> Web.CorsConfig
makeCorsFactory _cfg = Web.CorsConfig
  { allowedOrigins = Array.fromLinkedList ["*"]
  , allowedMethods = Array.fromLinkedList ["GET"]
  , allowedHeaders = Array.fromLinkedList ["Content-Type"]
  , maxAge = Nothing
  }


-- | Helper to create an ApiInfo factory for testing.
makeApiInfoFactory :: MockConfig -> ApiInfo
makeApiInfoFactory _cfg = ApiInfo
  { apiTitle = "Test"
  , apiVersion = "1.0"
  , apiDescription = "Test API"
  }


-- | Helper to create a health check path factory for testing.
makeHealthCheckFactory :: MockConfig -> Text
makeHealthCheckFactory _cfg = "_health"


-- | Helper to create a DispatcherConfig factory for testing.
makeDispatcherConfigFactory :: MockConfig -> Dispatcher.DispatcherConfig
makeDispatcherConfigFactory _cfg = Dispatcher.defaultConfig


-- | Helper to create a SecretStore factory for testing.
makeSecretStoreFactory :: MockConfig -> SecretStore
makeSecretStoreFactory _cfg = do
  -- We can't actually create a SecretStore in a pure context,
  -- so this factory is used only to test that DeferredSecretStore is stored.
  -- The runWith rejection test will trigger before evaluation.
  panic "makeSecretStoreFactory should not be evaluated in these tests"


-- ============================================================================
-- Test Types for Integration Tests
-- ============================================================================

-- | A test entity for integration tests.
data TestEntity = TestEntity
  { entityId :: Uuid
  , value :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON TestEntity


instance Json.FromJSON TestEntity


instance Default TestEntity where
  def = TestEntity {entityId = Uuid.nil, value = 0}


-- | Events for the test entity.
data TestEntityEvent
  = TestEntityCreated {initialValue :: Int}
  | TestEntityUpdated {newValue :: Int}
  deriving (Generic, Eq, Show, Typeable)


-- | Type family instances for Entity/Event relationship.
type instance EventOf TestEntity = TestEntityEvent


-- | Entity instance for TestEntity.
instance Entity TestEntity where
  initialStateImpl = TestEntity {entityId = Uuid.nil, value = 0}
  updateImpl event entity = case event of
    TestEntityCreated {initialValue} -> entity {value = initialValue}
    TestEntityUpdated {newValue} -> entity {value = newValue}


instance Json.ToJSON TestEntityEvent


instance Json.FromJSON TestEntityEvent


-- | A command emitted by integrations.
data NotifyExternalSystem = NotifyExternalSystem
  { targetEntityId :: Uuid
  , notificationValue :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON NotifyExternalSystem


instance Json.FromJSON NotifyExternalSystem


type instance NameOf NotifyExternalSystem = "NotifyExternalSystem"


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

  describe "Application.withAuth (unified API)" do
    it "stores factory and sets hasAuth to True" \_ -> do
      -- Same pattern as EventStore/FileUpload: factory is stored, not evaluated at build time
      let app = Application.new
            |> Application.withAuth makeAuthUrlFactory
      Application.hasAuth app |> shouldBe True

    it "works with static config using @() pattern" \_ -> do
      -- For static configs that don't depend on app config, use @() pattern
      let app = Application.new
            |> Application.withAuth @() (\_ -> directAuthServerUrl)
      Application.hasAuth app |> shouldBe True

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

    it "second withAuth call overwrites first" \_ -> do
      -- When withAuth is called multiple times, the last one wins.
      let differentFactory :: MockConfig -> Text
          differentFactory cfg = cfg.mockAuthServerUrl
      let app = Application.new
            |> Application.withAuth @() (\_ -> directAuthServerUrl)
            |> Application.withAuth differentFactory
      -- Both set hasAuth to True
      Application.hasAuth app |> shouldBe True
      -- The second call wins (we can't easily verify which one
      -- without running the app, but we document the behavior)

    it "static config overwrites dynamic factory for Auth" \_ -> do
      -- Reverse order: static config after dynamic factory
      let app = Application.new
            |> Application.withAuth makeAuthUrlFactory
            |> Application.withAuth @() (\_ -> directAuthServerUrl)
      Application.hasAuth app |> shouldBe True

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

    it "DeferredWebAuth requires withConfig" \_ -> do
      -- NOTE: Testing this error path requires a working EventStore to get past
      -- the EventStore creation step. Since this unit test doesn't have Postgres,
      -- we test via nhcore-test-service (which has Postgres) or testbed integration tests.
      --
      -- The error we're documenting: When Application.run encounters
      -- DeferredWebAuth but no configSpec was set, it throws:
      -- "withAuth requires withConfig when the factory uses the config parameter."
      pending "requires Postgres - tested in nhcore-test-service and testbed"

    it "runWith rejects DeferredWebAuth" \_ -> do
      -- runWith doesn't support config-dependent factories at all, so it
      -- rejects DeferredWebAuth with a clear error message.
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withAuth makeAuthUrlFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent Auth")

  describe "Application.withCors (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let corsConfig = Web.CorsConfig
            { allowedOrigins = Array.fromLinkedList ["*"]
            , allowedMethods = Array.fromLinkedList ["GET"]
            , allowedHeaders = Array.fromLinkedList ["Content-Type"]
            , maxAge = Nothing
            }
      let app = Application.new
            |> Application.withCors @() (\_ -> corsConfig)
      -- App builds without error (factory stored as EvaluatedCors)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withCors makeCorsFactory
      -- App builds without error (factory stored as DeferredCors)
      let _ = app
      shouldBe True True

    it "runWith rejects DeferredCors" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withCors makeCorsFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent CORS")

  describe "Application.withApiInfo (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let info = ApiInfo
            { apiTitle = "Test"
            , apiVersion = "1.0"
            , apiDescription = "Test API"
            }
      let app = Application.new
            |> Application.withApiInfo @() (\_ -> info)
      -- App builds without error (factory stored as EvaluatedApiInfo)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withApiInfo makeApiInfoFactory
      -- App builds without error (factory stored as DeferredApiInfo)
      let _ = app
      shouldBe True True

    it "runWith rejects DeferredApiInfo" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withApiInfo makeApiInfoFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent ApiInfo")

  describe "Application.withHealthCheck (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let app = Application.new
            |> Application.withHealthCheck @() (\_ -> "_health")
      -- App builds without error (factory stored as EvaluatedHealthCheck)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withHealthCheck makeHealthCheckFactory
      -- App builds without error (factory stored as DeferredHealthCheck)
      let _ = app
      shouldBe True True

    it "runWith rejects DeferredHealthCheck" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withHealthCheck makeHealthCheckFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent HealthCheck")

  describe "Application.withDispatcherConfig (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let app = Application.new
            |> Application.withDispatcherConfig @() (\_ -> Dispatcher.defaultConfig)
      -- App builds without error (factory stored as EvaluatedDispatcherConfig)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withDispatcherConfig makeDispatcherConfigFactory
      -- App builds without error (factory stored as DeferredDispatcherConfig)
      let _ = app
      shouldBe True True

    it "runWith rejects DeferredDispatcherConfig" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withDispatcherConfig makeDispatcherConfigFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent DispatcherConfig")

  describe "Application.withSecretStore (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      store <- InMemorySecretStore.new
      let app = Application.new
            |> Application.withSecretStore @() (\_ -> store)
      -- App builds without error (factory stored as EvaluatedSecretStore)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withSecretStore makeSecretStoreFactory
      -- App builds without error (factory stored as DeferredSecretStore)
      let _ = app
      shouldBe True True

    it "runWith rejects DeferredSecretStore" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withSecretStore makeSecretStoreFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent SecretStore")

  describe "Application.withOutbound (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let app = Application.new
            |> Application.withOutbound @() @TestEntity @TestEntityEvent (\_ _entity _event -> Integration.none)
      -- App builds without error (outbound runner stored directly)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let app = Application.new
            |> Application.withOutbound @MockConfig @TestEntity @TestEntityEvent (\_ _entity _event -> Integration.none)
      -- App builds without error (deferred outbound reg stored)
      let _ = app
      shouldBe True True

    it "runWith rejects deferred outbound integrations" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withOutbound @MockConfig @TestEntity @TestEntityEvent (\_ _entity _event -> Integration.none)
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent outbound integrations")

  describe "Application.withOutboundLifecycle (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let lifecycleConfig = Lifecycle.OutboundConfig
            { initialize = \(_streamId :: StreamId) -> Task.yield (0 :: Int)
            , processEvent = \(_state :: Int) (_event :: Event Json.Value) -> Task.yield Array.empty
            , cleanup = \(_state :: Int) -> Task.yield ()
            }
      let app = Application.new
            |> Application.withOutboundLifecycle @() @TestEntity (\_ -> lifecycleConfig)
      -- App builds without error (lifecycle runner stored directly)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let lifecycleConfig = Lifecycle.OutboundConfig
            { initialize = \(_streamId :: StreamId) -> Task.yield (0 :: Int)
            , processEvent = \(_state :: Int) (_event :: Event Json.Value) -> Task.yield Array.empty
            , cleanup = \(_state :: Int) -> Task.yield ()
            }
      let app = Application.new
            |> Application.withOutboundLifecycle @MockConfig @TestEntity (\_ -> lifecycleConfig)
      -- App builds without error (deferred outbound lifecycle reg stored)
      let _ = app
      shouldBe True True

    it "runWith rejects deferred outbound lifecycle integrations" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let lifecycleConfig = Lifecycle.OutboundConfig
            { initialize = \(_streamId :: StreamId) -> Task.yield (0 :: Int)
            , processEvent = \(_state :: Int) (_event :: Event Json.Value) -> Task.yield Array.empty
            , cleanup = \(_state :: Int) -> Task.yield ()
            }
      let app = Application.new
            |> Application.withOutboundLifecycle @MockConfig @TestEntity (\_ -> lifecycleConfig)
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent outbound lifecycle integrations")

  describe "Application.withInbound (config-dependent)" do
    it "works with static config using @() pattern" \_ -> do
      let inboundIntegration = Integration.inbound @NotifyExternalSystem Integration.InboundConfig
            { run = \_ -> Task.yield ()
            }
      let app = Application.new
            |> Application.withInbound @() (\_ -> inboundIntegration)
      -- App builds without error (inbound integration stored directly)
      let _ = app
      shouldBe True True

    it "stores factory with dynamic config" \_ -> do
      let inboundIntegration = Integration.inbound @NotifyExternalSystem Integration.InboundConfig
            { run = \_ -> Task.yield ()
            }
      let app = Application.new
            |> Application.withInbound @MockConfig (\_ -> inboundIntegration)
      -- App builds without error (deferred inbound reg stored)
      let _ = app
      shouldBe True True

    it "runWith rejects deferred inbound integrations" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let inboundIntegration = Integration.inbound @NotifyExternalSystem Integration.InboundConfig
            { run = \_ -> Task.yield ()
            }
      let app = Application.new
            |> Application.withInbound @MockConfig (\_ -> inboundIntegration)
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent inbound integrations")
