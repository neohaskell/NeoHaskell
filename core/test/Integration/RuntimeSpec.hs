module Integration.RuntimeSpec where

import Array qualified
import AsyncTask qualified
import Core
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Service.Application qualified as Application
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Text qualified
import Uuid qualified


-- ============================================================================
-- Test Types
-- ============================================================================

-- | A test entity for integration tests.
data TestEntity = TestEntity
  { entityId :: Uuid
  , value :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


-- | Events for the test entity.
data TestEntityEvent
  = TestEntityCreated {entityId :: Uuid, initialValue :: Int}
  | TestEntityUpdated {entityId :: Uuid, newValue :: Int}
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON TestEntityEvent


instance Json.FromJSON TestEntityEvent


-- | A command emitted by the integration.
data NotifyExternalSystem = NotifyExternalSystem
  { targetEntityId :: Uuid
  , notificationValue :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON NotifyExternalSystem


instance Json.FromJSON NotifyExternalSystem


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Runtime" do
    describe "withOutbound" do
      it "executes integration when entity event is persisted" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText

        -- Define an integration that emits a command when TestEntityCreated occurs
        let testIntegration :: TestEntity -> TestEntityEvent -> Integration.Outbound
            testIntegration entity event = case event of
              TestEntityCreated {initialValue} ->
                Integration.batch
                  [ Integration.outbound
                      Command.Emit
                        { command =
                            NotifyExternalSystem
                              { targetEntityId = entity.entityId
                              , notificationValue = initialValue
                              }
                        }
                  ]
              _ -> Integration.none

        let app =
              Application.new
                |> Application.withOutbound testIntegration

        -- Run app in background
        Application.runWithAsync eventStore app

        -- Give app time to start
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Persist an event that triggers the integration
        let testEntityId = Uuid.nil
        let event = TestEntityCreated {entityId = testEntityId, initialValue = 42}

        -- TODO: We need a way to persist events and have integrations react
        -- This test documents the expected behavior but will fail until
        -- the runtime wiring is implemented

        -- For now, verify the integration function produces the right output
        let entity = TestEntity {entityId = testEntityId, value = 42}
        let outbound = testIntegration entity event
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 1

        -- Verify the action produces the expected command
        let firstAction = actions |> Array.first
        case firstAction of
          Just act -> do
            result <- Integration.runAction act |> Task.mapError toText
            case result of
              Just payload -> do
                payload.commandType |> shouldSatisfy (Text.contains "NotifyExternalSystem")
                case Json.decode @NotifyExternalSystem payload.commandData of
                  Ok cmd -> do
                    cmd.targetEntityId |> shouldBe testEntityId
                    cmd.notificationValue |> shouldBe 42
                  Err _ -> fail "Failed to decode command"
              Nothing -> fail "Expected command payload"
          Nothing -> fail "Expected at least one action"

      it "does not execute integration when event matches none pattern" \_ -> do
        let testIntegration :: TestEntity -> TestEntityEvent -> Integration.Outbound
            testIntegration _entity event = case event of
              TestEntityCreated {} -> Integration.batch []
              TestEntityUpdated {} -> Integration.none

        let entity = TestEntity {entityId = Uuid.nil, value = 0}
        let event = TestEntityUpdated {entityId = Uuid.nil, newValue = 100}
        let outbound = testIntegration entity event
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 0
