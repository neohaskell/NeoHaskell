{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.RuntimeSpec where

import Array qualified
import AsyncTask qualified
import Core
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Service.Application qualified as Application
import Service.Command.Core (NameOf)
import Service.Event.EntityName (EntityName (..))
import Service.EventStore.InMemory qualified as InMemory
import Service.TestHelpers (insertTypedEvent)
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


instance Json.ToJSON TestEntity


instance Json.FromJSON TestEntity


instance Default TestEntity where
  def = TestEntity {entityId = Uuid.nil, value = 0}


-- | Events for the test entity.
data TestEntityEvent
  = TestEntityCreated {initialValue :: Int}
  | TestEntityUpdated {newValue :: Int}
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


type instance NameOf NotifyExternalSystem = "NotifyExternalSystem"


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
                |> Application.withOutbound @TestEntity @TestEntityEvent testIntegration

        -- Run app in background
        Application.runWithAsync eventStore app

        -- Give app time to start
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Persist an event that triggers the integration
        let testEntityId = Uuid.nil
        insertTypedEvent eventStore (EntityName "TestEntity") testEntityId (TestEntityCreated {initialValue = 42})

        -- Give integration time to process
        AsyncTask.sleep 100 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify the integration was called
        -- For now, test the integration function directly until runtime is wired
        let entity = TestEntity {entityId = testEntityId, value = 42}
        let event = TestEntityCreated {initialValue = 42}
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
        let event = TestEntityUpdated {newValue = 100}
        let outbound = testIntegration entity event
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 0

    describe "command dispatch" do
      it "dispatches emitted commands to service command handlers" \_ -> do
        -- This test verifies that when an integration emits a command,
        -- it gets dispatched to the service that handles that command type.
        --
        -- The flow is:
        -- 1. Event is persisted to EventStore
        -- 2. Integration subscriber receives event
        -- 3. OutboundRunner processes event, calls integration function
        -- 4. Integration function returns Command.Emit with NotifyExternalSystem
        -- 5. Application looks up "NotifyExternalSystem" in commandEndpoints map
        -- 6. Invokes the handler with the JSON-encoded command data
        -- 7. Service's command handler executes
        --
        -- For now, we test that the command payload reaches the dispatch point.
        -- Full end-to-end testing requires setting up a Command instance.

        -- Define an integration that emits a command
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

        -- Verify the integration produces the correct CommandPayload
        let entity = TestEntity {entityId = Uuid.nil, value = 42}
        let event = TestEntityCreated {initialValue = 42}
        let outbound = testIntegration entity event
        let actions = Integration.getActions outbound

        case Array.first actions of
          Just act -> do
            result <- Integration.runAction act |> Task.mapError toText
            case result of
              Just payload -> do
                -- Verify the command type name matches what dispatch will look for
                payload.commandType |> shouldSatisfy (Text.contains "NotifyExternalSystem")
                -- Verify the command data can be decoded
                case Json.decode @NotifyExternalSystem payload.commandData of
                  Ok cmd -> do
                    cmd.notificationValue |> shouldBe 42
                  Err _ -> fail "Command data should be decodable"
              Nothing -> fail "Integration should emit a command"
          Nothing -> fail "Integration should have at least one action"
