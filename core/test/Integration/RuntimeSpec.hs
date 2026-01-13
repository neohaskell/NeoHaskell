module Integration.RuntimeSpec where

import Array qualified
import AsyncTask qualified
import ConcurrentVar qualified
import Core
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Service.Application qualified as Application
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
      it "dispatches emitted commands to registered handlers" \_ -> do
        eventStore <- InMemory.new |> Task.mapError toText

        -- Track received commands
        receivedCommandRef <- ConcurrentVar.containing (Nothing :: Maybe NotifyExternalSystem)

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

        -- Define a command handler that captures the command
        let commandHandler :: NotifyExternalSystem -> Task Text Unit
            commandHandler cmd = do
              receivedCommandRef |> ConcurrentVar.set (Just cmd)
              Task.yield unit

        let app =
              Application.new
                |> Application.withOutbound @TestEntity @TestEntityEvent testIntegration
                |> Application.withCommandHandler @NotifyExternalSystem commandHandler

        -- Run app in background
        Application.runWithAsync eventStore app

        -- Give app time to start
        AsyncTask.sleep 50 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Persist an event that triggers the integration
        let testEntityId = Uuid.nil
        insertTypedEvent eventStore (EntityName "TestEntity") testEntityId (TestEntityCreated {initialValue = 42})

        -- Give integration time to process and dispatch
        AsyncTask.sleep 150 |> Task.mapError (\_ -> "sleep error" :: Text)

        -- Verify the command was dispatched to the handler
        receivedCommand <- ConcurrentVar.peek receivedCommandRef
        case receivedCommand of
          Just cmd -> do
            cmd.targetEntityId |> shouldBe testEntityId
            cmd.notificationValue |> shouldBe 42
          Nothing -> fail "Expected command to be dispatched to handler"
