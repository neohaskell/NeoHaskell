{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module IntegrationSpec where

import Array qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Core
import GHC.TypeLits qualified as GHC
import Integration qualified
import Json qualified
import Map qualified
import Service.Command.Core (NameOf)
import Task qualified
import Test
import Text qualified


-- ============================================================================
-- Test Types
-- ============================================================================

-- | A simple test command for integration tests.
data TestCommand = TestCommand
  { commandId :: Int
  , commandName :: Text
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON TestCommand


instance Json.FromJSON TestCommand


type instance NameOf TestCommand = "TestCommand"


makeContext :: Task Text Integration.ActionContext
makeContext = do
  store <- InMemorySecretStore.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Map.empty
      }


-- | A test config record with ToAction instance.
data TestConfig command = TestConfig
  { configValue :: Int
  , toCommand :: Int -> command
  }
  deriving (Generic)


instance
  (Json.ToJSON command, GHC.KnownSymbol (NameOf command)) =>
  Integration.ToAction (TestConfig command)
  where
  toAction config = Integration.action \_ctx -> do
    Integration.emitCommand (config.toCommand config.configValue)


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Outbound" do
    describe "batch" do
      it "collects multiple actions" \_ -> do
        let config = TestConfig {configValue = 42, toCommand = \n -> TestCommand {commandId = n, commandName = "test"}}
        let action1 = Integration.outbound config
        let action2 = Integration.outbound config
        let outbound = Integration.batch [action1, action2]
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 2

      it "creates empty Outbound from empty array" \_ -> do
        let outbound = Integration.batch (Array.empty :: Array Integration.Action)
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 0

    describe "none" do
      it "creates an Outbound with zero actions" \_ -> do
        let outbound = Integration.none
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 0

    describe "outbound" do
      it "converts a ToAction config to an executable Action" \_ -> do
        context <- makeContext
        let config = TestConfig {configValue = 100, toCommand = \n -> TestCommand {commandId = n, commandName = "outbound-test"}}
        let act = Integration.outbound config
        result <- Integration.runAction context act |> Task.mapError toText
        case result of
          Just payload -> do
            payload.commandType |> shouldSatisfy (Text.contains "TestCommand")
          Nothing -> do
            fail "Expected Just CommandPayload but got Nothing"

  describe "Integration.Action construction" do
    describe "action" do
      it "creates an Action that can be executed" \_ -> do
        context <- makeContext
        let cmd = TestCommand {commandId = 1, commandName = "action-test"}
        let act = Integration.action \_ctx -> Integration.emitCommand cmd
        result <- Integration.runAction context act |> Task.mapError toText
        case result of
          Just payload -> do
            payload.commandType |> shouldSatisfy (Text.contains "TestCommand")
          Nothing -> do
            fail "Expected Just CommandPayload but got Nothing"

    describe "emitCommand" do
      it "creates a CommandPayload with correct type name" \_ -> do
        let cmd = TestCommand {commandId = 42, commandName = "emit-test"}
        let task = Integration.emitCommand cmd
        result <- task |> Task.mapError toText
        case result of
          Just payload -> do
            payload.commandType |> shouldSatisfy (Text.contains "TestCommand")
          Nothing -> do
            fail "Expected Just but got Nothing"

      it "creates a CommandPayload with JSON-encoded command data" \_ -> do
        let cmd = TestCommand {commandId = 42, commandName = "json-test"}
        let task = Integration.emitCommand cmd
        result <- task |> Task.mapError toText
        case result of
          Just payload -> do
            case Json.decode payload.commandData of
              Ok decoded -> decoded |> shouldBe cmd
              Err _ -> fail "Failed to decode command data"
          Nothing -> do
            fail "Expected Just but got Nothing"

    describe "noCommand" do
      it "yields Nothing indicating no follow-up command" \_ -> do
        let task = Integration.noCommand
        result <- task |> Task.mapError toText
        case result of
          Nothing -> Task.yield unit
          Just _ -> fail "Expected Nothing but got Just"

  describe "Integration.IntegrationError" do
    it "NetworkError contains error message" \_ -> do
      let err = Integration.NetworkError "connection refused"
      let errText = show err |> Text.fromLinkedList
      errText |> shouldSatisfy (Text.contains "connection refused")

    it "AuthenticationError contains error message" \_ -> do
      let err = Integration.AuthenticationError "invalid token"
      let errText = show err |> Text.fromLinkedList
      errText |> shouldSatisfy (Text.contains "invalid token")

    it "ValidationError contains error message" \_ -> do
      let err = Integration.ValidationError "missing field"
      let errText = show err |> Text.fromLinkedList
      errText |> shouldSatisfy (Text.contains "missing field")

    it "RateLimited contains retry seconds" \_ -> do
      let err = Integration.RateLimited 60
      let errText = show err |> Text.fromLinkedList
      errText |> shouldSatisfy (Text.contains "60")

    it "PermanentFailure contains error message" \_ -> do
      let err = Integration.PermanentFailure "resource not found"
      let errText = show err |> Text.fromLinkedList
      errText |> shouldSatisfy (Text.contains "resource not found")

  describe "Integration.CommandPayload" do
    it "has commandType and commandData fields" \_ -> do
      let payload = Integration.CommandPayload
            { commandType = "TestCommand"
            , commandData = Json.encode (TestCommand {commandId = 1, commandName = "test"})
            }
      payload.commandType |> shouldBe "TestCommand"

    it "supports equality" \_ -> do
      let payload1 = Integration.CommandPayload
            { commandType = "Test"
            , commandData = Json.encode (42 :: Int)
            }
      let payload2 = Integration.CommandPayload
            { commandType = "Test"
            , commandData = Json.encode (42 :: Int)
            }
      payload1 |> shouldBe payload2

  describe "Integration.encodeCommand" do
    it "produces JSON text with commandType and commandData" \_ -> do
      let cmd = TestCommand {commandId = 42, commandName = "encode-test"}
      let jsonText = Integration.encodeCommand cmd
      -- Should contain the command type name
      jsonText |> shouldSatisfy (Text.contains "TestCommand")
      -- Should contain the command data
      jsonText |> shouldSatisfy (Text.contains "encode-test")

    it "round-trips through decodeText -> CommandPayload" \_ -> do
      let cmd = TestCommand {commandId = 99, commandName = "roundtrip-test"}
      let jsonText = Integration.encodeCommand cmd
      case Json.decodeText jsonText of
        Err err -> fail [fmt|Failed to decode JSON: #{err}|]
        Ok (payload :: Integration.CommandPayload) -> do
          payload.commandType |> shouldSatisfy (Text.contains "TestCommand")
          case Json.decode @TestCommand payload.commandData of
            Err err -> fail [fmt|Failed to decode command data: #{err}|]
            Ok decoded -> do
              decoded.commandId |> shouldBe 99
              decoded.commandName |> shouldBe "roundtrip-test"

    it "matches the format expected by dispatchAction" \_ -> do
      -- This verifies encodeCommand produces the exact JSON format
      -- that Application.dispatchAction expects to decode
      let cmd = TestCommand {commandId = 1, commandName = "dispatch-format"}
      let jsonText = Integration.encodeCommand cmd
      -- Verify it decodes as CommandPayload (the format dispatchAction uses)
      case Json.decodeText @Integration.CommandPayload jsonText of
        Err _ -> fail "encodeCommand output should decode as CommandPayload"
        Ok payload -> do
          -- The commandType should be the type name
          payload.commandType |> shouldSatisfy (Text.contains "TestCommand")
          -- The commandData should be valid JSON
          case Json.decode @TestCommand payload.commandData of
            Err _ -> fail "commandData should decode to original command"
            Ok _ -> Task.yield unit
