{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.CommandSpec where

import Array qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Core
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Map qualified
import Service.Command.Core (NameOf)
import Task qualified
import Test
import Text qualified
import Uuid qualified


-- ============================================================================
-- Test Types
-- ============================================================================

-- | A simple test command for integration tests.
data ReserveStock = ReserveStock
  { stockId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON ReserveStock


instance Json.FromJSON ReserveStock


type instance NameOf ReserveStock = "ReserveStock"


makeContext :: Task Text Integration.ActionContext
makeContext = do
  store <- InMemorySecretStore.new
  Task.yield
    Integration.ActionContext
      { Integration.secretStore = store
      , Integration.providerRegistry = Map.empty
      , Integration.fileAccess = Nothing
      }


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Command" do
    describe "Emit" do
      it "has a command field" \_ -> do
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 5, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        emit.command |> shouldBe cmd

      it "emits a CommandPayload with correct type name" \_ -> do
        context <- makeContext
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 10, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        let act = Integration.outbound emit
        result <- Integration.runAction context act |> Task.mapError toText
        case result of
          Just payload -> do
            payload.commandType |> shouldSatisfy (Text.contains "ReserveStock")
          Nothing -> do
            fail "Expected Just CommandPayload but got Nothing"

      it "emits a CommandPayload with JSON-decodable data" \_ -> do
        context <- makeContext
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 10, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        let act = Integration.outbound emit
        result <- Integration.runAction context act |> Task.mapError toText
        case result of
          Just payload -> do
            case Json.decode payload.commandData of
              Ok decoded -> decoded |> shouldBe cmd
              Err _ -> fail "Failed to decode command data"
          Nothing -> do
            fail "Expected Just CommandPayload but got Nothing"

      it "can batch multiple Emit actions" \_ -> do
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd1 = ReserveStock {stockId = stockId, quantity = 5, cartId = cartId}
        let cmd2 = ReserveStock {stockId = stockId, quantity = 3, cartId = cartId}
        let outbound = Integration.batch
              [ Integration.outbound Command.Emit {command = cmd1}
              , Integration.outbound Command.Emit {command = cmd2}
              ]
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 2

    describe "Emit ToAction instance" do
      it "produces a CommandPayload matching the wrapped command" \_ -> do
        context <- makeContext
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 7, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        let act = Integration.toAction emit
        result <- Integration.runAction context act |> Task.mapError toText
        case result of
          Just payload -> do
            case Json.decode @ReserveStock payload.commandData of
              Ok decoded -> do
                decoded.quantity |> shouldBe 7
                decoded.stockId |> shouldBe stockId
                decoded.cartId |> shouldBe cartId
              Err _ -> fail "Failed to decode command data"
          Nothing -> do
            fail "Expected Just CommandPayload but got Nothing"
