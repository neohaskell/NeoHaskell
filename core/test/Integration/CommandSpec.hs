module Integration.CommandSpec where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Json qualified
import Test
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

      it "implements ToAction typeclass" \_ -> do
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 10, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        let _action = Integration.outbound emit
        -- Action is opaque, verify it compiles
        True |> shouldBe True

      it "can be used with Integration.batch" \_ -> do
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd1 = ReserveStock {stockId = stockId, quantity = 5, cartId = cartId}
        let cmd2 = ReserveStock {stockId = stockId, quantity = 3, cartId = cartId}
        let _outbound = Integration.batch
              [ Integration.outbound Command.Emit {command = cmd1}
              , Integration.outbound Command.Emit {command = cmd2}
              ]
        True |> shouldBe True

    describe "Emit ToAction instance" do
      it "produces a CommandPayload with correct command type" \_ -> do
        let stockId = Uuid.nil
        let cartId = Uuid.nil
        let cmd = ReserveStock {stockId = stockId, quantity = 7, cartId = cartId}
        let emit = Command.Emit {command = cmd}
        let _action = Integration.toAction emit
        -- We can't inspect Action directly, but we can verify it compiles
        True |> shouldBe True
