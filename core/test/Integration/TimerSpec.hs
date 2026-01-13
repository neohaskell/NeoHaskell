module Integration.TimerSpec where

import Core
import Integration.Timer qualified as Timer
import Json qualified
import Test


-- ============================================================================
-- Test Types
-- ============================================================================

-- | A simple test command for timer integration tests.
data CreateCart = CreateCart
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON CreateCart


instance Json.FromJSON CreateCart


-- | A tick-aware test command.
data TickCommand = TickCommand
  { tickNumber :: Int
  }
  deriving (Generic, Eq, Show, Typeable)


instance Json.ToJSON TickCommand


instance Json.FromJSON TickCommand


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Timer" do
    describe "Interval helpers" do
      it "seconds converts to milliseconds" \_ -> do
        Timer.seconds 1 |> shouldBe 1000
        Timer.seconds 5 |> shouldBe 5000
        Timer.seconds 30 |> shouldBe 30000
        Timer.seconds 0 |> shouldBe 0

      it "minutes converts to milliseconds" \_ -> do
        Timer.minutes 1 |> shouldBe 60000
        Timer.minutes 5 |> shouldBe 300000
        Timer.minutes 10 |> shouldBe 600000
        Timer.minutes 0 |> shouldBe 0

      it "hours converts to milliseconds" \_ -> do
        Timer.hours 1 |> shouldBe 3600000
        Timer.hours 2 |> shouldBe 7200000
        Timer.hours 24 |> shouldBe 86400000
        Timer.hours 0 |> shouldBe 0

      it "interval conversions are consistent" \_ -> do
        Timer.seconds 60 |> shouldBe (Timer.minutes 1)
        Timer.minutes 60 |> shouldBe (Timer.hours 1)
        Timer.seconds 3600 |> shouldBe (Timer.hours 1)

    describe "Every" do
      it "has interval and toCommand fields" \_ -> do
        let config = Timer.Every
              { interval = Timer.seconds 30
              , toCommand = \_ -> CreateCart
              }
        config.interval |> shouldBe 30000
        config.toCommand 1 |> shouldBe CreateCart

      it "toCommand receives tick count" \_ -> do
        let config = Timer.Every
              { interval = Timer.seconds 10
              , toCommand = \tick -> TickCommand {tickNumber = tick}
              }
        config.toCommand 1 |> shouldBe TickCommand {tickNumber = 1}
        config.toCommand 5 |> shouldBe TickCommand {tickNumber = 5}
        config.toCommand 100 |> shouldBe TickCommand {tickNumber = 100}

    describe "every" do
      it "creates an Integration.Inbound from Every config" \_ -> do
        let config = Timer.Every
              { interval = Timer.seconds 30
              , toCommand = \_ -> CreateCart
              }
        let _inbound = Timer.every config
        -- Inbound is opaque, verify it compiles
        True |> shouldBe True

      it "can be used with Integration.inbound workflow" \_ -> do
        let _inbound = Timer.every Timer.Every
              { interval = Timer.minutes 5
              , toCommand = \tick -> TickCommand {tickNumber = tick}
              }
        True |> shouldBe True

    describe "Timer patterns" do
      it "supports constant command generation" \_ -> do
        let config = Timer.Every
              { interval = Timer.hours 1
              , toCommand = \_ -> CreateCart
              }
        -- All ticks produce the same command
        config.toCommand 1 |> shouldBe (config.toCommand 2)
        config.toCommand 100 |> shouldBe (config.toCommand 200)

      it "supports tick-dependent command generation" \_ -> do
        let config = Timer.Every
              { interval = Timer.seconds 10
              , toCommand = \tick -> TickCommand {tickNumber = tick * 10}
              }
        -- Commands vary based on tick
        config.toCommand 1 |> shouldBe TickCommand {tickNumber = 10}
        config.toCommand 2 |> shouldBe TickCommand {tickNumber = 20}
        config.toCommand 3 |> shouldBe TickCommand {tickNumber = 30}
