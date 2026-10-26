-- | Verify Unix epoch millisecond conversion through the public DateTime API.
-- The core test runner calls 'spec' when checking rounding behavior.
module DateTimeSpec (spec) where

import Core
import DateTime qualified
import Json qualified
import Test


spec :: Spec Unit
spec = do
  describe "DateTime.toEpochMilliseconds" do
    it "preserves whole epoch seconds in milliseconds" \_ -> do
      DateTime.fromEpochSeconds 0 |> DateTime.toEpochMilliseconds |> shouldBe 0
      DateTime.fromEpochSeconds 2 |> DateTime.toEpochMilliseconds |> shouldBe 2000
      DateTime.fromEpochSeconds (-1) |> DateTime.toEpochMilliseconds |> shouldBe (-1000)

    it "rounds a submillisecond fraction to the nearest millisecond" \_ -> do
      let below = Json.decodeText @DateTime "\"1970-01-01T00:00:00.0004Z\""
      let above = Json.decodeText @DateTime "\"1970-01-01T00:00:00.0006Z\""
      case (below, above) of
        (Ok belowTime, Ok aboveTime) -> do
          DateTime.toEpochMilliseconds belowTime |> shouldBe 0
          DateTime.toEpochMilliseconds aboveTime |> shouldBe 1
        (Err err, _) -> Test.fail err
        (_, Err err) -> Test.fail err

    it "rounds present-epoch half-millisecond boundaries precisely" \_ -> do
      let aboveFirstHalf = Json.decodeText @DateTime "\"2026-10-26T00:00:00.000500000001Z\""
      let belowSecondHalf = Json.decodeText @DateTime "\"2026-10-26T00:00:00.001499999999Z\""
      case (aboveFirstHalf, belowSecondHalf) of
        (Ok firstTime, Ok secondTime) -> do
          DateTime.toEpochMilliseconds firstTime |> shouldBe 1792972800001
          DateTime.toEpochMilliseconds secondTime |> shouldBe 1792972800001
        (Err err, _) -> Test.fail err
        (_, Err err) -> Test.fail err
