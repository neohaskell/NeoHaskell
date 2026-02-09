{-# OPTIONS_GHC -Wno-unused-imports #-}

module DecimalSpec where

import Core
import Data.Aeson qualified as Aeson
import Decimal (Decimal (..))
import Decimal qualified
import Json qualified
import Result qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Decimal" do
    describe "construction" do
      it "decimal 12.50 creates correct internal value" \_ -> do
        let d = Decimal.decimal 12.50
        d.unDecimal |> shouldBe 125000

      it "fromCents 1250 equals decimal 12.50" \_ -> do
        Decimal.fromCents 1250 |> shouldBe (Decimal.decimal 12.50)

      it "toCents (decimal 12.50) equals 1250" \_ -> do
        Decimal.decimal 12.50 |> Decimal.toCents |> shouldBe 1250

      it "zero is zero" \_ -> do
        Decimal.zero |> shouldBe (Decimal.decimal 0.00)

      it "fromInteger creates correct value" \_ -> do
        let d = 42 :: Decimal
        d.unDecimal |> shouldBe 420000

    describe "arithmetic" do
      it "addition: 10.50 + 2.25 = 12.75" \_ -> do
        Decimal.decimal 10.50 + Decimal.decimal 2.25 |> shouldBe (Decimal.decimal 12.75)

      it "subtraction: 10.50 - 2.25 = 8.25" \_ -> do
        Decimal.decimal 10.50 - Decimal.decimal 2.25 |> shouldBe (Decimal.decimal 8.25)

      it "multiplication: 10.00 * 1.21 = 12.10" \_ -> do
        Decimal.decimal 10.00 * Decimal.decimal 1.21 |> shouldBe (Decimal.decimal 12.10)

      it "division: 100.00 / 4.00 = 25.00" \_ -> do
        Decimal.divide (Decimal.decimal 100.00) (Decimal.decimal 4.00) |> shouldBe (Decimal.decimal 25.00)

      it "negate works" \_ -> do
        negate (Decimal.decimal 5.25) |> shouldBe (Decimal.decimal (-5.25))

      it "abs of negative" \_ -> do
        abs (Decimal.decimal (-5.25)) |> shouldBe (Decimal.decimal 5.25)

    describe "edge cases" do
      it "zero + zero = zero" \_ -> do
        Decimal.decimal 0.00 + Decimal.decimal 0.00 |> shouldBe (Decimal.decimal 0.00)

      it "negative + positive = zero" \_ -> do
        Decimal.decimal (-10.50) + Decimal.decimal 10.50 |> shouldBe (Decimal.decimal 0.00)

      it "small multiplication preserves precision" \_ -> do
        Decimal.decimal 0.01 * Decimal.decimal 0.01 |> shouldBe (Decimal.decimal 0.0001)

      it "fromCents and toCents round-trip" \_ -> do
        let cents = 9999 :: Int64
        Decimal.fromCents cents |> Decimal.toCents |> shouldBe cents

    describe "JSON" do
      it "encodes as string" \_ -> do
        let encoded = Json.encodeText (Decimal.decimal 12.50)
        encoded |> shouldBe "\"12.5000\""

      it "decodes from string" \_ -> do
        let decoded = Json.decodeText @Decimal "\"12.5000\""
        decoded |> shouldBe (Result.Ok (Decimal.decimal 12.50))

      it "round-trips correctly" \_ -> do
        let original = Decimal.decimal 99.99
        let encoded = Json.encodeText original
        let decoded = Json.decodeText @Decimal encoded
        decoded |> shouldBe (Result.Ok original)

      it "rejects invalid JSON" \_ -> do
        let decoded = Json.decodeText @Decimal "\"abc\""
        case decoded of
          Result.Err _ -> True |> shouldBe True
          Result.Ok _ -> True |> shouldBe False

    describe "formatting" do
      it "formats with 4 decimal places" \_ -> do
        Decimal.formatDecimal (Decimal.decimal 12.50) |> shouldBe "12.5000"

      it "formats zero correctly" \_ -> do
        Decimal.formatDecimal (Decimal.decimal 0.00) |> shouldBe "0.0000"

      it "formats negative correctly" \_ -> do
        Decimal.formatDecimal (Decimal.decimal (-5.25)) |> shouldBe "-5.2500"

      it "formats large numbers" \_ -> do
        Decimal.formatDecimal (Decimal.decimal 99999.99) |> shouldBe "99999.9900"

    describe "parsing" do
      it "parses valid decimal" \_ -> do
        Decimal.parseDecimal "12.50" |> shouldBe (Just (Decimal.decimal 12.50))

      it "parses negative" \_ -> do
        Decimal.parseDecimal "-5.25" |> shouldBe (Just (Decimal.decimal (-5.25)))

      it "parses integer" \_ -> do
        Decimal.parseDecimal "42" |> shouldBe (Just (Decimal.decimal 42.00))

      it "rejects invalid input" \_ -> do
        Decimal.parseDecimal "abc" |> shouldBe Nothing

      it "rejects empty string" \_ -> do
        Decimal.parseDecimal "" |> shouldBe Nothing

    describe "rounding" do
      it "roundTo2 rounds correctly" \_ -> do
        let d = Decimal 125678  -- 12.5678
        let rounded = Decimal.roundTo2 d
        -- Should round to 12.5700 (125700)
        rounded.unDecimal |> shouldBe 125700
