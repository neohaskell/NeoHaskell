{-# OPTIONS_GHC -Wno-unused-imports #-}

module IntSpec where

import Core
import Int qualified
import Test


spec :: Spec Unit
spec = do
  describe "Int" do
    describe "powerOf" do
      it "2^0 = 1" \_ -> do
        0 |> Int.powerOf 2 |> shouldBe 1

      it "2^1 = 2" \_ -> do
        1 |> Int.powerOf 2 |> shouldBe 2

      it "2^2 = 4" \_ -> do
        2 |> Int.powerOf 2 |> shouldBe 4

      it "2^3 = 8" \_ -> do
        3 |> Int.powerOf 2 |> shouldBe 8

      it "2^10 = 1024" \_ -> do
        10 |> Int.powerOf 2 |> shouldBe 1024

      it "3^4 = 81" \_ -> do
        4 |> Int.powerOf 3 |> shouldBe 81

      it "5^3 = 125" \_ -> do
        3 |> Int.powerOf 5 |> shouldBe 125

      it "10^0 = 1" \_ -> do
        0 |> Int.powerOf 10 |> shouldBe 1

      it "handles negative exponent as 0" \_ -> do
        (-1) |> Int.powerOf 2 |> shouldBe 1
        (-5) |> Int.powerOf 3 |> shouldBe 1

      it "any base to power 0 is 1" \_ -> do
        0 |> Int.powerOf 1 |> shouldBe 1
        0 |> Int.powerOf 100 |> shouldBe 1
        0 |> Int.powerOf 999 |> shouldBe 1

      it "1 to any power is 1" \_ -> do
        5 |> Int.powerOf 1 |> shouldBe 1
        10 |> Int.powerOf 1 |> shouldBe 1
        100 |> Int.powerOf 1 |> shouldBe 1
