{-# OPTIONS_GHC -Wno-unused-imports #-}

module ArraySpec where

import Array qualified
import Core
import Test

spec :: Spec Unit
spec = do
  describe "Array" do
    describe "dropLast" do
      it "removes the final element" \_ -> do
        let array = Array.fromLinkedList [1, 2, 3] :: Array.Array Int
        array |> Array.dropLast |> shouldBe (Array.fromLinkedList [1, 2])

      it "returns the same empty array" \_ -> do
        Array.empty |> Array.dropLast |> shouldBe (Array.empty :: Array.Array Int)

      it "returns an empty array for a single-element array" \_ -> do
        let array = Array.fromLinkedList [1] :: Array.Array Int
        array |> Array.dropLast |> shouldBe Array.empty

    describe "dropRight" do
      it "drops the last n elements" \_ -> do
        let array = Array.fromLinkedList [1, 2, 3, 4] :: Array.Array Int
        array |> Array.dropRight 2 |> shouldBe (Array.fromLinkedList [1, 2])

      it "returns the original array when n is non-positive" \_ -> do
        let array = Array.fromLinkedList [1, 2, 3] :: Array.Array Int
        array |> Array.dropRight 0 |> shouldBe array
        array |> Array.dropRight (-1) |> shouldBe array
        array |> Array.dropRight (minValue :: Int) |> shouldBe array

      it "returns an empty array when n is at least the length" \_ -> do
        let array = Array.fromLinkedList [1, 2, 3] :: Array.Array Int
        array |> Array.dropRight 3 |> shouldBe Array.empty
        array |> Array.dropRight 5 |> shouldBe Array.empty
        (Array.empty :: Array.Array Int) |> Array.dropRight 1 |> shouldBe Array.empty
