{-# OPTIONS_GHC -Wno-unused-imports #-}

module ArraySpec where

import Array qualified
import Test

spec :: Spec Unit
spec = do
  describe "Array" do
    describe "dropLast" do
      it "removes the final element" \_ -> do
        Array.fromLinkedList [1, 2, 3] |> Array.dropLast |> shouldBe (Array.fromLinkedList [1, 2])

      it "returns the same empty array" \_ -> do
        Array.empty |> Array.dropLast |> shouldBe (Array.empty :: Array.Array Int)

    describe "dropRight" do
      it "drops the last n elements" \_ -> do
        Array.fromLinkedList [1, 2, 3, 4] |> Array.dropRight 2 |> shouldBe (Array.fromLinkedList [1, 2])

      it "returns the original array when n is zero" \_ -> do
        Array.fromLinkedList [1, 2, 3] |> Array.dropRight 0 |> shouldBe (Array.fromLinkedList [1, 2, 3])

      it "returns an empty array when n is at least the length" \_ -> do
        Array.fromLinkedList [1, 2, 3] |> Array.dropRight 3 |> shouldBe (Array.empty :: Array.Array Int)
        Array.fromLinkedList [1, 2, 3] |> Array.dropRight 5 |> shouldBe (Array.empty :: Array.Array Int)
