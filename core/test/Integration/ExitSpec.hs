{-# OPTIONS_GHC -Wno-unused-imports #-}

module Integration.ExitSpec where

import Array qualified
import Core
import Integration qualified
import Integration.Exit qualified as Exit
import Test


-- ============================================================================
-- Tests
-- ============================================================================

spec :: Spec Unit
spec = do
  describe "Integration.Exit" do
    describe "onEvent" do
      it "returns an Outbound with exactly 1 action" \_ -> do
        let outbound = Exit.onEvent () ()
        let actions = Integration.getActions outbound
        Array.length actions |> shouldBe 1

      it "ignores entity argument" \_ -> do
        let outbound1 = Exit.onEvent () ()
        let outbound2 = Exit.onEvent ("entity" :: Text) ()
        Array.length (Integration.getActions outbound1) |> shouldBe 1
        Array.length (Integration.getActions outbound2) |> shouldBe 1

      it "ignores event argument" \_ -> do
        let outbound1 = Exit.onEvent () ()
        let outbound2 = Exit.onEvent () ("event" :: Text)
        Array.length (Integration.getActions outbound1) |> shouldBe 1
        Array.length (Integration.getActions outbound2) |> shouldBe 1

      it "produces a non-empty outbound (not Integration.none)" \_ -> do
        let outbound = Exit.onEvent () ()
        let actions = Integration.getActions outbound
        Array.length actions |> shouldSatisfy (> 0)
