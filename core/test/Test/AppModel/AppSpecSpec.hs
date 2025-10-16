module Test.AppModel.AppSpecSpec where

import Core
import Task qualified
import Test
import Test.AppSpec.Core (AppSpec (..))
import Test.AppSpec.Verify qualified as Verify


spec :: Spec Unit
spec =
  describe "AppSpec" do
    describe "verify" do
      it "does not verify any scenario if there aren't any" \_ -> do
        let appSpec = AppSpec @MyAppModel
        ops <- mockVerifyOps

        Verify.run ops appSpec

        ops.verifyScenarioCalls
          |> testRef shouldBe 0


data MyAppModel = MyAppModel


mockVerifyOps :: Task Text Verify.Ops
mockVerifyOps =
  Task.yield Verify.Ops