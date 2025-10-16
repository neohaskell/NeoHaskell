module Test.AppModel.AppSpecSpec where

import Core
import Task qualified
import Test
import Test.AppSpec.Core (AppSpec (..))
import Test.AppSpec.Verify qualified as Verify
import Var qualified


data MyAppModel = MyAppModel


spec :: Spec Unit
spec =
  describe "AppSpec" do
    describe "verify" do
      it "does not verify any scenario if there aren't any" \_ -> do
        let appSpec = AppSpec @MyAppModel
        (ops, observe) <- mockVerifyOps

        Verify.run ops appSpec

        observe.verifyScenarioCalls
          |> varContents shouldBe 0


data VerifyObserve = VerifyObserve
  { verifyScenarioCalls :: Var Int
  }


mockVerifyOps :: Task Text (Verify.Ops, VerifyObserve)
mockVerifyOps = do
  verifyScenarioCalls <- Var.new 0
  let verifyObserver = VerifyObserve {verifyScenarioCalls}
  Task.yield (Verify.Ops, verifyObserver)