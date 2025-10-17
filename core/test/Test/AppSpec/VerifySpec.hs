module Test.AppSpec.VerifySpec where

import Core
import Task qualified
import Test
import Test.AppSpec.Core (AppSpec (..))
import Test.AppSpec.Verify qualified as Verify
import Var qualified


data MyAppModel = MyAppModel


testAppModel :: MyAppModel
testAppModel = MyAppModel


spec :: Spec Unit
spec =
  describe "verification" do
    it "does not verify any scenario if there aren't any" \_ -> do
      let appSpec = AppSpec @MyAppModel
      (ops, observe) <- mockVerifyOps

      Verify.run ops appSpec

      observe.verifyScenarioCalls
        |> varContents shouldBe 0

    it "does verify a single scenario" \_ -> do
      let appSpec =
            specificationFor testAppModel do
              scenario "test scenario" do
                emptyScenario

      (ops, observe) <- mockVerifyOps

      Verify.run ops appSpec

      observe.verifyScenarioCalls
        |> varContents shouldBe 1


data VerifyObserve = VerifyObserve
  { verifyScenarioCalls :: Var Int
  }


mockVerifyOps :: Task Text (Verify.Ops, VerifyObserve)
mockVerifyOps = do
  verifyScenarioCalls <- Var.new 0
  let verifyObserver = VerifyObserve {verifyScenarioCalls}
  Task.yield (Verify.Ops, verifyObserver)