module Test.AppSpec.Verify where

import Core
import Task qualified
import Test.AppSpec.AppSpec
import Test.AppSpec.Scenario (Scenario)


run :: Ops appModel -> AppSpec appModel -> Task Text Unit
run ops spec =
  spec.scenarios
    |> Task.forEach ops.verifyScenario


data Ops appModel = Ops
  { verifyScenario :: Scenario appModel -> Task Text Unit
  }


defaultOps :: Ops appModel
defaultOps = do
  let verifyScenario :: Scenario appModel -> Task Text Unit
      verifyScenario = panic "verifyScenario: not implemented"
  Ops {verifyScenario}
