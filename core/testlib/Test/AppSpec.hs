module Test.AppSpec (
  AppSpec,
  specificationFor,
  Scenario,
  scenario,
  given,
  expect,
  and,
  receivedCommand,
  registeredEvent,
  executedTask,
  verifyAppSpec,
  emptyScenario,
  noScenarios,
) where

import Array qualified
import Core
import Test.AppSpec.AppSpec (AppSpec (..), AppSpecSteps (..))
import Test.AppSpec.AppSpec qualified as AppSpec
import Test.AppSpec.Scenario (Scenario)
import Test.AppSpec.Scenario qualified as Scenario
import Test.AppSpec.Verify qualified as Verify


specificationFor ::
  appModel -> AppSpecSteps appModel Unit -> AppSpec appModel
specificationFor _ steps =
  AppSpec.fromSteps steps


noScenarios :: AppSpecSteps appModel Unit
noScenarios =
  AppSpec.emptySteps


scenario ::
  Text ->
  Scenario.ScenarioSteps appModel Unit ->
  AppSpecSteps appModel Unit
scenario name steps =
  AppSpec.emptySteps
    { stepScenarios =
        Array.fromLinkedList
          [ Scenario.Scenario
              { name,
                steps
              }
          ]
    }


verifyAppSpec :: AppSpec appModel -> Task Text Unit
verifyAppSpec = Verify.run Verify.defaultOps
{-# INLINE verifyAppSpec #-}


emptyScenario :: Scenario.ScenarioSteps appModel Unit
emptyScenario = Scenario.empty


given ::
  commandPattern -> Scenario.ScenarioSteps appModel Unit
given = panic "given: not implemented"


expect ::
  outcomePattern -> Scenario.ScenarioSteps appModel Unit
expect = panic "expect: not implemented"


and ::
  outcomePattern -> Scenario.ScenarioSteps appModel Unit
and = panic "and: not implemented"


receivedCommand :: adapter
receivedCommand = panic "receivedCommand: not implemented"


registeredEvent :: adapter
registeredEvent = panic "registeredEvent: not implemented"


executedTask :: adapter
executedTask = panic "executedTask: not implemented"
