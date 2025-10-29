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
import Test.AppSpec.AppSpec (AppSpec (..))
import Test.AppSpec.Scenario (Scenario)
import Test.AppSpec.Scenario qualified as Scenario
import Test.AppSpec.Verify qualified as Verify


specificationFor ::
  appModel -> AppSpec appModel -> AppSpec appModel
specificationFor _ spec = spec


noScenarios :: AppSpec appModel
noScenarios =
  AppSpec
    { scenarios = Array.empty
    }


scenario ::
  Text ->
  Scenario.ScenarioSteps appModel Unit ->
  AppSpec appModel
scenario name steps =
  AppSpec
    { scenarios =
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
  commandPattern -> Scenario appModel
given = panic "given: not implemented"


expect ::
  outcomePattern -> Scenario appModel
expect = panic "expect: not implemented"


and ::
  outcomePattern -> Scenario appModel
and = panic "and: not implemented"


receivedCommand :: adapter
receivedCommand = panic "receivedCommand: not implemented"


registeredEvent :: adapter
registeredEvent = panic "registeredEvent: not implemented"


executedTask :: adapter
executedTask = panic "executedTask: not implemented"