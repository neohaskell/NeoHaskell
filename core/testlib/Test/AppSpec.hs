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
import Test.AppSpec.Verify qualified as Verify


specificationFor ::
  appModel -> AppSpec appModel -> AppSpec appModel
specificationFor _ _ =
  AppSpec
    { scenarios = Array.empty
    }


noScenarios :: AppSpec appModel
noScenarios =
  AppSpec
    { scenarios = Array.empty
    }


scenario ::
  Text -> Scenario appModel -> AppSpec appModel
scenario _ = panic "scenario: not implemented"


verifyAppSpec :: AppSpec appModel -> Task Text Unit
verifyAppSpec = Verify.run Verify.defaultOps
{-# INLINE verifyAppSpec #-}


emptyScenario :: Scenario appModel
emptyScenario = panic "emptyScenario: not implemented"


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