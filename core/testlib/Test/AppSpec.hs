{-# OPTIONS_GHC -Wno-missing-methods #-}

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
noScenarios = panic "noScenarios: not implemented"


scenario ::
  Text -> Scenario appModel -> AppSpec appModel
scenario _ = panic "not implemented"


verifyAppSpec :: AppSpec appModel -> Task Text Unit
verifyAppSpec = Verify.run Verify.defaultOps
{-# INLINE verifyAppSpec #-}


emptyScenario :: Scenario appModel
emptyScenario = panic "not implemented"


given ::
  commandPattern -> Scenario appModel
given = panic "not implemented"


expect ::
  outcomePattern -> Scenario appModel
expect = panic "not implemented"


and ::
  outcomePattern -> Scenario appModel
and = panic "not implemented"


receivedCommand :: adapter
receivedCommand = panic "not implemented"


registeredEvent :: adapter
registeredEvent = panic "not implemented"


executedTask :: adapter
executedTask = panic "not implemented"