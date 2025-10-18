{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.AppSpec (
  AppSpec,
  specificationFor,
  Scenarios (..),
  noScenarios,
  ScenarioDef (..),
  scenario,
  given,
  expect,
  and,
  receivedCommand,
  registeredEvent,
  executedTask,
  verifyAppSpec,
  emptyScenario,
) where

import Applicable (Applicative)
import Array qualified
import Core
import Mappable (Functor)
import Test.AppSpec.AppSpec (AppSpec (..))
import Test.AppSpec.Verify qualified as Verify
import Thenable (Monad)


verifyAppSpec :: AppSpec appModel -> Task Text Unit
verifyAppSpec = Verify.run Verify.defaultOps
{-# INLINE verifyAppSpec #-}


specificationFor ::
  appModel -> AppSpec appModel -> AppSpec appModel
specificationFor _ _ = AppSpec


scenario ::
  Text -> Scenario appModel -> AppSpec appModel
scenario _ = panic "not implemented"


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