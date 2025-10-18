{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.AppSpec (
  AppSpec,
  specificationFor,
  ScenarioDef,
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

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Test.AppSpec.Core (AppSpec)
import Test.AppSpec.Verify qualified as Verify
import Thenable (Monad)


verifyAppSpec :: AppSpec appModel -> Task Text Unit
verifyAppSpec = Verify.run Verify.defaultOps
{-# INLINE verifyAppSpec #-}


specificationFor ::
  appModel -> Scenarios appModel -> AppSpec appModel
specificationFor _ = panic "not implemented"


data Scenarios (appModel :: Type)
  deriving (Functor)


instance Applicative Scenarios


instance Monad Scenarios


noScenarios :: Scenarios appModel
noScenarios = panic "not implemented"


data ScenarioDef (appModel :: Type)
  deriving (Functor)


instance Applicative ScenarioDef


instance Monad ScenarioDef


scenario ::
  Text -> ScenarioDef appModel -> Scenarios appModel
scenario _ = panic "not implemented"


emptyScenario :: ScenarioDef appModel
emptyScenario = panic "not implemented"


given ::
  commandPattern -> ScenarioDef appModel
given = panic "not implemented"


expect ::
  outcomePattern -> ScenarioDef appModel
expect = panic "not implemented"


and ::
  outcomePattern -> ScenarioDef appModel
and = panic "not implemented"


receivedCommand :: adapter
receivedCommand = panic "not implemented"


registeredEvent :: adapter
registeredEvent = panic "not implemented"


executedTask :: adapter
executedTask = panic "not implemented"