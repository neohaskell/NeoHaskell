{-# OPTIONS_GHC -Wno-missing-methods #-}

module Test.AppModel (
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
) where

import Applicable (Applicative)
import Core
import Mappable (Functor)
import Test.AppModel.AppSpec (AppSpec)
import Test.AppModel.AppSpec qualified as AppSpec
import Test.Spec (Spec)
import Thenable (Monad)


verifyAppSpec :: AppSpec appModel -> Spec Unit
verifyAppSpec =
  AppSpec.verify
{-# INLINE verifyAppSpec #-}


specificationFor ::
  appModel -> t appModel -> AppSpec appModel
specificationFor _ = panic "not implemented"


data ScenarioDef (appModel :: Type)
  deriving (Functor)


instance Applicative ScenarioDef


instance Monad ScenarioDef


scenario ::
  Text -> ScenarioDef appModel -> t appModel
scenario _ = panic "not implemented"


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