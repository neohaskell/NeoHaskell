module Test.AppModel.AppSpec (
  AppSpec,
  verify,
  Ops (..),
  defaultOps,
) where

import Core
import Test.Spec


data AppSpec (appModel :: Type)


verify :: ops -> AppSpec appModel -> Spec Unit
verify = panic "not implemented"


data Ops = Ops


defaultOps :: Ops
defaultOps = Ops