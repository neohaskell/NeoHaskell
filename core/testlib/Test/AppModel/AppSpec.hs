module Test.AppModel.AppSpec where

import Core
import Test.Spec


data AppSpec (appModel :: Type)


verify :: AppSpec appModel -> Spec Unit
verify = panic "not implemented"