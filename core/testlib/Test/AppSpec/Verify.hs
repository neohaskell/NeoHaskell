module Test.AppSpec.Verify where

import Core
import Test.AppSpec.Core


run :: ops -> AppSpec appModel -> Task Text Unit
run = panic "not implemented"


data Ops = Ops


defaultOps :: Ops
defaultOps = Ops