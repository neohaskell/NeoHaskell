module Test.AppSpec.Verify where

import Core
import Task qualified
import Test.AppSpec.AppSpec


run :: Ops -> AppSpec appModel -> Task Text Unit
run _ _ = do
  Task.yield unit


data Ops = Ops


defaultOps :: Ops
defaultOps = Ops