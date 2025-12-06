module Test.Service.Command where

import Core
import Test
import Test.Service.Command.Decide.Spec qualified as Decide


spec :: Spec Unit
spec = do
  describe "Command Specification Tests" do
    Decide.spec
