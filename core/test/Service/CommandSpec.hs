module Service.CommandSpec where

import Core
import Test
import Test.Service.Command qualified as Command


spec :: Spec Unit
spec = do
  describe "Service.Command" do
    Command.spec