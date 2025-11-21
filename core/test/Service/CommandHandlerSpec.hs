module Service.CommandHandlerSpec where

import Core
import Test
import Test.Service.CommandHandler qualified as CommandHandler


spec :: Spec Unit
spec = do
  describe "CommandHandler with InMemory EventStore" do
    CommandHandler.spec
