module Service.CommandHandlerSpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.Command.Core (CartEvent)
import Test.Service.CommandHandler qualified as CommandHandler


spec :: Spec Unit
spec = do
  describe "CommandHandler with InMemory EventStore" do
    let newStore = InMemory.new @CartEvent |> Task.mapError toText
    CommandHandler.spec newStore
