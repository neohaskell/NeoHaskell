module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    let newStore = InMemory.new |> Task.mapError toText
    EventStore.spec newStore
