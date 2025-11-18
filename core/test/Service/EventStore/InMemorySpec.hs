module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.Entity qualified as Entity
import Test.Service.Entity.Core qualified as EntityCore
import Test.Service.EventStore qualified as EventStore


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    let newStore = InMemory.new @EntityCore.BankAccountEvent |> Task.mapError toText
    EventStore.spec newStore

    let newStoreAndReducer = do
          store <- newStore
          reducer <- EntityCore.newReducer store |> Task.mapError toText
          Task.yield (store, reducer)

    Entity.spec newStoreAndReducer
