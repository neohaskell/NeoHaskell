module Service.EntitySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.Entity qualified as Entity
import Test.Service.Entity.Core qualified as EntityCore


spec :: Spec Unit
spec = do
  describe "Entity Reducer" do
    let newStore = InMemory.new |> Task.mapError toText
    let newReducer = do
          store <- newStore
          EntityCore.newReducer store |> Task.mapError toText
    Entity.spec newStore newReducer
