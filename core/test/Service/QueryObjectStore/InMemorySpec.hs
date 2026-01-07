module Service.QueryObjectStore.InMemorySpec where

import Core
import Service.QueryObjectStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.QueryObjectStore qualified as QueryObjectStore


spec :: Spec Unit
spec = do
  describe "InMemoryQueryObjectStore" do
    let newStore = InMemory.new |> Task.mapError toText
    QueryObjectStore.spec newStore
