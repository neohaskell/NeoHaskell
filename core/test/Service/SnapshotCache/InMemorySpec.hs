module Service.SnapshotCache.InMemorySpec where

import Core
import Service.SnapshotCache.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.SnapshotCache qualified as SnapshotCache
import Test.Service.SnapshotCache.Core (CartState)


spec :: Spec Unit
spec = do
  describe "InMemorySnapshotCache" do
    let newCache = InMemory.new @CartState |> Task.mapError toText
    SnapshotCache.spec newCache
