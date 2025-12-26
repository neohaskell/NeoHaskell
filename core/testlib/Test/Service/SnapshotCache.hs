module Test.Service.SnapshotCache where

import Core
import Service.SnapshotCache.Core (SnapshotCache)
import Test
import Test.Service.SnapshotCache.Core (CartState)
import Test.Service.SnapshotCache.InMemory.Spec qualified as InMemory


spec :: Task Text (SnapshotCache CartState) -> Spec Unit
spec newCache = do
  describe "Snapshot Cache Specification Tests" do
    InMemory.spec newCache
