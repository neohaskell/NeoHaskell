module Service.EventStore.InMemorySpec where

import Array qualified
import AsyncIO qualified
import Core
import GHC.IO qualified as GHC
import Maybe qualified
import Service.Event (Event (..))
import Service.Event qualified as Event
import Service.EventStore (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EventStore (testOptimisticConcurrency, testStreamOrderingSequential)
import ToText (toText)


spec :: Spec
spec = do
  describe "InMemoryEventStore" do
    describe "ability to maintain total ordering per stream" do
      it "handles 10 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 10

      it "handles 100 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 100

      it "handles 1000 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 1000

      it "handles 10000 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 10000

      xit "handles 100000 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 100000

      xit "handles 1000000 events" do
        store <- InMemory.new |> runTask
        testStreamOrderingSequential store 1000000

    describe "ability to enforce optimistic concurrency control" do
      it "should handle well two concurrent operations" do
        store <- InMemory.new |> runTask
        testOptimisticConcurrency store
