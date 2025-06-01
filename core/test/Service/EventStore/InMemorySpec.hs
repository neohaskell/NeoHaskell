module Service.EventStore.InMemorySpec where

import Core
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Test.Service.EventStore
import Test.Service.EventStore.GlobalStreamOrdering.Spec qualified as GlobalStreamOrdering
import ToText (toText)


spec :: Spec Unit
spec = do
  describe "InMemoryEventStore" do
    describe "ability to maintain total ordering per stream" do
      it "handles 10 events" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 10

      it "handles 100 events" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 100

      it "handles 1000 events" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 1000

      it "handles 10000 events" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 10000

      it "handles 100000 events" \_ -> do
        pending "This test is only run manually"
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 100000

      it "handles 1000000 events" \_ -> do
        pending "This test is only run manually"
        store <- InMemory.new |> Task.mapError toText
        testStreamOrderingSequential store 1000000

    describe "ability to enforce optimistic concurrency control" do
      it "should handle well two concurrent operations" \_ -> do
        store <- InMemory.new |> Task.mapError toText
        testOptimisticConcurrency store

    GlobalStreamOrdering.spec (InMemory.new |> Task.mapError toText)
