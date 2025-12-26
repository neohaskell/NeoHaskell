module Test.Service.SnapshotCache.InMemory.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.SnapshotCache.Core (SnapshotCache)
import Service.SnapshotCache.Core qualified as SnapshotCache
import Service.SnapshotCache.Snapshot (Snapshot (..), SnapshotKey (..))
import Task qualified
import Test
import Test.Service.EntityFetcher.Core (CartState (..), initialState)
import Test.Service.SnapshotCache.InMemory.Context qualified as Context


spec ::
  Task Text (SnapshotCache CartState) ->
  Spec Unit
spec newCache = do
  describe "Snapshot Cache InMemory" do
    describe "Basic Operations" do
      before (Context.initialize newCache) do
        it "get returns Nothing for non-existent snapshot" \context -> do
          result <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          result |> shouldBe Nothing

        it "set stores a snapshot and get retrieves it" \context -> do
          let testState =
                CartState
                  { cartId = Nothing,
                    cartItems = Array.empty,
                    isCheckedOut = False,
                    version = 5
                  }
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState,
                    position = Event.StreamPosition 4
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          result <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected to find snapshot but got Nothing"
            Just retrieved -> do
              retrieved.state.version |> shouldBe 5
              retrieved.position |> shouldBe (Event.StreamPosition 4)

        it "set overwrites existing snapshot" \context -> do
          let testState1 =
                initialState {version = 1}
          let snapshot1 =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState1,
                    position = Event.StreamPosition 0
                  }

          context.cache.set snapshot1
            |> Task.mapError toText
            |> discard

          let testState2 =
                initialState {version = 10}
          let snapshot2 =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState2,
                    position = Event.StreamPosition 9
                  }

          context.cache.set snapshot2
            |> Task.mapError toText
            |> discard

          result <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected to find snapshot but got Nothing"
            Just retrieved -> do
              retrieved.state.version |> shouldBe 10
              retrieved.position |> shouldBe (Event.StreamPosition 9)

        it "delete removes a snapshot" \context -> do
          let testState = initialState {version = 3}
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState,
                    position = Event.StreamPosition 2
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          context.cache.delete context.entityName context.streamId
            |> Task.mapError toText
            |> discard

          result <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          result |> shouldBe Nothing

        it "delete on non-existent key does not fail" \context -> do
          result <-
            context.cache.delete context.entityName context.streamId
              |> Task.mapError toText
              |> Task.asResult

          result |> shouldSatisfy (\r -> case r of Ok _ -> True; Err _ -> False)

        it "clear removes all snapshots" \context -> do
          streamId2 <- StreamId.new

          let testState1 = initialState {version = 1}
          let snapshot1 =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState1,
                    position = Event.StreamPosition 0
                  }

          let testState2 = initialState {version = 2}
          let snapshot2 =
                Snapshot
                  { key = SnapshotKey context.entityName streamId2,
                    state = testState2,
                    position = Event.StreamPosition 1
                  }

          context.cache.set snapshot1
            |> Task.mapError toText
            |> discard

          context.cache.set snapshot2
            |> Task.mapError toText
            |> discard

          context.cache.clear
            |> Task.mapError toText
            |> discard

          result1 <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          result2 <-
            context.cache.get context.entityName streamId2
              |> Task.mapError toText

          result1 |> shouldBe Nothing
          result2 |> shouldBe Nothing

    describe "Multiple Entities" do
      before (Context.initialize newCache) do
        it "caches different entities independently" \context -> do
          streamId2 <- StreamId.new
          let entityName2 = Event.EntityName "Order"

          let testState1 = initialState {version = 10}
          let snapshot1 =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState1,
                    position = Event.StreamPosition 9
                  }

          let testState2 = initialState {version = 20}
          let snapshot2 =
                Snapshot
                  { key = SnapshotKey entityName2 streamId2,
                    state = testState2,
                    position = Event.StreamPosition 19
                  }

          context.cache.set snapshot1
            |> Task.mapError toText
            |> discard

          context.cache.set snapshot2
            |> Task.mapError toText
            |> discard

          result1 <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          result2 <-
            context.cache.get entityName2 streamId2
              |> Task.mapError toText

          case (result1, result2) of
            (Just r1, Just r2) -> do
              r1.state.version |> shouldBe 10
              r2.state.version |> shouldBe 20
            _ -> fail "Expected both snapshots to exist"

        it "same entity name with different stream IDs are independent" \context -> do
          streamId2 <- StreamId.new

          let testState1 = initialState {version = 5}
          let snapshot1 =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState1,
                    position = Event.StreamPosition 4
                  }

          let testState2 = initialState {version = 15}
          let snapshot2 =
                Snapshot
                  { key = SnapshotKey context.entityName streamId2,
                    state = testState2,
                    position = Event.StreamPosition 14
                  }

          context.cache.set snapshot1
            |> Task.mapError toText
            |> discard

          context.cache.set snapshot2
            |> Task.mapError toText
            |> discard

          result1 <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          result2 <-
            context.cache.get context.entityName streamId2
              |> Task.mapError toText

          case (result1, result2) of
            (Just r1, Just r2) -> do
              r1.state.version |> shouldBe 5
              r2.state.version |> shouldBe 15
            _ -> fail "Expected both snapshots to exist"

    describe "Concurrent Access" do
      before (Context.initialize newCache) do
        it "handles concurrent reads safely" \context -> do
          let testState = initialState {version = 42}
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState,
                    position = Event.StreamPosition 41
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          tasks <-
            Array.initialize 10 identity
              |> Array.map
                ( \_ -> do
                    AsyncTask.run
                      ( context.cache.get context.entityName context.streamId
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          results <-
            tasks
              |> Array.map AsyncTask.waitFor
              |> Task.mapArray identity

          results
            |> Task.forEach \result -> do
              case result of
                Nothing -> fail "Expected to find snapshot"
                Just r -> r.state.version |> shouldBe 42

        it "handles concurrent writes safely" \context -> do
          tasks <-
            Array.initialize 10 identity
              |> Array.map
                ( \index -> do
                    let testState = initialState {version = index}
                    let snapshot =
                          Snapshot
                            { key = SnapshotKey context.entityName context.streamId,
                              state = testState,
                              position = Event.StreamPosition (fromIntegral index)
                            }
                    AsyncTask.run
                      ( context.cache.set snapshot
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          tasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity
            |> discard

          result <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected to find snapshot after concurrent writes"
            Just _ -> pass

        it "handles concurrent reads and writes safely" \context -> do
          let testState = initialState {version = 100}
          let snapshot =
                Snapshot
                  { key = SnapshotKey context.entityName context.streamId,
                    state = testState,
                    position = Event.StreamPosition 99
                  }

          context.cache.set snapshot
            |> Task.mapError toText
            |> discard

          readTasks <-
            Array.initialize 5 identity
              |> Array.map
                ( \_ -> do
                    AsyncTask.run
                      ( context.cache.get context.entityName context.streamId
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          writeTasks <-
            Array.initialize 5 identity
              |> Array.map
                ( \index -> do
                    let newState = initialState {version = 100 + index}
                    let newSnapshot =
                          Snapshot
                            { key = SnapshotKey context.entityName context.streamId,
                              state = newState,
                              position = Event.StreamPosition (fromIntegral (99 + index))
                            }
                    AsyncTask.run
                      ( context.cache.set newSnapshot
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          readTasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity
            |> discard

          writeTasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity
            |> discard

          finalResult <-
            context.cache.get context.entityName context.streamId
              |> Task.mapError toText

          case finalResult of
            Nothing -> fail "Expected snapshot to exist after concurrent operations"
            Just r -> r.state.version |> shouldSatisfy (\v -> v >= 100)
