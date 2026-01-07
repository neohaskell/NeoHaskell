module Test.Service.QueryObjectStore.InMemory.Spec where

import Array qualified
import AsyncTask qualified
import Core
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Task qualified
import Test
import Test.Service.QueryObjectStore.Core (TestQuery (..))
import Test.Service.QueryObjectStore.InMemory.Context qualified as Context


spec ::
  Task Text (QueryObjectStore TestQuery) ->
  Spec Unit
spec newStore = do
  describe "QueryObjectStore InMemory" do
    describe "Basic Operations" do
      before (Context.initialize newStore) do
        it "get returns Nothing for non-existent ID" \context -> do
          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          result |> shouldBe Nothing

        it "atomicUpdate creates new entry when None" \context -> do
          let testQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "test",
                    count = 1
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
            |> Task.mapError toText

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected to find query but got Nothing"
            Just retrieved -> do
              retrieved.name |> shouldBe "test"
              retrieved.count |> shouldBe 1

        it "atomicUpdate updates existing entry" \context -> do
          let testQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "initial",
                    count = 1
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
            |> Task.mapError toText

          context.store.atomicUpdate
            context.testId1
            ( \maybeExisting -> case maybeExisting of
                Just existing -> Just existing {count = existing.count + 10}
                Nothing -> Nothing
            )
            |> Task.mapError toText

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected to find query but got Nothing"
            Just retrieved -> do
              retrieved.name |> shouldBe "initial"
              retrieved.count |> shouldBe 11

        it "atomicUpdate can delete by returning Nothing" \context -> do
          let testQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "to-delete",
                    count = 1
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
            |> Task.mapError toText

          context.store.atomicUpdate context.testId1 (\_ -> Nothing)
            |> Task.mapError toText

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          result |> shouldBe Nothing

        it "delete removes entry" \context -> do
          let testQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "to-delete",
                    count = 5
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
            |> Task.mapError toText

          context.store.delete context.testId1
            |> Task.mapError toText

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          result |> shouldBe Nothing

        it "delete on non-existent ID does not fail" \context -> do
          result <-
            context.store.delete context.testId1
              |> Task.mapError toText
              |> Task.asResult

          result |> shouldSatisfy (\r -> case r of Ok _ -> True; Err _ -> False)

        it "getAll returns all entries" \context -> do
          let query1 =
                TestQuery
                  { queryId = context.testId1,
                    name = "first",
                    count = 1
                  }
          let query2 =
                TestQuery
                  { queryId = context.testId2,
                    name = "second",
                    count = 2
                  }
          let query3 =
                TestQuery
                  { queryId = context.testId3,
                    name = "third",
                    count = 3
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just query1)
            |> Task.mapError toText
          context.store.atomicUpdate context.testId2 (\_ -> Just query2)
            |> Task.mapError toText
          context.store.atomicUpdate context.testId3 (\_ -> Just query3)
            |> Task.mapError toText

          results <-
            context.store.getAll
              |> Task.mapError toText

          results |> Array.length |> shouldBe 3

        it "getAll returns empty array for empty store" \context -> do
          results <-
            context.store.getAll
              |> Task.mapError toText

          results |> Array.length |> shouldBe 0

    describe "Concurrent Access" do
      before (Context.initialize newStore) do
        it "handles concurrent reads safely" \context -> do
          let testQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "concurrent-read",
                    count = 42
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
            |> Task.mapError toText

          tasks <-
            Array.initialize 10 identity
              |> Array.map
                ( \_ -> do
                    AsyncTask.run
                      ( context.store.get context.testId1
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
                Nothing -> fail "Expected to find query"
                Just r -> r.count |> shouldBe 42

        it "handles concurrent writes safely" \context -> do
          tasks <-
            Array.initialize 10 identity
              |> Array.map
                ( \index -> do
                    let testQuery =
                          TestQuery
                            { queryId = context.testId1,
                              name = "concurrent-write",
                              count = index
                            }
                    AsyncTask.run
                      ( context.store.atomicUpdate context.testId1 (\_ -> Just testQuery)
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          tasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity
            |> discard

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected query to exist after concurrent writes"
            Just _ -> pass

        it "atomicUpdate handles concurrent updates correctly" \context -> do
          -- Start with count = 0
          let initialQuery =
                TestQuery
                  { queryId = context.testId1,
                    name = "counter",
                    count = 0
                  }

          context.store.atomicUpdate context.testId1 (\_ -> Just initialQuery)
            |> Task.mapError toText

          -- Concurrently increment the count 10 times
          tasks <-
            Array.initialize 10 identity
              |> Array.map
                ( \_ -> do
                    AsyncTask.run
                      ( context.store.atomicUpdate
                          context.testId1
                          ( \maybeExisting -> case maybeExisting of
                              Just existing -> Just existing {count = existing.count + 1}
                              Nothing -> Nothing
                          )
                          |> Task.mapError toText
                      )
                )
              |> Task.mapArray identity

          tasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity
            |> discard

          result <-
            context.store.get context.testId1
              |> Task.mapError toText

          case result of
            Nothing -> fail "Expected query to exist after concurrent updates"
            Just r -> r.count |> shouldBe 10
