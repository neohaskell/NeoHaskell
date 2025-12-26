module ConcurrentMapSpec where

import Array qualified
import AsyncTask qualified
import Basics
import ConcurrentMap qualified
import Core
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "ConcurrentMap" do
    describe "Basic Operations" do
      it "new creates an empty map" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        len <- ConcurrentMap.length cmap
        len |> shouldBe 0

      it "set and get work for single entry" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 42 cmap
        result <- ConcurrentMap.get "key1" cmap
        result |> shouldBe (Just 42)

      it "get returns Nothing for non-existent key" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        result <- ConcurrentMap.get "missing" cmap
        result |> shouldBe Nothing

      it "set overwrites existing value" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 42 cmap
        ConcurrentMap.set "key1" 100 cmap
        result <- ConcurrentMap.get "key1" cmap
        result |> shouldBe (Just 100)

      it "remove deletes a key" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 42 cmap
        ConcurrentMap.remove "key1" cmap
        result <- ConcurrentMap.get "key1" cmap
        result |> shouldBe Nothing

      it "remove on non-existent key does not fail" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        result <- ConcurrentMap.remove "missing" cmap |> Task.asResult
        (result :: Result Text Unit) |> shouldSatisfy (\r -> case r of Ok _ -> True; Err _ -> False)

      it "contains returns True for existing key" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 42 cmap
        result <- ConcurrentMap.contains "key1" cmap
        result |> shouldBe True

      it "contains returns False for non-existent key" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        result <- ConcurrentMap.contains "missing" cmap
        result |> shouldBe False

      it "clear removes all entries" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 1 cmap
        ConcurrentMap.set "key2" 2 cmap
        ConcurrentMap.set "key3" 3 cmap
        ConcurrentMap.clear cmap
        len <- ConcurrentMap.length cmap
        len |> shouldBe 0

      it "length returns correct count" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "key1" 1 cmap
        ConcurrentMap.set "key2" 2 cmap
        ConcurrentMap.set "key3" 3 cmap
        len <- ConcurrentMap.length cmap
        len |> shouldBe 3

    describe "Collection Operations" do
      it "keys returns all keys" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "a" 1 cmap
        ConcurrentMap.set "b" 2 cmap
        ks <- ConcurrentMap.keys cmap
        Array.length ks |> shouldBe 2

      it "values returns all values" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "a" 1 cmap
        ConcurrentMap.set "b" 2 cmap
        vs <- ConcurrentMap.values cmap
        Array.length vs |> shouldBe 2

      it "entries returns all key-value pairs" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "a" 1 cmap
        ConcurrentMap.set "b" 2 cmap
        es <- ConcurrentMap.entries cmap
        Array.length es |> shouldBe 2

      it "keys returns empty array for empty map" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ks <- ConcurrentMap.keys cmap
        Array.length ks |> shouldBe 0

      it "values returns empty array for empty map" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        vs <- ConcurrentMap.values cmap
        Array.length vs |> shouldBe 0

      it "entries returns empty array for empty map" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        es <- ConcurrentMap.entries cmap
        Array.length es |> shouldBe 0

    describe "Multiple Keys" do
      it "stores and retrieves multiple keys independently" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "first" 1 cmap
        ConcurrentMap.set "second" 2 cmap
        ConcurrentMap.set "third" 3 cmap

        result1 <- ConcurrentMap.get "first" cmap
        result2 <- ConcurrentMap.get "second" cmap
        result3 <- ConcurrentMap.get "third" cmap

        result1 |> shouldBe (Just 1)
        result2 |> shouldBe (Just 2)
        result3 |> shouldBe (Just 3)

      it "removing one key does not affect others" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "keep1" 1 cmap
        ConcurrentMap.set "remove" 2 cmap
        ConcurrentMap.set "keep2" 3 cmap

        ConcurrentMap.remove "remove" cmap

        result1 <- ConcurrentMap.get "keep1" cmap
        result2 <- ConcurrentMap.get "remove" cmap
        result3 <- ConcurrentMap.get "keep2" cmap

        result1 |> shouldBe (Just 1)
        result2 |> shouldBe Nothing
        result3 |> shouldBe (Just 3)

    describe "Edge Cases" do
      it "handles integer keys" \_ -> do
        cmap <- ConcurrentMap.new @Int @Text
        ConcurrentMap.set 1 "one" cmap
        ConcurrentMap.set 2 "two" cmap
        result <- ConcurrentMap.get 1 cmap
        result |> shouldBe (Just "one")

      it "handles empty string key" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "" 42 cmap
        result <- ConcurrentMap.get "" cmap
        result |> shouldBe (Just 42)

      it "handles large number of entries" \_ -> do
        cmap <- ConcurrentMap.new @Int @Int
        Array.initialize 1000 identity
          |> Task.forEach \i -> do
            ConcurrentMap.set i (i * 2) cmap
        len <- ConcurrentMap.length cmap
        len |> shouldBe 1000

        result <- ConcurrentMap.get 500 cmap
        result |> shouldBe (Just 1000)

    describe "Concurrent Access" do
      it "handles concurrent reads safely" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "shared" 42 cmap

        tasks <-
          Array.initialize 100 identity
            |> Array.map
              ( \_ -> do
                  AsyncTask.run (ConcurrentMap.get "shared" cmap)
              )
            |> Task.mapArray identity

        results <-
          tasks
            |> Array.map AsyncTask.waitFor
            |> Task.mapArray identity

        results
          |> Task.forEach \result -> do
            result |> shouldBe (Just 42)

      it "handles concurrent writes to different keys safely" \_ -> do
        cmap <- ConcurrentMap.new @Int @Int

        tasks <-
          Array.initialize 100 identity
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.set index (index * 2) cmap)
              )
            |> Task.mapArray identity

        tasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        len <- ConcurrentMap.length cmap
        len |> shouldBe 100

        result <- ConcurrentMap.get 50 cmap
        result |> shouldBe (Just 100)

      it "handles concurrent writes to same key safely" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int

        tasks <-
          Array.initialize 100 identity
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.set "same" index cmap)
              )
            |> Task.mapArray identity

        tasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        result <- ConcurrentMap.get "same" cmap
        result |> shouldSatisfy (\r -> case r of Just _ -> True; Nothing -> False)

      it "handles concurrent reads and writes safely" \_ -> do
        cmap <- ConcurrentMap.new @Text @Int
        ConcurrentMap.set "counter" 0 cmap

        writeTasks <-
          Array.initialize 50 identity
            |> Array.map
              ( \index -> do
                  let key = [fmt|key-#{toText index}|]
                  AsyncTask.run (ConcurrentMap.set key index cmap)
              )
            |> Task.mapArray identity

        readTasks <-
          Array.initialize 50 identity
            |> Array.map
              ( \_ -> do
                  AsyncTask.run (ConcurrentMap.get "counter" cmap)
              )
            |> Task.mapArray identity

        writeTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        readTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        len <- ConcurrentMap.length cmap
        len |> shouldBeGreaterThanOrEqual 50

      it "handles concurrent removes safely" \_ -> do
        cmap <- ConcurrentMap.new @Int @Int

        Array.initialize 100 identity
          |> Task.forEach \i -> do
            ConcurrentMap.set i i cmap

        removeTasks <-
          Array.initialize 50 identity
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.remove index cmap)
              )
            |> Task.mapArray identity

        removeTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        len <- ConcurrentMap.length cmap
        len |> shouldBe 50

      it "handles mixed concurrent operations safely" \_ -> do
        cmap <- ConcurrentMap.new @Int @Int

        Array.initialize 50 identity
          |> Task.forEach \i -> do
            ConcurrentMap.set i i cmap

        setTasks <-
          Array.initialize 25 (\i -> i + 50)
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.set index index cmap)
              )
            |> Task.mapArray identity

        removeTasks <-
          Array.initialize 25 identity
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.remove index cmap)
              )
            |> Task.mapArray identity

        readTasks <-
          Array.initialize 25 (\i -> i + 25)
            |> Array.map
              ( \index -> do
                  AsyncTask.run (ConcurrentMap.get index cmap)
              )
            |> Task.mapArray identity

        setTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        removeTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        readTasks
          |> Array.map AsyncTask.waitFor
          |> Task.mapArray identity
          |> discard

        len <- ConcurrentMap.length cmap
        len |> shouldBeGreaterThanOrEqual 25
