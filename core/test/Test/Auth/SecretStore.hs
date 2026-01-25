-- | Shared test suite for SecretStore implementations.
--
-- This module provides a reusable test suite that can be run against
-- any SecretStore implementation. It tests:
--
-- * Basic CRUD operations (get, put, delete, atomicModify)
-- * Concurrent access patterns
-- * Key collision scenarios
-- * Edge cases (special chars, empty values, large values)
--
-- = Usage
--
-- @
-- -- In your implementation spec:
-- module Auth.InMemorySecretStoreSpec where
--
-- import Test.Auth.SecretStore qualified as SecretStore
-- import Auth.SecretStore.InMemory qualified as InMemory
--
-- spec :: Spec Unit
-- spec = do
--   describe "InMemorySecretStore" do
--     SecretStore.spec InMemory.new
-- @
module Test.Auth.SecretStore (
  spec,
) where

import Array qualified
import AsyncTask qualified
import Auth.OAuth2.Types (
  TokenSet (..),
  mkAccessToken,
  mkRefreshToken,
 )
import Auth.SecretStore (SecretStore (..))
import Test.Auth.TestUtils qualified as TestUtils
import Basics
import Core
import Maybe qualified
import Task qualified
import Test
import Text qualified


-- | Shared test suite for any SecretStore implementation.
--
-- Takes a Task that creates a new SecretStore instance.
-- Each test group gets a fresh store.
spec :: Task Text SecretStore -> Spec Unit
spec newStore = do
  describe "Basic CRUD Operations" do
    basicCrudSpec newStore

  describe "Concurrent Access" do
    concurrentAccessSpec newStore

  describe "Key Collision Scenarios" do
    keyCollisionSpec newStore


-- | Tests for basic get/put/delete/atomicModify operations.
basicCrudSpec :: Task Text SecretStore -> Spec Unit
basicCrudSpec newStore = do
  it "get returns Nothing for non-existent key" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "non-existent"
    result <- store.get key
    result |> shouldBe Nothing

  it "put and get work for a single key" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "user-123"
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    result <- store.get key
    result |> shouldBe (Just tokens)

  it "put overwrites existing value" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "user-123"
    let tokens1 = TestUtils.makeTestTokenSet
    let tokens2 = TestUtils.makeTestTokenSetWithRefresh
    store.put key tokens1
    store.put key tokens2
    result <- store.get key
    result |> shouldBe (Just tokens2)

  it "delete removes a key" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "user-123"
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    store.delete key
    result <- store.get key
    result |> shouldBe Nothing

  it "delete on non-existent key is a no-op" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "non-existent"
    -- Should not throw
    store.delete key
    result <- store.get key
    result |> shouldBe Nothing

  it "handles special characters in keys" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "oauth2:oura:user@example.com!#$%"
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    result <- store.get key
    result |> shouldBe (Just tokens)

  it "handles empty key" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey ""
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    result <- store.get key
    result |> shouldBe (Just tokens)

  it "handles unicode characters in keys" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "用户:émoji:🔐"
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    result <- store.get key
    result |> shouldBe (Just tokens)

  it "handles long key values" \_ -> do
    store <- newStore
    -- Create a 10KB key
    let longKey = TestUtils.makeTestTokenKey (Text.repeat 10000 "x")
    let tokens = TestUtils.makeTestTokenSet
    store.put longKey tokens
    result <- store.get longKey
    result |> shouldBe (Just tokens)

  describe "atomicModify" do
    it "modifies existing value atomically" \_ -> do
      store <- newStore
      let key = TestUtils.makeTestTokenKey "user-123"
      let tokens1 = TestUtils.makeTestTokenSet
      store.put key tokens1
      -- Modify to add refresh token
      store.atomicModify key (\maybeTokens -> do
        case maybeTokens of
          Just ts -> Just ts {refreshToken = Just (mkRefreshToken "new-refresh")}
          Nothing -> Nothing)
      result <- store.get key
      case result of
        Just ts -> ts.refreshToken |> shouldBe (Just (mkRefreshToken "new-refresh"))
        Nothing -> fail "Expected token to exist"

    it "creates value if key does not exist" \_ -> do
      store <- newStore
      let key = TestUtils.makeTestTokenKey "new-user"
      let newTokens = TestUtils.makeTestTokenSet
      -- atomicModify on non-existent key creates it
      store.atomicModify key (\_ -> Just newTokens)
      result <- store.get key
      result |> shouldBe (Just newTokens)

    it "deletes value when transform returns Nothing" \_ -> do
      store <- newStore
      let key = TestUtils.makeTestTokenKey "user-to-delete"
      let tokens = TestUtils.makeTestTokenSet
      store.put key tokens
      -- Delete via atomicModify
      store.atomicModify key (\_ -> Nothing)
      result <- store.get key
      result |> shouldBe Nothing

    it "no-op when key does not exist and transform returns Nothing" \_ -> do
      store <- newStore
      let key = TestUtils.makeTestTokenKey "never-existed"
      -- Should be a no-op
      store.atomicModify key (\_ -> Nothing)
      result <- store.get key
      result |> shouldBe Nothing


-- | Tests for concurrent access patterns.
concurrentAccessSpec :: Task Text SecretStore -> Spec Unit
concurrentAccessSpec newStore = do
  it "handles concurrent reads safely" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "shared"
    let tokens = TestUtils.makeTestTokenSet
    store.put key tokens
    -- Spawn 100 concurrent reads
    tasks <-
      Array.initialize 100 identity
        |> Array.map (\_ -> AsyncTask.run (store.get key))
        |> Task.mapArray identity
    -- Wait for all reads
    results <-
      tasks
        |> Array.map AsyncTask.waitFor
        |> Task.mapArray identity
    -- All should return the same value
    results
      |> Task.forEach (\result -> result |> shouldBe (Just tokens))

  it "handles concurrent puts to different keys" \_ -> do
    store <- newStore
    -- Spawn 100 concurrent puts to different keys
    tasks <-
      Array.initialize 100 identity
        |> Array.map (\index -> do
          let key = TestUtils.makeTestTokenKey [fmt|user-#{toText index}|]
          let tokens = makeTokenSetWithIndex index
          AsyncTask.run (store.put key tokens))
        |> Task.mapArray identity
    -- Wait for all puts
    tasks
      |> Array.map AsyncTask.waitFor
      |> Task.mapArray identity
      |> discard
    -- Verify all values are correct
    Array.initialize 100 identity
      |> Task.forEach (\index -> do
        let key = TestUtils.makeTestTokenKey [fmt|user-#{toText index}|]
        result <- store.get key
        result |> shouldBe (Just (makeTokenSetWithIndex index)))

  it "handles concurrent puts to same key" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "contested"
    -- Spawn 50 concurrent puts to same key
    tasks <-
      Array.initialize 50 identity
        |> Array.map (\index -> do
          let tokens = makeTokenSetWithIndex index
          AsyncTask.run (store.put key tokens))
        |> Task.mapArray identity
    -- Wait for all puts
    tasks
      |> Array.map AsyncTask.waitFor
      |> Task.mapArray identity
      |> discard
    -- Final value should be one of the written values (last writer wins)
    result <- store.get key
    result |> shouldSatisfy (\r -> case r of Just _ -> True; Nothing -> False)

  it "atomicModify is atomic under contention" \_ -> do
    store <- newStore
    let key = TestUtils.makeTestTokenKey "counter"
    -- Start with expiresInSeconds = 0
    let initialTokens = TokenSet
          { accessToken = mkAccessToken "token"
          , refreshToken = Nothing
          , expiresInSeconds = Just 0
          }
    store.put key initialTokens
    -- Spawn 100 concurrent increments
    tasks <-
      Array.initialize 100 identity
        |> Array.map (\_ -> do
          AsyncTask.run (store.atomicModify key (\maybeTokens -> do
            case maybeTokens of
              Just ts -> do
                let current = ts.expiresInSeconds |> Maybe.withDefault 0
                Just ts {expiresInSeconds = Just (current + 1)}
              Nothing -> Nothing)))
        |> Task.mapArray identity
    -- Wait for all modifications
    tasks
      |> Array.map AsyncTask.waitFor
      |> Task.mapArray identity
      |> discard
    -- Final value should be exactly 100
    result <- store.get key
    case result of
      Just ts -> ts.expiresInSeconds |> shouldBe (Just 100)
      Nothing -> fail "Expected token to exist"

  it "handles mixed concurrent operations" \_ -> do
    store <- newStore
    -- Pre-populate some keys
    Array.initialize 25 identity
      |> Task.forEach (\i -> do
        let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
        store.put key TestUtils.makeTestTokenSet)
    -- Spawn mixed operations: 25 reads, 25 writes, 25 deletes, 25 atomicModifies
    readTasks <-
      Array.initialize 25 identity
        |> Array.map (\i -> do
          let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
          AsyncTask.run (store.get key))
        |> Task.mapArray identity
    writeTasks <-
      Array.initialize 25 (\i -> i + 25)
        |> Array.map (\i -> do
          let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
          AsyncTask.run (store.put key TestUtils.makeTestTokenSetWithRefresh))
        |> Task.mapArray identity
    deleteTasks <-
      Array.initialize 25 (\i -> i + 50)
        |> Array.map (\i -> do
          let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
          AsyncTask.run (store.delete key))
        |> Task.mapArray identity
    modifyTasks <-
      Array.initialize 25 identity
        |> Array.map (\i -> do
          let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
          AsyncTask.run (store.atomicModify key (\ts -> ts)))
        |> Task.mapArray identity
    -- Wait for all
    readTasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity |> discard
    writeTasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity |> discard
    deleteTasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity |> discard
    modifyTasks |> Array.map AsyncTask.waitFor |> Task.mapArray identity |> discard
    -- Verify written keys exist
    Array.initialize 25 (\i -> i + 25)
      |> Task.forEach (\i -> do
        let key = TestUtils.makeTestTokenKey [fmt|key-#{toText i}|]
        result <- store.get key
        result |> shouldBe (Just TestUtils.makeTestTokenSetWithRefresh))


-- | Tests for key collision and independence scenarios.
keyCollisionSpec :: Task Text SecretStore -> Spec Unit
keyCollisionSpec newStore = do
  it "keys with common prefixes are independent" \_ -> do
    store <- newStore
    let key1 = TestUtils.makeTestTokenKey "oauth2:oura:user-123"
    let key2 = TestUtils.makeTestTokenKey "oauth2:oura:user-1234"
    let key3 = TestUtils.makeTestTokenKey "oauth2:oura:user-12"
    let tokens1 = makeTokenSetWithIndex 1
    let tokens2 = makeTokenSetWithIndex 2
    let tokens3 = makeTokenSetWithIndex 3
    store.put key1 tokens1
    store.put key2 tokens2
    store.put key3 tokens3
    -- Each key should have its own value
    result1 <- store.get key1
    result2 <- store.get key2
    result3 <- store.get key3
    result1 |> shouldBe (Just tokens1)
    result2 |> shouldBe (Just tokens2)
    result3 |> shouldBe (Just tokens3)

  it "deleting one key does not affect similar keys" \_ -> do
    store <- newStore
    let key1 = TestUtils.makeTestTokenKey "prefix:key"
    let key2 = TestUtils.makeTestTokenKey "prefix:key:suffix"
    let tokens1 = makeTokenSetWithIndex 1
    let tokens2 = makeTokenSetWithIndex 2
    store.put key1 tokens1
    store.put key2 tokens2
    -- Delete key1
    store.delete key1
    -- key2 should still exist
    result1 <- store.get key1
    result2 <- store.get key2
    result1 |> shouldBe Nothing
    result2 |> shouldBe (Just tokens2)

  it "handles unicode key variations correctly" \_ -> do
    store <- newStore
    -- These look similar but are different unicode characters
    let key1 = TestUtils.makeTestTokenKey "café" -- e with acute accent
    let key2 = TestUtils.makeTestTokenKey "cafe" -- plain e
    let tokens1 = makeTokenSetWithIndex 1
    let tokens2 = makeTokenSetWithIndex 2
    store.put key1 tokens1
    store.put key2 tokens2
    result1 <- store.get key1
    result2 <- store.get key2
    result1 |> shouldBe (Just tokens1)
    result2 |> shouldBe (Just tokens2)

  it "handles very long keys with same prefix" \_ -> do
    store <- newStore
    let longPrefix = Text.repeat 1000 "a"
    let key1 = TestUtils.makeTestTokenKey [fmt|#{longPrefix}:1|]
    let key2 = TestUtils.makeTestTokenKey [fmt|#{longPrefix}:2|]
    let tokens1 = makeTokenSetWithIndex 1
    let tokens2 = makeTokenSetWithIndex 2
    store.put key1 tokens1
    store.put key2 tokens2
    result1 <- store.get key1
    result2 <- store.get key2
    result1 |> shouldBe (Just tokens1)
    result2 |> shouldBe (Just tokens2)


-- | Helper to create a TokenSet with a unique index in the access token.
makeTokenSetWithIndex :: Int -> TokenSet
makeTokenSetWithIndex index =
  TokenSet
    { accessToken = mkAccessToken [fmt|access-token-#{toText index}|]
    , refreshToken = Just (mkRefreshToken [fmt|refresh-token-#{toText index}|])
    , expiresInSeconds = Just 3600
    }
