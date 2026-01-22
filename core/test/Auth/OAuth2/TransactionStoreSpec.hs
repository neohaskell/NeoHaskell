module Auth.OAuth2.TransactionStoreSpec where

import Array qualified
import AsyncTask qualified
import Auth.OAuth2.TransactionStore (
  Transaction (..),
  TransactionStore (..),
 )
import Auth.OAuth2.TransactionStore qualified as TransactionKey
import Auth.OAuth2.TransactionStore.InMemory qualified as InMemory
import Auth.OAuth2.Types (mkCodeVerifierUnsafe)
import Core
import Result qualified
import Task qualified
import Test


-- | Helper to check if Maybe is Just
isJust :: forall value. Maybe value -> Bool
isJust maybeVal =
  case maybeVal of
    Just _ -> True
    Nothing -> False


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.TransactionStore" do
    describe "InMemory Implementation" do
      -- ========================================================================
      -- Basic Operations
      -- ========================================================================
      describe "Basic Operations" do
        it "stores and retrieves a transaction" \_ -> do
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          let expiresAt = 1700000300
          let tx = Transaction {verifier = verifier, expiresAt = expiresAt}

          -- Store
          putResult <- store.put stateKey tx |> Task.asResult
          putResult |> shouldSatisfy Result.isOk

          -- Retrieve (peek, not consume)
          getResult <- store.get stateKey |> Task.asResult
          case getResult of
            Err err -> fail [fmt|Get failed: #{toText err}|]
            Ok maybeTx -> do
              maybeTx |> shouldSatisfy isJust
              case maybeTx of
                Just retrieved -> do
                  retrieved.expiresAt |> shouldBe expiresAt
                Nothing -> fail "Expected Just"

        it "returns Nothing for non-existent key" \_ -> do
          store <- InMemory.new
          let nonExistentKey = TransactionKey.fromText "non-existent-key"
          getResult <- store.get nonExistentKey |> Task.asResult
          case getResult of
            Err err -> fail [fmt|Get failed: #{toText err}|]
            Ok maybeTx -> maybeTx |> shouldBe Nothing

        it "overwrites existing transaction with same key" \_ -> do
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier1 = mkCodeVerifierUnsafe "verifier1-43-chars-long-abcdefghijklmno"
          let verifier2 = mkCodeVerifierUnsafe "verifier2-43-chars-long-abcdefghijklmno"
          let tx1 = Transaction {verifier = verifier1, expiresAt = 1700000300}
          let tx2 = Transaction {verifier = verifier2, expiresAt = 1700000600}

          -- Store first
          _ <- store.put stateKey tx1
          -- Overwrite
          _ <- store.put stateKey tx2

          -- Should get the second one
          getResult <- store.get stateKey |> Task.asResult
          case getResult of
            Err _ -> fail "Get failed"
            Ok maybeTx -> do
              case maybeTx of
                Just retrieved -> retrieved.expiresAt |> shouldBe 1700000600
                Nothing -> fail "Expected Just"

      -- ========================================================================
      -- Atomic Consume (Critical for One-Time Use)
      -- ========================================================================
      describe "Atomic Consume" do
        it "consume returns value and deletes atomically" \_ -> do
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          let tx = Transaction {verifier = verifier, expiresAt = 1700000300}

          -- Store
          _ <- store.put stateKey tx

          -- Consume (should return value and delete)
          consumeResult <- store.consume stateKey |> Task.asResult
          case consumeResult of
            Err err -> fail [fmt|Consume failed: #{toText err}|]
            Ok maybeTx -> do
              maybeTx |> shouldSatisfy isJust

          -- Second consume should return Nothing (already deleted)
          consumeResult2 <- store.consume stateKey |> Task.asResult
          case consumeResult2 of
            Err err -> fail [fmt|Second consume failed: #{toText err}|]
            Ok maybeTx -> maybeTx |> shouldBe Nothing

        it "consume returns Nothing for non-existent key" \_ -> do
          store <- InMemory.new
          let nonExistentKey = TransactionKey.fromText "non-existent"
          consumeResult <- store.consume nonExistentKey |> Task.asResult
          case consumeResult of
            Err err -> fail [fmt|Consume failed: #{toText err}|]
            Ok maybeTx -> maybeTx |> shouldBe Nothing

        it "concurrent consumes - exactly one succeeds" \_ -> do
          -- This is the critical test for replay attack prevention
          -- Multiple concurrent attempts to consume the same state
          -- should result in exactly one success
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          let tx = Transaction {verifier = verifier, expiresAt = 1700000300}

          -- Store
          _ <- store.put stateKey tx

          -- Launch 10 concurrent consume attempts
          let consumeTask = store.consume stateKey
          results <-
            Array.fromLinkedList [1 .. 10 :: Int]
              |> Task.mapArray (\_ -> AsyncTask.run consumeTask)
              |> Task.andThen (\tasks -> tasks |> Task.mapArray AsyncTask.waitFor)

          -- Count successes (Just values)
          let successCount =
                results
                  |> Array.takeIf isJust
                  |> Array.length

          -- Exactly one should have succeeded
          successCount |> shouldBe 1

      -- ========================================================================
      -- Delete Operation
      -- ========================================================================
      describe "Delete Operation" do
        it "delete removes a transaction" \_ -> do
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          let tx = Transaction {verifier = verifier, expiresAt = 1700000300}

          -- Store
          _ <- store.put stateKey tx

          -- Delete
          deleteResult <- store.delete stateKey |> Task.asResult
          deleteResult |> shouldSatisfy Result.isOk

          -- Should be gone
          getResult <- store.get stateKey |> Task.asResult
          case getResult of
            Err _ -> fail "Get failed"
            Ok maybeTx -> maybeTx |> shouldBe Nothing

        it "delete on non-existent key is a no-op" \_ -> do
          store <- InMemory.new
          let nonExistentKey = TransactionKey.fromText "non-existent"
          deleteResult <- store.delete nonExistentKey |> Task.asResult
          -- Should not fail
          deleteResult |> shouldSatisfy Result.isOk

      -- ========================================================================
      -- Expiry Behavior
      -- ========================================================================
      describe "Expiry Behavior" do
        it "get does not filter by expiry (caller's responsibility)" \_ -> do
          -- The store just stores data; expiry checking is done by the caller
          -- This keeps the store simple and testable
          store <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          -- Already expired
          let tx = Transaction {verifier = verifier, expiresAt = 1}

          _ <- store.put stateKey tx

          -- Should still be retrievable (expiry check is caller's job)
          getResult <- store.get stateKey |> Task.asResult
          case getResult of
            Err _ -> fail "Get failed"
            Ok maybeTx -> maybeTx |> shouldSatisfy isJust

      -- ========================================================================
      -- Isolation
      -- ========================================================================
      describe "Store Isolation" do
        it "different stores are independent" \_ -> do
          store1 <- InMemory.new
          store2 <- InMemory.new
          let stateKey = TransactionKey.fromText "state-key-123"
          let verifier = mkCodeVerifierUnsafe "verifier-43-chars-long-abcdefghijklmnop"
          let tx = Transaction {verifier = verifier, expiresAt = 1700000300}

          -- Store in store1
          _ <- store1.put stateKey tx

          -- Should not be in store2
          getResult <- store2.get stateKey |> Task.asResult
          case getResult of
            Err _ -> fail "Get failed"
            Ok maybeTx -> maybeTx |> shouldBe Nothing
