-- | In-memory TransactionStore implementation.
--
-- WARNING: This implementation is for DEVELOPMENT ONLY.
--
-- * Data is lost on process restart
-- * NOT suitable for multi-instance deployments (breaks one-time use guarantee)
-- * Not encrypted at rest
--
-- For production with multiple instances, use Redis (GETDEL for atomic consume).
--
-- = Usage
--
-- @
-- store <- TransactionStore.InMemory.new
--
-- let key = TransactionKey.fromText stateToken
-- let tx = Transaction { verifier = verifier, expiresAt = now + 300 }
-- store.put key tx
--
-- -- Later, in callback:
-- let key = TransactionKey.fromText stateToken
-- maybeTx <- store.consume key  -- Atomically get + delete
-- @
module Auth.OAuth2.TransactionStore.InMemory (
  new,
) where

import Auth.OAuth2.TransactionStore (Transaction, TransactionKey, TransactionStore (..))
import Auth.OAuth2.TransactionStore.TransactionKey qualified as TransactionKey
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | Create a new in-memory TransactionStore.
--
-- WARNING: Development only. See module documentation.
--
-- The returned store guarantees atomic `consume` semantics within
-- a single process. Multiple instances will break the one-time use
-- guarantee - use Redis for production multi-instance deployments.
--
-- @
-- store <- TransactionStore.InMemory.new
-- @
new :: Task Text TransactionStore
new = do
  storage <- ConcurrentVar.containing Map.empty
  Task.yield
    TransactionStore
      { get = getImpl storage
      , put = putImpl storage
      , consume = consumeImpl storage
      , delete = deleteImpl storage
      }


-- | Internal storage type.
-- Uses Text representation of TransactionKey for Map storage.
type Storage = ConcurrentVar (Map Text Transaction)


getImpl :: Storage -> TransactionKey -> Task Text (Maybe Transaction)
getImpl storage key = do
  let keyText = TransactionKey.toText key
  store <- storage |> ConcurrentVar.peek
  store |> Map.get keyText |> Task.yield


putImpl :: Storage -> TransactionKey -> Transaction -> Task Text Unit
putImpl storage key tx = do
  let keyText = TransactionKey.toText key
  storage |> ConcurrentVar.modify (\store -> store |> Map.set keyText tx)


-- | Atomically consume a transaction (get + delete in one operation).
--
-- This is the critical operation for one-time use / replay prevention.
-- Uses ConcurrentVar.modifyReturning to ensure atomicity:
-- - Reads current value
-- - Deletes entry
-- - Returns the value that was read
--
-- Concurrent calls with the same key: exactly one gets the value,
-- all others get Nothing.
consumeImpl :: Storage -> TransactionKey -> Task Text (Maybe Transaction)
consumeImpl storage key = do
  let keyText = TransactionKey.toText key
  storage
    |> ConcurrentVar.modifyReturning
      ( \store -> do
          let maybeValue = store |> Map.get keyText
          let newStore = store |> Map.remove keyText
          Task.yield (newStore, maybeValue)
      )


deleteImpl :: Storage -> TransactionKey -> Task Text Unit
deleteImpl storage key = do
  let keyText = TransactionKey.toText key
  storage |> ConcurrentVar.modify (\store -> store |> Map.remove keyText)
