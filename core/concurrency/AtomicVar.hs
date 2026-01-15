-- | # AtomicVar
--
-- A transactional variable for composable atomic operations.
--
-- Unlike 'ConcurrentVar' (which wraps MVar and has blocking semantics),
-- 'AtomicVar' wraps STM's TVar and supports:
--
-- * Non-blocking reads and writes
-- * Composable atomic transactions (multiple operations execute as one)
-- * Pure reads via 'peekSTM' for use inside STM transactions (e.g., ConcurrentMap predicates)
--
-- == When to use AtomicVar vs ConcurrentVar
--
-- * Use 'AtomicVar' when you need to read the value inside STM transactions
--   (e.g., predicates for 'ConcurrentMap.getOrInsertIf')
-- * Use 'AtomicVar' when you need multiple variables to update atomically together
-- * Use 'ConcurrentVar' for simple producer/consumer patterns with blocking semantics
module AtomicVar (
  AtomicVar,
  containing,
  peek,
  peekSTM,
  set,
  modify,
) where

import Basics
import Control.Concurrent.STM qualified as GhcSTM
import Task (Task)
import Task qualified


-- | A transactional variable that supports atomic composition.
newtype AtomicVar value = AtomicVar (GhcSTM.TVar value)


-- | Create an AtomicVar containing an initial value.
containing ::
  forall value.
  value ->
  Task _ (AtomicVar value)
containing value = do
  tvar <- GhcSTM.newTVarIO value |> Task.fromIO
  Task.yield (AtomicVar tvar)


-- | Read the current value.
peek ::
  forall value.
  AtomicVar value ->
  Task _ value
peek (AtomicVar tvar) =
  GhcSTM.readTVarIO tvar
    |> Task.fromIO


-- | Read the current value inside an STM transaction.
--
-- This is useful for predicates in 'ConcurrentMap.getOrInsertIf' which
-- run inside STM and cannot perform IO.
--
-- Note: This returns an STM action, not a Task. Use it like:
--
-- @
-- let predicate worker = GhcSTM.unsafePerformIO (GhcSTM.atomically (AtomicVar.peekSTM worker.status)) == Draining
-- @
--
-- Or more commonly, ConcurrentMap operations will handle the STM context for you.
peekSTM ::
  forall value.
  AtomicVar value ->
  GhcSTM.STM value
peekSTM (AtomicVar tvar) =
  GhcSTM.readTVar tvar


-- | Set a new value.
set ::
  forall value.
  value ->
  AtomicVar value ->
  Task _ Unit
set value (AtomicVar tvar) =
  GhcSTM.atomically (GhcSTM.writeTVar tvar value)
    |> Task.fromIO


-- | Atomically modify the value.
modify ::
  forall value.
  (value -> value) ->
  AtomicVar value ->
  Task _ Unit
modify f (AtomicVar tvar) =
  GhcSTM.atomically (GhcSTM.modifyTVar' tvar f)
    |> Task.fromIO
