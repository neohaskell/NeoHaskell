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
  Transaction,
  containing,
  peek,
  peekSTM,
  set,
  modify,
  modifyWithResult,
  modifyWithResultSTM,
  waitUntil,
) where

import Basics
import Control.Concurrent.STM qualified as GhcSTM
import Prelude qualified
import Task (Task)
import Task qualified


-- | A transactional variable that supports atomic composition.
newtype AtomicVar value = AtomicVar (GhcSTM.TVar value)


type Transaction result = GhcSTM.STM result


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
peek atomicVar = do
  case atomicVar of
    AtomicVar tvar ->
      GhcSTM.readTVarIO tvar
        |> Task.fromIO


-- | Read the current value inside an STM transaction.
--
-- This is useful for predicates in 'ConcurrentMap.getOrInsertIfM' which
-- run inside STM and cannot perform IO.
--
-- Example usage with ConcurrentMap:
--
-- @
-- let shouldReplace existing = do
--       status <- AtomicVar.peekSTM existing.status
--       pure (status == Draining)
-- ConcurrentMap.getOrInsertIfM key candidate shouldReplace map
-- @
peekSTM ::
  forall value.
  AtomicVar value ->
  GhcSTM.STM value
peekSTM atomicVar =
  case atomicVar of
    AtomicVar tvar ->
      GhcSTM.readTVar tvar


-- | Set a new value.
set ::
  forall value.
  value ->
  AtomicVar value ->
  Task _ Unit
set value atomicVar =
  case atomicVar of
    AtomicVar tvar ->
      GhcSTM.atomically (GhcSTM.writeTVar tvar value)
        |> Task.fromIO


-- | Atomically modify the value.
modify ::
  forall value.
  (value -> value) ->
  AtomicVar value ->
  Task _ Unit
modify f atomicVar =
  case atomicVar of
    AtomicVar tvar ->
      GhcSTM.atomically (GhcSTM.modifyTVar' tvar f)
        |> Task.fromIO


-- | Atomically modify the value and return a result.
-- The function receives the current value and returns (newValue, result).
-- This is useful for implementing atomic check-and-set patterns.
modifyWithResult ::
  forall value result.
  AtomicVar value ->
  (value -> (value, result)) ->
  Task _ result
modifyWithResult atomicVar f =
  GhcSTM.atomically (modifyWithResultSTM f atomicVar)
    |> Task.fromIO


-- | Update a value inside a caller-owned transaction, such as a map predicate.
modifyWithResultSTM ::
  forall value result.
  (value -> (value, result)) ->
  AtomicVar value ->
  Transaction result
modifyWithResultSTM f (AtomicVar tvar) = do
  oldValue <- GhcSTM.readTVar tvar
  let (newValue, result) = f oldValue
  GhcSTM.writeTVar tvar newValue
  Prelude.pure result


-- | Wait until a value satisfies a predicate without polling.
waitUntil ::
  forall value err.
  (value -> Bool) ->
  AtomicVar value ->
  Task err Unit
waitUntil predicate (AtomicVar tvar) =
  GhcSTM.atomically do
    value <- GhcSTM.readTVar tvar
    if predicate value then Prelude.pure unit else GhcSTM.retry
  |> Task.fromIO
