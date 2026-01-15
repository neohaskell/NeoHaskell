module ConcurrentVar (
  ConcurrentVar,
  new,
  containing,
  get,
  set,
  swap,
  peek,
  modify,
  modifyReturning,
) where

import Basics
import Control.Concurrent.MVar qualified as GHC
import Task (Task)
import Task qualified


newtype ConcurrentVar value = ConcurrentVar (GHC.MVar value)


new :: Task _ (ConcurrentVar value)
new = do
  ref <- Task.fromIO GHC.newEmptyMVar
  Task.yield (ConcurrentVar ref)


containing :: value -> Task _ (ConcurrentVar value)
containing value = do
  ref <- GHC.newMVar value |> Task.fromIO
  Task.yield (ConcurrentVar ref)


get :: ConcurrentVar value -> Task _ value
get (ConcurrentVar ref) =
  GHC.takeMVar ref
    |> Task.fromIO


peek :: ConcurrentVar value -> Task _ value
peek (ConcurrentVar ref) =
  GHC.readMVar ref
    |> Task.fromIO


-- | Put a value into an empty ConcurrentVar.
--
-- WARNING: This will block indefinitely if the ConcurrentVar already contains
-- a value! This is designed for the pattern: create empty with 'new', then
-- 'set' once.
--
-- For replacing an existing value, use 'swap' instead.
-- For updating based on current value, use 'modify' instead.
set :: value -> ConcurrentVar value -> Task _ ()
set value (ConcurrentVar ref) =
  GHC.putMVar ref value
    |> Task.fromIO


-- | Atomically replace the current value with a new one, returning the old value.
--
-- This is safe to call on a ConcurrentVar that already contains a value.
-- Use this instead of 'set' when updating an existing value.
--
-- Blocks if the ConcurrentVar is empty until a value is available.
swap :: value -> ConcurrentVar value -> Task _ value
swap newValue (ConcurrentVar ref) =
  GHC.swapMVar ref newValue
    |> Task.fromIO


modify ::
  (value -> value) -> ConcurrentVar value -> Task _ Unit
modify transformer (ConcurrentVar ref) =
  GHC.modifyMVar_ ref (transformer .> pure)
    |> Task.fromIO


-- | Modifies the value in the ConcurrentVar and returns an additional value.
-- The callback function that is passed must return a task that calculates a
-- tuple of the new value and the additional value.
modifyReturning ::
  (value -> Task _ (value, a)) ->
  ConcurrentVar value ->
  Task _ a
modifyReturning modifier (ConcurrentVar ref) = do
  let ioModifier value = Task.runNoErrors (modifier value)
  GHC.modifyMVar ref ioModifier
    |> Task.fromIO
