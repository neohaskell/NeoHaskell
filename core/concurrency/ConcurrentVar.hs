module ConcurrentVar (
  ConcurrentVar,
  new,
  containing,
  get,
  set,
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


set :: value -> ConcurrentVar value -> Task _ ()
set value (ConcurrentVar ref) =
  GHC.putMVar ref value
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
