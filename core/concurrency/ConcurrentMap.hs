module ConcurrentMap (
  ConcurrentMap,
  new,
  get,
  set,
  remove,
  contains,
  clear,
  length,
  keys,
  values,
  entries,
) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent.STM qualified as GhcSTM
import Data.Hashable (Hashable)
import Data.List qualified as GhcList
import ListT qualified
import Maybe (Maybe (..))
import StmContainers.Map qualified as STMMap
import Task (Task)
import Task qualified
import Tuple qualified


-- | A concurrent map that supports high-throughput concurrent access.
-- Uses STM (Software Transactional Memory) internally for lock-free operations.
newtype ConcurrentMap key value = ConcurrentMap (STMMap.Map key value)


-- | Create a new empty ConcurrentMap.
new ::
  forall key value.
  Task _ (ConcurrentMap key value)
new = do
  stmMap <- STMMap.newIO |> Task.fromIO
  Task.yield (ConcurrentMap stmMap)


-- | Get a value from the map by key.
-- Returns Nothing if the key is not present.
get ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  ConcurrentMap key value ->
  Task _ (Maybe value)
get key (ConcurrentMap stmMap) =
  STMMap.lookup key stmMap
    |> GhcSTM.atomically
    |> Task.fromIO


-- | Set a value in the map for the given key.
-- If the key already exists, the value is updated.
set ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  value ->
  ConcurrentMap key value ->
  Task _ Unit
set key value (ConcurrentMap stmMap) =
  STMMap.insert value key stmMap
    |> GhcSTM.atomically
    |> Task.fromIO


-- | Remove a key from the map.
-- Does nothing if the key is not present.
remove ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  ConcurrentMap key value ->
  Task _ Unit
remove key (ConcurrentMap stmMap) =
  STMMap.delete key stmMap
    |> GhcSTM.atomically
    |> Task.fromIO


-- | Check if a key exists in the map.
contains ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  ConcurrentMap key value ->
  Task _ Bool
contains key (ConcurrentMap stmMap) = do
  result <- STMMap.lookup key stmMap |> GhcSTM.atomically |> Task.fromIO
  case result of
    Just _ -> Task.yield True
    Nothing -> Task.yield False


-- | Remove all entries from the map.
clear ::
  forall key value.
  ConcurrentMap key value ->
  Task _ Unit
clear (ConcurrentMap stmMap) =
  STMMap.reset stmMap
    |> GhcSTM.atomically
    |> Task.fromIO


-- | Get the number of entries in the map.
-- Note: This requires iterating over the entire map.
length ::
  forall key value.
  ConcurrentMap key value ->
  Task _ Int
length (ConcurrentMap stmMap) = do
  entryList <- STMMap.listT stmMap |> ListT.toList |> GhcSTM.atomically |> Task.fromIO
  entryList |> GhcList.length |> Task.yield


-- | Get all keys in the map.
keys ::
  forall key value.
  ConcurrentMap key value ->
  Task _ (Array key)
keys (ConcurrentMap stmMap) = do
  entryList <- STMMap.listT stmMap |> ListT.toList |> GhcSTM.atomically |> Task.fromIO
  entryList
    |> GhcList.map Tuple.first
    |> Array.fromLinkedList
    |> Task.yield


-- | Get all values in the map.
values ::
  forall key value.
  ConcurrentMap key value ->
  Task _ (Array value)
values (ConcurrentMap stmMap) = do
  entryList <- STMMap.listT stmMap |> ListT.toList |> GhcSTM.atomically |> Task.fromIO
  entryList
    |> GhcList.map Tuple.second
    |> Array.fromLinkedList
    |> Task.yield


-- | Get all entries (key-value pairs) in the map.
entries ::
  forall key value.
  ConcurrentMap key value ->
  Task _ (Array (key, value))
entries (ConcurrentMap stmMap) = do
  entryList <- STMMap.listT stmMap |> ListT.toList |> GhcSTM.atomically |> Task.fromIO
  entryList
    |> Array.fromLinkedList
    |> Task.yield
