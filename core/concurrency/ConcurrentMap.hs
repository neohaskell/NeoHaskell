module ConcurrentMap (
  ConcurrentMap,
  new,
  get,
  set,
  getOrInsert,
  getOrInsertIf,
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


-- | Atomically get an existing value or insert a new one.
--
-- This is useful for the "get or create" pattern where you want to:
-- 1. Return the existing value if present
-- 2. Insert and return a new value if not present
-- 3. Guarantee that only one value is ever inserted for a key, even under concurrent access
--
-- The candidate value is computed before the STM transaction. If another
-- thread inserts first, the pre-computed value is returned via the second
-- element of the tuple so the caller can clean it up.
--
-- Returns: (actualValue, maybeDiscardedCandidate)
-- - actualValue: The value that ended up in the map (existing or newly inserted)
-- - maybeDiscardedCandidate: Just candidate if we lost the race, Nothing if we won or key existed
getOrInsert ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  value ->
  ConcurrentMap key value ->
  Task _ (value, Maybe value)
getOrInsert key candidate (ConcurrentMap stmMap) =
  GhcSTM.atomically do
    existing <- STMMap.lookup key stmMap
    case existing of
      Just existingValue ->
        -- Key exists, return existing and indicate candidate should be discarded
        pure (existingValue, Just candidate)
      Nothing -> do
        -- Key doesn't exist, insert candidate
        STMMap.insert candidate key stmMap
        pure (candidate, Nothing)
  |> Task.fromIO


-- | Atomically get an existing value, or insert a new one if the key is missing
-- OR if the existing value satisfies a replacement predicate.
--
-- This is useful for the "get or create, but replace if stale" pattern:
-- 1. If key is missing, insert candidate and return it
-- 2. If key exists and predicate returns False, return existing value
-- 3. If key exists and predicate returns True, replace with candidate and return candidate
--
-- The predicate is evaluated INSIDE the STM transaction, so the decision
-- to replace is atomic with respect to other operations.
--
-- Returns: (actualValue, maybeDiscardedValue)
-- - actualValue: The value that ended up in the map
-- - maybeDiscardedValue: The value that was discarded (either candidate or replaced existing)
getOrInsertIf ::
  forall key value.
  (Hashable key, Eq key) =>
  key ->
  value ->
  (value -> Bool) ->  -- ^ Predicate: True = replace existing, False = keep existing
  ConcurrentMap key value ->
  Task _ (value, Maybe value)
getOrInsertIf key candidate shouldReplace (ConcurrentMap stmMap) =
  GhcSTM.atomically do
    existing <- STMMap.lookup key stmMap
    case existing of
      Just existingValue ->
        if shouldReplace existingValue
          then do
            -- Replace existing with candidate
            STMMap.insert candidate key stmMap
            pure (candidate, Just existingValue)
          else
            -- Keep existing, discard candidate
            pure (existingValue, Just candidate)
      Nothing -> do
        -- Key doesn't exist, insert candidate
        STMMap.insert candidate key stmMap
        pure (candidate, Nothing)
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
