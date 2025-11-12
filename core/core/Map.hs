module Map (
  Map,
  build,
  HaskellMap.empty,
  (-->),
  set,
  get,
  reduce,
  merge,
  entries,
  contains,
  remove,
  getOrElse,
  size,
) where

import Accumulator (Accumulator)
import Accumulator qualified
import Array (Array)
import Array qualified
import Basics
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as HaskellMap
import Maybe (Maybe)
import Maybe qualified


-- | Merge two `Map`s.
merge :: (Eq key, Ord key) => Map key value -> Map key value -> Map key value
merge left right = HaskellMap.union left right


-- | Accumulator operator to build a `Map`
(-->) ::
  (Eq key, Ord key) =>
  key ->
  value ->
  Accumulator (Map key value)
(-->) key value =
  HaskellMap.singleton key value
    |> Accumulator.push


-- |
-- A builder API like this can be easily implemented through the
-- usage of the `Accumulator` module. `-->` is an operator that
-- will push a tuple into the accumulator list. After that, the
-- `build` function will convert the accumulator list into a
-- `Map`.
--
-- Example:
--
-- ```haskell
-- Map.build do
--   "a" --> 1
--   "b" --> 2
--   "c" --> 3
-- ```
build :: Accumulator (Map key value) -> Map key value
build = Accumulator.accumulate


-- | Set a value in a `Map`.
set :: (Eq key, Ord key) => key -> value -> Map key value -> Map key value
set key value map = (HaskellMap.insert key value map)


-- | Get the value from a `Map`.
get :: (Eq key, Ord key) => key -> Map key value -> Maybe value
get key map = HaskellMap.lookup key map


-- | Get the value from a `Map`, defaulting to a value
getOrElse :: (Eq key, Ord key) => key -> value -> Map key value -> value
getOrElse key defaultValue map =
  get key map
    |> Maybe.withDefault defaultValue


-- | Reduce a `Map`.
reduce :: acc -> (key -> value -> acc -> acc) -> Map key value -> acc
reduce acc f map = HaskellMap.foldrWithKey f acc map


-- | Converts a map to an array of tuples
entries :: Map key value -> Array (key, value)
entries self = HaskellMap.toList self |> Array.fromLinkedList


-- | Checks if a key exists in a map
contains :: (Eq key, Ord key) => key -> Map key value -> Bool
contains key map = HaskellMap.member key map
{-# INLINE contains #-}


-- | Removes an element from the map
remove :: (Ord key) => key -> Map key value -> Map key value
remove key self = HaskellMap.delete key self


-- | Returns the size of the map
size :: Map key value -> Int
size self = HaskellMap.size self
