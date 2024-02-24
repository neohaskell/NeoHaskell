{-# LANGUAGE TypeFamilies #-}

module Array (
  -- * Type
  Array (..),

  -- * Constructors
  empty,
  wrap,

  -- * Basic functions
  isEmpty,
  length,
  appendArray,
  flatten,
  applyToEach,
  takeIf,
  reduce,
  reduceFromRight,
  dropIf,
  push,
  -- -- * Transformations
  -- reverse,
  -- intersperse,

  -- -- * Subarrays
  -- slice,
  -- take,
  -- drop,
  -- splitAt,

  -- -- * Searching
  -- elem,
  -- notElem,
  -- find,
  -- findIndex,

  -- -- * Accessing elements
  -- (!?),
  -- index,
  -- head,
  -- last,
  -- tail,
  -- init,

  -- -- * Zipping and unzipping
  -- pairWith,
  -- combineWith,
  -- splitPairs,

  -- -- * Special folds
  -- allTrue,
  -- anyTrue,
  -- anySatisfy,
  -- allSatisfy,

  -- -- * Set operations
  -- union,
  -- intersect,
  -- difference,

  -- -- * Additional operations
  -- sort,
  -- sortBy,
  -- deduplicate,
  -- TODO: Explore Data.Vector.Algorithms and see if there are more interesting ones
) where

import Bool (Bool, not)
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import Int (Int)
import Pipe
import Traits.Dsl


-- TODO: Add property-based doc-tests for all functions

-- | 'Array' represents a list of items.
newtype Array item = INTERNAL_CORE_ARRAY_CONSTRUCTOR (Vector item)


-- | Create an empty array.
empty :: Array item
empty = INTERNAL_CORE_ARRAY_CONSTRUCTOR Vector.empty


-- | Create an array with one element.
wrap :: item -> Array item
wrap item =
  Vector.singleton item
    |> INTERNAL_CORE_ARRAY_CONSTRUCTOR


-- | Check if an array is empty.
isEmpty :: Array item -> Bool
isEmpty (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = Vector.null vector


-- | Get the length of an array.
length :: Array item -> Int
length (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = Vector.length vector


-- | Append two arrays.
appendArray :: Array item -> Array item -> Array item
appendArray (INTERNAL_CORE_ARRAY_CONSTRUCTOR v1) (INTERNAL_CORE_ARRAY_CONSTRUCTOR v2) = INTERNAL_CORE_ARRAY_CONSTRUCTOR (v1 Vector.++ v2)


-- | Flatten an array of arrays.
flatten :: Array (Array item) -> Array item
flatten self =
  reduce appendArray empty self


-- | Apply a transformation function to each element of the array.
applyToEach :: (item -> otherItem) -> Array item -> Array otherItem
applyToEach transformation self =
  applyToVector (Vector.map transformation) self


-- | Filter all elements that satisfy the predicate.
takeIf :: (item -> Bool) -> Array item -> Array item
takeIf predicate self =
  applyToVector (Vector.filter predicate) self


-- | Filter all elements that do not satisfy the predicate.
dropIf :: (item -> Bool) -> Array item -> Array item
dropIf predicate self = do
  takeIf (predicate .> not) self


-- | Combines elements of an array using a binary function.
reduce :: (otherItem -> item -> otherItem) -> otherItem -> Array item -> otherItem
reduce f z (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = Vector.foldl f z vector


-- | Combines elements of an array using a binary function, starting from the right.
reduceFromRight :: (item -> otherItem -> otherItem) -> otherItem -> Array item -> otherItem
reduceFromRight f z (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = Vector.foldr f z vector


-- | Adds an item to the end of the array
push :: item -> Array item -> Array item
push item (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = INTERNAL_CORE_ARRAY_CONSTRUCTOR (Vector.snoc vector item)


-- | applyDsl is an alias for `mapM`
applyDsl :: (Dsl context) => (item -> context otherItem) -> Array item -> context (Array otherItem)
applyDsl f (INTERNAL_CORE_ARRAY_CONSTRUCTOR self) = do
  result <- Vector.mapM f self
  INTERNAL_CORE_ARRAY_CONSTRUCTOR result
    |> yield


-- Additional functions to be implemented...

-- PRIVATE

-- | Applies a function to the underlying vector. Used for implementing
-- the compatibility layer with Data.Vector.
applyToVector :: (Vector item -> Vector otherItem) -> Array item -> Array otherItem
applyToVector f (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) = INTERNAL_CORE_ARRAY_CONSTRUCTOR (f vector)
{-# INLINE applyToVector #-}