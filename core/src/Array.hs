module Array (
  -- * Type
  Array,

  -- * Constructors
  empty,
  singleton,
  fromList,

  -- * Basic functions
  isEmpty,
  length,
  append,
  concat,
  map,
  filter,
  foldl,
  foldr,
  toList,

  -- * Transformations
  reverse,
  intersperse,

  -- * Subarrays
  slice,
  take,
  drop,
  splitAt,

  -- * Searching
  elem,
  notElem,
  find,
  findIndex,

  -- * Accessing elements
  (!?),
  index,
  head,
  last,
  tail,
  init,

  -- * Zipping and unzipping
  zip,
  zipWith,
  unzip,

  -- * Special folds
  and,
  or,
  any,
  all,

  -- * Set operations
  union,
  intersect,
  difference,

  -- * Conversion
  toArray,
  fromArray
) where

import Data.Vector (Vector)
import qualified Data.Vector as Vector

-- | 'Array' is a type that wraps Haskell's 'Vector' to provide a data-last API.
newtype Array a = Array (Vector a)

-- | Create an empty array.
empty :: Array a
empty = Array Vector.empty

-- | Create an array with one element.
singleton :: a -> Array a
singleton = Array . Vector.singleton

-- | Convert a list to an array.
fromList :: [a] -> Array a
fromList = Array . Vector.fromList

-- | Check if an array is empty.
isEmpty :: Array a -> Bool
isEmpty (Array v) = Vector.null v

-- | Get the length of an array.
length :: Array a -> Int
length (Array v) = Vector.length v

-- | Append two arrays.
append :: Array a -> Array a -> Array a
append (Array v1) (Array v2) = Array (v1 Vector.++ v2)

-- | Merge multiple arrays into one.
mergeArrays :: [Array a] -> Array a
mergeArrays = Array . Vector.concat . applyToEach (\(Array v) -> v)

-- | Apply a function to each element of an array.
applyToEach :: (a -> b) -> Array a -> Array b
applyToEach f (Array v) = Array (Vector.map f v)

-- | Take elements that satisfy the predicate.
takeIf :: (a -> Bool) -> Array a -> Array a
takeIf f (Array v) = Array (Vector.filter f v)

-- | Reduce the array from the left.
reduceFromLeft :: (b -> a -> b) -> b -> Array a -> b
reduceFromLeft f z (Array v) = Vector.foldl f z v

-- | Reduce the array from the right.
reduceFromRight :: (a -> b -> b) -> b -> Array a -> b
reduceFromRight f z (Array v) = Vector.foldr f z v

-- | Convert an array to a list.
toList :: Array a -> [a]
toList (Array v) = Vector.toList v

-- Additional functions to be implemented...
-- | Drop elements that satisfy the predicate.
dropIf :: (a -> Bool) -> Array a -> Array a
dropIf f (Array v) = Array (Vector.filter (not . f) v)
