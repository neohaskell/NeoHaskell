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
  fromArray,
) where

import Data.Vector (Vector)
import Data.Vector qualified as Vector


-- | 'Array' is a type that wraps Haskell's 'Vector' to provide a data-last API.
newtype Array item = Array (Vector item)


-- | Create an empty array.
empty :: Array item
empty = Array Vector.empty


-- | Create an array with one element.
singleton :: item -> Array item
singleton = Array . Vector.singleton


-- | Convert a list to an array.
fromList :: [item] -> Array item
fromList = Array . Vector.fromList


-- | Check if an array is empty.
isEmpty :: Array item -> Bool
isEmpty (Array v) = Vector.null v


-- | Get the length of an array.
length :: Array item -> Int
length (Array v) = Vector.length v


-- | Append two arrays.
append :: Array item -> Array item -> Array item
append (Array v1) (Array v2) = Array (v1 Vector.++ v2)


-- | Concatenate a list of arrays.
concat :: [Array item] -> Array item
concat = Array . Vector.concat . map (\(Array v) -> v)


-- | Map a function over an array.
map :: (itemA -> itemB) -> Array itemA -> Array itemB
map f (Array v) = Array (Vector.map f v)


-- | Filter all elements that satisfy the predicate.
filter :: (item -> Bool) -> Array item -> Array item
filter f (Array v) = Array (Vector.filter f v)


-- | Left fold.
foldl :: (resultItem -> item -> resultItem) -> resultItem -> Array item -> resultItem
foldl f z (Array v) = Vector.foldl f z v


-- | Right fold.
foldr :: (item -> resultItem -> resultItem) -> resultItem -> Array item -> resultItem
foldr f z (Array v) = Vector.foldr f z v


-- | Convert an array to a list.
toList :: Array item -> [item]
toList (Array v) = Vector.toList v

-- Additional functions to be implemented...
