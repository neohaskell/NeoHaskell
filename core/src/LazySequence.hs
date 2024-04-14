module LazySequence (
  LazySequence,
  empty,
  wrap,
  isEmpty,
  length,
  append,
  flatten,
  map,
  takeIf,
  reduce,
  reduceFromRight,
  dropIf,
  mapMaybe,
  dropNones,
  reverse,
  intersperse,
  slice,
  take,
  drop,
  splitAt,
  contains,
  doesNotContain,
  findIndex,
  get,
  first,
  last,
  allButFirst,
  allButLast,
  pairWith,
  combineWith,
  splitPairs,
  allTrue,
  anyTrue,
  anySatisfy,
  allSatisfy,
  union,
  intersect,
  difference,
  sort,
  sortBy,
  deduplicate,
) where

import Data.List qualified as L
import Data.Maybe (catMaybes)


-- | A `LazySequence` represents a series of elements that are computed only when they are needed, rather than all at once. This concept is akin to 'lazy evaluation' in the functional programming realm.
-- | In TypeScript, this might be similar to using generators to create sequences whose computations are deferred until the elements are actually iterated over.
-- |
-- | Here's how you might initialize and use a `LazySequence`:
-- |
-- | >>> let seq = [1..]
-- | >>> take 5 seq
-- | [1, 2, 3, 4, 5]
type LazySequence value = [value]


-- Constructors
empty :: LazySequence a
empty = []


wrap :: a -> LazySequence a
wrap x = [x]


-- Basic functions
isEmpty :: LazySequence a -> Bool
isEmpty = null


length :: LazySequence a -> Int
length = L.length


append :: LazySequence a -> a -> LazySequence a
append seq x = seq ++ [x]


flatten :: LazySequence (LazySequence a) -> LazySequence a
flatten = concat


map :: (a -> b) -> LazySequence a -> LazySequence b
map = Prelude.map


takeIf :: (a -> Bool) -> LazySequence a -> LazySequence a
takeIf = filter


reduce :: (b -> a -> b) -> b -> LazySequence a -> b
reduce = foldl


reduceFromRight :: (a -> b -> b) -> b -> LazySequence a -> b
reduceFromRight = foldr


dropIf :: (a -> Bool) -> LazySequence a -> LazySequence a
dropIf p = filter (not . p)


mapMaybe :: (a -> Maybe b) -> LazySequence a -> LazySequence b
mapMaybe f = catMaybes . map f


dropNones :: LazySequence (Maybe a) -> LazySequence a
dropNones = catMaybes


-- Transformations
reverse :: LazySequence a -> LazySequence a
reverse = L.reverse


intersperse :: a -> LazySequence a -> LazySequence a
intersperse sep = L.intersperse sep


-- Subarrays
slice :: Int -> Int -> LazySequence a -> LazySequence a
slice from to = take (to - from + 1) . drop from


take :: Int -> LazySequence a -> LazySequence a
take = Prelude.take


drop :: Int -> LazySequence a -> LazySequence a
drop = Prelude.drop


splitAt :: Int -> LazySequence a -> (LazySequence a, LazySequence a)
splitAt = Prelude.splitAt


-- Searching functionalities

-- | Checks if the given element is present in the sequence.
-- | Similar to `Array.includes(value)` in JavaScript/TypeScript.
contains :: (Eq a) => a -> LazySequence a -> Bool
contains x = any (x ==)


-- | Checks if the given element is not present in the sequence.
-- | Similar to `!Array.includes(value)` in JavaScript/TypeScript.
doesNotContain :: (Eq a) => a -> LazySequence a -> Bool
doesNotContain x = not . contains x


-- | Finds the index of the first element that matches the given predicate.
-- | Similar to `Array.findIndex(predicate)` in JavaScript/TypeScript.
findIndex :: (a -> Bool) -> LazySequence a -> Maybe Int
findIndex pred = fmap fst . L.find (pred . snd) . zip [0 ..]


-- Accessing elements

-- | Gets the element at the specified index, if it exists.
-- | This is equivalent to accessing an element via an array index in TypeScript,
-- | but it returns 'Maybe a' since the index may be out of bounds.
get :: Int -> LazySequence a -> Maybe a
get idx seq = if idx < 0 then Nothing else listToMaybe $ drop idx seq


-- | Gets the first element of the sequence, if it exists.
-- | Similar to accessing the first element of an array in TypeScript.
first :: LazySequence a -> Maybe a
first = listToMaybe


-- | Retrieves the last element of the sequence, if it exists.
-- | Similar to `Array.slice(-1)[0]` in TypeScript.
last :: LazySequence a -> Maybe a
last = listToMaybe . Prelude.reverse


-- | Returns all elements of the sequence except the first.
-- | Similar to `sequence.slice(1)` in TypeScript.
allButFirst :: LazySequence a -> LazySequence a
allButFirst = drop 1


-- | Returns all elements of the sequence except the last.
-- | Similar to `sequence.slice(0, sequence.length - 1)` in TypeScript.
allButLast :: LazySequence a -> LazySequence a
allButLast = L.init -- `init` can fail on an empty list, so usage should be guarded by checks for non-emptiness if needed.

-- Now, let's also provide an example for a few of these, to illustrate how to write doc-comments.

-- >>> contains 3 [1, 2, 3, 4, 5]
-- True

-- >>> doesNotContain 10 [1, 2, 3, 4, 5]
-- True

-- >>> findIndex (>3) [1, 2, 3, 4, 5]
-- Just 3

-- >>> get 2 [1, 2, 3, 4, 5]
-- Just 3
