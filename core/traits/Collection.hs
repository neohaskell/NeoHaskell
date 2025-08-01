{-# LANGUAGE TypeFamilies #-}

module Collection (
  Collection (..),
  empty,
  repeat,
  wrap,
  fromLinkedList,
  isEmpty,
  length,
  indices,
  get,
  first,
  set,
  push,
  append,
  slice,
  toLinkedList,
  toIndexedLinkedList,
  map,
  indexedMap,
  reduce,
  flatMap,
  takeIf,
  dropIf,
  dropWhile,
  takeWhile,
  take,
  drop,
  indexed,
  foldl,
  foldM,
  partitionBy,
  splitFirst,
  any,
  fromLegacy,
  last,
  zip,
  sumIntegers,
  reverse,
) where

import Basics hiding (Item)
import Data.Vector qualified
import IO (IO)
import LinkedList (LinkedList)
import Maybe (Maybe (..))


-- | The Collection typeclass defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
class Collection (f :: Type -> Type) where
  emptyImpl :: forall a. f a
  repeatImpl :: Int -> a -> f a
  wrapImpl :: a -> f a
  fromLinkedListImpl :: LinkedList a -> f a


  isEmptyImpl :: f a -> Bool
  lengthImpl :: f a -> Int
  indicesImpl :: f a -> f Int
  getImpl :: Int -> f a -> Maybe a
  firstImpl :: f a -> Maybe a


  setImpl :: Int -> a -> f a -> f a
  pushImpl :: a -> f a -> f a
  appendImpl :: f a -> f a -> f a
  sliceImpl :: Int -> Int -> f a -> f a


  toLinkedListImpl :: f a -> LinkedList a
  toIndexedLinkedListImpl :: f a -> LinkedList (Int, a)


  mapImpl :: (a -> b) -> f a -> f b
  indexedMapImpl :: (Int -> a -> b) -> f a -> f b
  reduceImpl :: (a -> b -> b) -> b -> f a -> b
  flatMapImpl :: (a -> f b) -> f a -> f b
  takeIfImpl :: (a -> Bool) -> f a -> f a
  dropIfImpl :: (a -> Bool) -> f a -> f a
  dropWhileImpl :: (a -> Bool) -> f a -> f a
  takeWhileImpl :: (a -> Bool) -> f a -> f a
  takeImpl :: Int -> f a -> f a
  dropImpl :: Int -> f a -> f a
  indexedImpl :: f a -> f (Int, a)
  foldlImpl :: (a -> b -> b) -> b -> f a -> b
  foldMImpl :: forall a b. (b -> a -> IO b) -> b -> f a -> IO b


  partitionByImpl :: (a -> Bool) -> f a -> (f a, f a)
  splitFirstImpl :: f a -> Maybe (a, f a)
  anyImpl :: (a -> Bool) -> f a -> Bool


  fromLegacyImpl :: Data.Vector.Vector a -> f a
  lastImpl :: f a -> Maybe a
  zipImpl :: f b -> f a -> f (a, b)
  sumIntegersImpl :: f Int -> Int
  reverseImpl :: f a -> f a


-- | Wrapper functions without the Impl suffix

-- | Creates an empty collection.
empty :: (Collection f) => f a
empty = emptyImpl


-- | Creates a collection by repeating a value a given number of times.
repeat :: (Collection f) => Int -> a -> f a
repeat = repeatImpl


-- | Creates a collection with a single element.
wrap :: (Collection f) => a -> f a
wrap = wrapImpl


-- | Creates a collection from a linked list.
fromLinkedList :: (Collection f) => LinkedList a -> f a
fromLinkedList = fromLinkedListImpl


-- | Checks if a collection is empty.
isEmpty :: (Collection f) => f a -> Bool
isEmpty = isEmptyImpl


-- | Returns the number of elements in a collection.
length :: (Collection f) => f a -> Int
length = lengthImpl


-- | Returns a collection of the indices.
indices :: (Collection f) => f a -> f Int
indices = indicesImpl


-- | Gets an element at a specific index.
get :: (Collection f) => Int -> f a -> Maybe a
get = getImpl


-- | Gets the first element of a collection.
first :: (Collection f) => f a -> Maybe a
first = firstImpl


-- | Sets an element at a specific index.
set :: (Collection f) => Int -> a -> f a -> f a
set = setImpl


-- | Adds an element to the end of a collection.
push :: (Collection f) => a -> f a -> f a
push = pushImpl


-- | Appends two collections.
append :: (Collection f) => f a -> f a -> f a
append = appendImpl


-- | Extracts a slice of a collection.
slice :: (Collection f) => Int -> Int -> f a -> f a
slice = sliceImpl


-- | Converts a collection to a linked list.
toLinkedList :: (Collection f) => f a -> LinkedList a
toLinkedList = toLinkedListImpl


-- | Converts a collection to a linked list of (index, value) pairs.
toIndexedLinkedList :: (Collection f) => f a -> LinkedList (Int, a)
toIndexedLinkedList = toIndexedLinkedListImpl


-- | Maps a function over a collection.
map :: (Collection f) => (a -> b) -> f a -> f b
map = mapImpl


-- | Maps a function over a collection with access to the index.
indexedMap :: (Collection f) => (Int -> a -> b) -> f a -> f b
indexedMap = indexedMapImpl


-- | Reduces a collection to a single value.
reduce :: (Collection f) => (a -> b -> b) -> b -> f a -> b
reduce = reduceImpl


-- | Maps a function that returns a collection over a collection and flattens the result.
flatMap :: (Collection f) => (a -> f b) -> f a -> f b
flatMap = flatMapImpl


-- | Keeps elements that satisfy a predicate.
takeIf :: (Collection f) => (a -> Bool) -> f a -> f a
takeIf = takeIfImpl


-- | Drops elements that satisfy a predicate.
dropIf :: (Collection f) => (a -> Bool) -> f a -> f a
dropIf = dropIfImpl


-- | Drops elements while a predicate is true.
dropWhile :: (Collection f) => (a -> Bool) -> f a -> f a
dropWhile = dropWhileImpl


-- | Takes elements while a predicate is true.
takeWhile :: (Collection f) => (a -> Bool) -> f a -> f a
takeWhile = takeWhileImpl


-- | Takes a specific number of elements from the start of a collection.
take :: (Collection f) => Int -> f a -> f a
take = takeImpl


-- | Drops a specific number of elements from the start of a collection.
drop :: (Collection f) => Int -> f a -> f a
drop = dropImpl


-- | Pairs each element with its index.
indexed :: (Collection f) => f a -> f (Int, a)
indexed = indexedImpl


-- | A left-associative fold of a collection.
foldl :: (Collection f) => (a -> b -> b) -> b -> f a -> b
foldl = foldlImpl


-- | A monadic, left-associative fold.
foldM :: (Collection f) => (b -> a -> IO b) -> b -> f a -> IO b
foldM = foldMImpl


-- | Partitions a collection into two based on a predicate.
partitionBy :: (Collection f) => (a -> Bool) -> f a -> (f a, f a)
partitionBy = partitionByImpl


-- | Splits a collection into its first element and the rest.
splitFirst :: (Collection f) => f a -> Maybe (a, f a)
splitFirst = splitFirstImpl


-- | Checks if any element satisfies a predicate.
any :: (Collection f) => (a -> Bool) -> f a -> Bool
any = anyImpl


-- | Converts a legacy Vector to a collection.
fromLegacy :: (Collection f) => Data.Vector.Vector a -> f a
fromLegacy = fromLegacyImpl


-- | Gets the last element of a collection.
last :: (Collection f) => f a -> Maybe a
last = lastImpl


-- | Zips two collections together.
zip :: (Collection f) => f b -> f a -> f (a, b)
zip = zipImpl


-- | Sums a collection of integers.
sumIntegers :: (Collection f) => f Int -> Int
sumIntegers = sumIntegersImpl


-- | Reverses a collection.
reverse :: (Collection f) => f a -> f a
reverse = reverseImpl
