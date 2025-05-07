{-# LANGUAGE TypeFamilies #-}

module Collection (
  Collection (..),
  length,
  empty,
  isEmpty,
  get,
  set,
  append,
  first,
  last,
  indices,
  map,
) where

import Basics hiding (Item)
import Maybe (Maybe (..))


-- | The Collection typeclass defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
class Collection collection where
  type Item collection


  lengthImpl :: collection -> Int
  emptyImpl :: collection
  isEmptyImpl :: collection -> Bool
  getImpl :: Int -> collection -> Maybe (Collection.Item collection)
  setImpl :: Int -> Collection.Item collection -> collection -> collection
  appendImpl :: collection -> collection -> collection
  firstImpl :: collection -> Maybe (Collection.Item collection)
  lastImpl :: collection -> Maybe (Collection.Item collection)
  indicesImpl :: collection -> [Int]
  mapImpl :: (Collection.Item collection -> Collection.Item collection) -> collection -> collection


-- containsImpl :: (Collection.Item collection) -> collection -> Bool

-- | Wrapper functions without the Impl suffix
length :: (Collection c) => c -> Int
length = lengthImpl


empty :: (Collection c) => c
empty = emptyImpl


isEmpty :: (Collection c) => c -> Bool
isEmpty = isEmptyImpl


get :: (Collection c) => Int -> c -> Maybe (Collection.Item c)
get = getImpl


set :: (Collection c) => Int -> (Collection.Item c) -> c -> c
set = setImpl


append :: (Collection c) => c -> c -> c
append = appendImpl


first :: (Collection c) => c -> Maybe (Collection.Item c)
first = firstImpl


last :: (Collection c) => c -> Maybe (Collection.Item c)
last = lastImpl


indices :: (Collection c) => c -> [Int]
indices = indicesImpl


map :: (Collection c) => (Collection.Item c -> Collection.Item c) -> c -> c
map = mapImpl
