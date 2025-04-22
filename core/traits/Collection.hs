{-# LANGUAGE TypeFamilies #-}

module Collection (
  Collection (..),
  length,
  empty,
  isEmpty,
  get,
  append,
  first,
  last,
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


-- | Wrapper functions without the Impl suffix
length :: (Collection c) => c -> Int
length = lengthImpl


empty :: (Collection c) => c
empty = emptyImpl


isEmpty :: (Collection c) => c -> Bool
isEmpty = isEmptyImpl


get :: (Collection c) => Int -> c -> Maybe (Collection.Item c)
get = getImpl


append :: (Collection c) => c -> c -> c
append = appendImpl


first :: (Collection c) => c -> Maybe (Collection.Item c)
first = firstImpl


last :: (Collection c) => c -> Maybe (Collection.Item c)
last = lastImpl
