{-# LANGUAGE TypeFamilies #-}

module Collection (
  Collection,
  count,
  empty,
  isEmpty,
  length,
  get,
  append,
  first,
  last,
) where

import Maybe (Maybe (..))
import Prelude qualified


-- | The Collection typeclass defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
class Collection collection where
  type Item collection


  countImpl :: collection -> Prelude.Int
  emptyImpl :: collection
  isEmptyImpl :: collection -> Prelude.Bool
  lengthImpl :: collection -> Prelude.Int
  getImpl :: collection -> Prelude.Int -> Maybe (Item collection)
  appendImpl :: collection -> collection -> collection
  firstImpl :: collection -> Maybe (Item collection)
  lastImpl :: collection -> Maybe (Item collection)


-- | Wrapper functions without the Impl suffix
count :: (Collection c) => c -> Prelude.Int
count = countImpl


empty :: (Collection c) => c
empty = emptyImpl


isEmpty :: (Collection c) => c -> Prelude.Bool
isEmpty = isEmptyImpl


length :: (Collection c) => c -> Prelude.Int
length = lengthImpl


get :: (Collection c) => c -> Prelude.Int -> Maybe (Item c)
get = getImpl


append :: (Collection c) => c -> c -> c
append = appendImpl


first :: (Collection c) => c -> Maybe (Item c)
first = firstImpl


last :: (Collection c) => c -> Maybe (Item c)
last = lastImpl
