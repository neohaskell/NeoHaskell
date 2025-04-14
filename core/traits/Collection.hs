module Collection (
  Collection,
) where

import Maybe (Maybe (..))
import Prelude qualified


-- | The Collection typeclass defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
class Collection collection where
  type Item collection

  countImpl   :: collection -> Int
  emptyImpl   :: collection
  isEmptyImpl :: collection -> Bool
  lengthImpl  :: collection -> Int
  getImpl     :: collection -> Int -> Maybe (Item collection)
  appendImpl  :: collection -> collection -> collection
  firstImpl   :: collection -> Maybe (Item collection)
  lastImpl    :: collection -> Maybe (Item collection)

-- | Wrapper functions without the Impl suffix
count :: Collection c => c -> Int
count = countImpl

empty :: Collection c => c
empty = emptyImpl

isEmpty :: Collection c => c -> Bool
isEmpty = isEmptyImpl

length :: Collection c => c -> Int
length = lengthImpl

get :: Collection c => c -> Int -> Maybe (Item c)
get = getImpl

append :: Collection c => c -> c -> c
append = appendImpl

first :: Collection c => c -> Maybe (Item c)
first = firstImpl

last :: Collection c => c -> Maybe (Item c)
last = lastImpl
