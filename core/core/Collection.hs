module Collection (
  Collection,
) where

import Maybe (Maybe (..))
import Prelude qualified


-- | The Collection trait defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
-- mapped over.
-- class Collection a where
--     countImpl :: a -> Int
--     isEmptyImpl :: a -> Bool
--     subscriptImple :: a -> Int -> Maybe Any
data Collection collection
  = Collection collection


countImpl :: (Collection collection) -> Prelude.Int
countImpl = Prelude.undefined


isEmptyImpl :: (Collection collection) -> Prelude.Bool
isEmptyImpl = Prelude.undefined


subscriptImpl :: (Collection collection) -> Prelude.Int -> Maybe b
subscriptImpl = Prelude.undefined


firstImpl :: (Collection collection) -> Maybe b
firstImpl = Prelude.undefined


lastImpl :: (Collection collection) -> Maybe b
lastImpl = Prelude.undefined
