module Collection (
  Collection,
) where

import Maybe (Maybe (..))
import Prelude qualified


-- | The Collection trait defines the behavior of a sequence whose
-- elements can be traversed and accessed by an indexed subscript.
data Collection collection
  = Collection collection


countImpl :: (Collection collection) -> Prelude.Int
countImpl = Prelude.undefined


emptyImpl :: (Collection collection)
emptyImpl = Prelude.undefined


isEmptyImpl :: (Collection collection) -> Prelude.Bool
isEmptyImpl = Prelude.undefined


lengthImpl :: (Collection collection) -> Prelude.Int
lengthImpl = Prelude.undefined


getImpl :: (Collection collection) -> Prelude.Int -> Maybe b
getImpl = Prelude.undefined


appendImpl :: (Collection collection) -> (Collection collection) -> (Collection collection)
appendImpl = Prelude.undefined


firstImpl :: (Collection collection) -> Maybe b
firstImpl = Prelude.undefined


lastImpl :: (Collection collection) -> Maybe b
lastImpl = Prelude.undefined
