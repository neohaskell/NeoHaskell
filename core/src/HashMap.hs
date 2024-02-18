module HashMap (
  HashMap,
) where

import Data.HashMap.Strict qualified as HaskellHashMap
import Traits.Addable
import Traits.HasDefault (HasDefault (..))


newtype HashMap key value
  = HashMap (HaskellHashMap.HashMap key value)


{-
A builder API like this can be easily implemented through the
usage of the `Accumulator` module. `-->` is an operator that
will push a tuple into the accumulator list. After that, the
`build` function will convert the accumulator list into a
`HashMap`.

HashMap.build do
  "a" --> 1
  "b" --> 2
  "c" --> 3
-}

instance HasDefault (HashMap key value) where
  defaultValue = HashMap HaskellHashMap.empty


instance (Eq key) => Addable (HashMap key value) where
  (+) (HashMap left) (HashMap right) = HashMap (HaskellHashMap.union left right)