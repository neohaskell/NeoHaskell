module Map (
  Map,
  build,
  (-->),
) where

import Accumulator (Accumulator)
import Accumulator qualified
import Data.Map.Strict qualified as HaskellMap
import Operators
import Traits.Addable
import Traits.Defaultable (Defaultable (..))
import Traits.Equatable (Equatable)
import Traits.Ordered


newtype Map key value
  = Map (HaskellMap.Map key value)


instance Defaultable (Map key value) where
  defaultValue = empty


instance (Equatable key, Ordered key) => Addable (Map key value) where
  (+) = merge


-- | Create an empty `Map`.
empty :: Map key value
empty = Map HaskellMap.empty


-- | Merge two `Map`s.
merge :: (Equatable key, Ordered key) => Map key value -> Map key value -> Map key value
merge (Map left) (Map right) = Map (HaskellMap.union left right)


-- | Accumulator operator to build a `Map`
(-->) ::
  (Equatable key, Ordered key) =>
  key ->
  value ->
  Accumulator (Map key value)
(-->) key value =
  HaskellMap.singleton key value
    |> Map
    |> Accumulator.push


-- |
-- A builder API like this can be easily implemented through the
-- usage of the `Accumulator` module. `-->` is an operator that
-- will push a tuple into the accumulator list. After that, the
-- `build` function will convert the accumulator list into a
-- `Map`.
--
-- Example:
--
-- ```haskell
-- Map.build do
--   "a" --> 1
--   "b" --> 2
--   "c" --> 3
-- ```
build :: Accumulator (Map key value) -> Map key value
build = Accumulator.accumulate