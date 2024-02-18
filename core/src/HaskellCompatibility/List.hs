module HaskellCompatibility.List (
  HaskellList,
  fromList,
  fromListN,
  toList,
) where

import Array (Array (..))
import Data.Vector qualified as Vector
import Int
import Pipe ((|>))


-- | A type alias for the Haskell list type.
-- It is exposed like this to ensure that it's
-- usage is explicit, and we make sure that
-- it is only used for compatibility with
-- Haskell.
type HaskellList item = [item]


-- | INTERNAL DO NOT USE
-- Required for compatibility in order to construct literals from a List of items.
-- Which is what the GHC compiler gives us when we write a list literal.
fromList :: HaskellList item -> Array item
fromList list =
  Vector.fromList list
    |> INTERNAL_CORE_ARRAY_CONSTRUCTOR


-- | INTERNAL DO NOT USE
-- Required for compatibility in order to construct literals from a List of items.
-- Which is what the GHC compiler gives us when we write a list literal.
fromListN :: Int -> HaskellList item -> Array item
fromListN n list =
  Vector.fromListN n list
    |> INTERNAL_CORE_ARRAY_CONSTRUCTOR


-- | INTERNAL DO NOT USE
-- Required for compatibility in order to deconstruct literals to a List of items.
-- Which is what the GHC compiler gives us when we write a list literal.
toList :: Array item -> HaskellList item
toList (INTERNAL_CORE_ARRAY_CONSTRUCTOR vector) =
  Vector.toList vector
