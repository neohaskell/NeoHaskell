module Traits.Mappable (
  Mappable,
  Functor (..),
  map,
) where

import Data.Functor (Functor (..))


-- | The Mappable trait defines the behavior of a type that can be
-- mapped over.
--
-- If you want to make a type mappable, you need to implement the
-- `Functor` trait.
type Mappable mappable =
  Functor mappable


-- | The `map` function applies a function to each element in a
-- mappable value.
map ::
  (Mappable mappable) =>
  (typeA -> typeB) ->
  mappable typeA ->
  mappable typeB
map = fmap