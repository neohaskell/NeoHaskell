module Traits.Thenable (
  Thenable,
  Monad (..),
  andThen,
) where

import Control.Monad (Monad (..), (=<<))


-- | The `Thenable` trait defines that a type can be used in a
-- chain of functions that return the same type.
type Thenable thenable = Monad thenable


-- | The `andThen` function chains two functions that return the same
-- type.
andThen ::
  (Thenable thenable) =>
  (a -> thenable b) ->
  thenable a ->
  thenable b
andThen = (=<<)
