module Applicable (
  Applicable,
  Applicative (..),
  apply,
) where

import Control.Applicative (Applicative (..))


-- | The Applicable trait defines the behavior of a type that can be
-- contains a function that can be applied to the contents of another type.
--
-- If you want to make a type applicable, you need to implement the
-- `Applicative` trait.
type Applicable applicable =
  Applicative applicable


-- | The `apply` function applies a function to each element in a
-- applicable value.
apply ::
  (Applicable applicable) =>
  applicable (typeA -> typeB) ->
  applicable typeA ->
  applicable typeB
apply = (<*>)