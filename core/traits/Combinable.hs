module Combinable (
  Combinable,
  Data.Monoid.Monoid (..),
  empty,
) where

import Data.Monoid qualified


type Combinable = Data.Monoid.Monoid


-- | The empty combinable thing. This includes arrays, lists, and text.
empty :: (Combinable combinable) => combinable
empty =
  Data.Monoid.mempty
