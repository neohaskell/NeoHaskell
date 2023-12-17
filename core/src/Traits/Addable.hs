module Traits.Addable (
  Addable (..),
) where

import Array qualified
import String qualified
import Types


-- | # Addable
--
-- The 'Addable' trait is for types that can be added together.
-- Examples of types that are 'Addable' are 'Int', 'Float', 'Double', 'String', etc.
class Addable value where
  infixl 6 +
  (+) :: value -> value -> value


instance Addable String where
  a + b = String.append a b


instance Addable (Array item) where
  a + b = Array.appendArray a b