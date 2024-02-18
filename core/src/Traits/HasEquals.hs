{-# LANGUAGE UndecidableInstances #-}

module Traits.HasEquals (
  HasEquals (..),
  (==),
  (!=),
) where

import Bool
import Data.Eq qualified as Ghc


class HasEquals value where
  equals :: value -> value -> Bool


instance {-# OVERLAPPABLE #-} (Ghc.Eq value) => HasEquals value where
  equals = (Ghc.==)


(==) :: (HasEquals value) => value -> value -> Bool
(==) = equals


(!=) :: (HasEquals value) => value -> value -> Bool
(!=) left right = not (left == right)