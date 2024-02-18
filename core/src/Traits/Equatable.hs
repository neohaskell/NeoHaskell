{-# LANGUAGE UndecidableInstances #-}

module Traits.Equatable (
  Equatable,
  (==),
  (!=),
) where

import Bool
import Data.Eq qualified as Ghc


-- | `Equatable` trait is used to define the equality operator for a type.
type Equatable = Ghc.Eq


(==) :: (Equatable value) => value -> value -> Bool
(==) = (Ghc.==)


(!=) :: (Equatable value) => value -> value -> Bool
(!=) = (Ghc./=)