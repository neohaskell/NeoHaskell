module Traits.Ordered (
  Ordered,
  (<=),
  (>=),
  (<),
  (>),
  max,
  min,
) where

import Bool
import Data.Ord qualified as Ghc


type Ordered = Ghc.Ord


(<=) :: (Ordered value) => value -> value -> Bool
(<=) = (Ghc.<=)


(>=) :: (Ordered value) => value -> value -> Bool
(>=) = (Ghc.>=)


(<) :: (Ordered value) => value -> value -> Bool
(<) = (Ghc.<)


(>) :: (Ordered value) => value -> value -> Bool
(>) = (Ghc.>)


max :: (Ordered value) => value -> value -> value
max = Ghc.max


min :: (Ordered value) => value -> value -> value
min = Ghc.min
