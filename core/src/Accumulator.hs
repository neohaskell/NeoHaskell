module Accumulator (
  Accumulator,
  AccumulatorDsl (..),
  push,
  accumulate,
) where

import Control.Applicative qualified as Ghc
import Control.Monad.Trans.State qualified as GhcState
import Dsl
import HaskellCompatibility.Monad qualified as Monad
import Label
import Operators
import Record
import Traits.Addable
import Traits.Defaultable


-- | `Accumulator` is a type that allows to accumulate values in
-- a monadic way. As long as the content of the accumulator implements
-- the `Addable` and `HasDefault` traits, it is possible to use the `do` notation to
-- accumulate values.
--
-- Example:
--
-- ```haskell
-- foo :: Array Int
-- foo = Accumulator.accumulate do
--   Accumulator.push [1]
--   Accumulator.push [2]
--   Accumulator.push [3]
-- ```
type Accumulator value = AccumulatorDsl value ()


data AccumulatorDsl someType result = AcculumatorDsl
  { value :: GhcState.State someType result
  }


-- TODO: Make traits have the `impl` suffix.
andThenImpl :: (input -> AccumulatorDsl someType output) -> AccumulatorDsl someType input -> AccumulatorDsl someType output
andThenImpl callback self =
  self.value
    |> Monad._andThen (callback .> value)
    |> AcculumatorDsl


yieldImpl :: value -> AccumulatorDsl someType value
yieldImpl value =
  Monad._yield value
    |> AcculumatorDsl


instance Dsl (AccumulatorDsl someType) where
  andThen = andThenImpl


  yield = yieldImpl


-- | Pushes a value into the accumulator.
push :: (Addable value) => value -> Accumulator value
push value = do
  let pushToState accumulated = accumulated + value
  AcculumatorDsl (GhcState.modify pushToState)


accumulate :: (Defaultable value) => Accumulator value -> value
accumulate (AcculumatorDsl state) = GhcState.execState state defaultValue