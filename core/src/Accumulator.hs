module Accumulator (
  Accumulator,
  AccumulatorDsl (..),
  push,
  accumulate,
  update,
  andThen,
  yield,
) where

import Basics
import Control.Monad.Trans.State qualified as GhcState
import Traits.Appendable
import Traits.Default
import Traits.Thenable qualified as Thenable


-- | `Accumulator` is a type that allows to accumulate values in
-- a monadic way. As long as the content of the accumulator implements
-- the `Combinable` and `HasDefault` traits, it is possible to use the `do` notation to
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


andThen ::
  (input -> AccumulatorDsl someType output) -> AccumulatorDsl someType input -> AccumulatorDsl someType output
andThen callback self =
  self.value
    |> Thenable.andThen (callback .> value)
    |> AcculumatorDsl


yield :: value -> AccumulatorDsl someType value
yield value =
  Thenable.yield value
    |> AcculumatorDsl


-- | Pushes a value into the accumulator.
push :: (Appendable value) => value -> Accumulator value
push value = do
  let pushToState accumulated = accumulated ++ value
  AcculumatorDsl (GhcState.modify pushToState)


-- | Updates the accumulator with a callback.
update :: (value -> value) -> Accumulator value
update callback = do
  let updateState state = state |> callback
  AcculumatorDsl (GhcState.modify updateState)


accumulate :: (Default value) => Accumulator value -> value
accumulate (AcculumatorDsl state) = GhcState.execState state defaultValue