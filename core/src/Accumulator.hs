module Accumulator (
  Accumulator,
  push,
  accumulate,
) where

import Control.Applicative qualified as Ghc
import Control.Monad qualified as Monad
import Control.Monad.Trans.State qualified as GhcState
import Dsl
import Operators
import Traits.Addable
import Traits.HasDefault


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
type Accumulator value = InternalAccumulator value ()


newtype InternalAccumulator someType result
  = InternalAccumulator
      (GhcState.State someType result)


instance Dsl (InternalAccumulator someType) where
  andThen f (InternalAccumulator stateAction) = InternalAccumulator do
    stateAction Monad.>>= \result -> case f result of
      InternalAccumulator nextAction -> nextAction


  yield value =
    Ghc.pure value
      |> InternalAccumulator


-- | Pushes a value into the accumulator.
push :: (Addable value) => value -> Accumulator value
push value = do
  let pushToState accumulated = accumulated + value
  InternalAccumulator (GhcState.modify pushToState)


accumulate :: (HasDefault value) => Accumulator value -> value
accumulate (InternalAccumulator state) = GhcState.execState state defaultValue