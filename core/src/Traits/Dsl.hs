module Traits.Dsl (Dsl (..), (>>), (>>=)) where

import Pipe ((|>))
import Traits.Applicable


-- | # Dsl
--
-- This trait specifies that some type can be used as a DSL inside of
-- `do` blocks.
--
-- The `andThen` function is used to chain together expressions inside
-- of `do` blocks, in order to create a pipeline of expressions. This
-- allows to desugar `do` blocks into a series of `andThen` calls.
-- Similar to what happens with JavaScript's `async`/`await` syntax
-- being desugared into a series of `.then` calls.
--
-- In order to use a type as a DSL, it must implement this trait, at least
-- for the `andThen` function. As `ignoreAndThen` has a default implementation
-- that uses `andThen`.
class Applicable context => Dsl context where
  -- TODO: Maybe should be called `andThenImpl` to force redefinition as monomorphic function?
  andThen :: (input -> context output) -> context input -> context output


  -- TODO: Define `yield` function that is equivalent to `pure` in Haskell.
  yield :: input -> context input


  ignoreAndThen :: context output -> context input -> context output
  ignoreAndThen secondExpr self =
    self
      |> andThen (\_ -> secondExpr)


(>>=) :: Dsl context => context input -> (input -> context output) -> context output
(>>=) firstExpr continuingExpr =
  firstExpr
    |> andThen continuingExpr
{-# INLINE (>>=) #-}


(>>) :: Dsl context => context input -> context output -> context output
(>>) firstExpr secondExpr =
  firstExpr
    |> ignoreAndThen secondExpr
{-# INLINE (>>) #-}