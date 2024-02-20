module HaskellCompatibility.Monad (
  (>>=),
  (>>),
  _andThen,
  _yield,
) where

import Control.Monad qualified as Monad
import Dsl (Dsl (..))
import Pipe ((|>))


(>>=) :: Dsl context => context input -> (input -> context output) -> context output
(>>=) firstExpr continuingExpr =
  firstExpr
    |> andThen continuingExpr
{-# INLINE (>>=) #-}


_andThen :: Monad.Monad context => (input -> context output) -> context input -> context output
_andThen continuingExpr firstExpr =
  firstExpr Monad.>>= continuingExpr
{-# INLINE _andThen #-}


_yield :: Monad.Monad context => input -> context input
_yield value =
  Monad.return value
{-# INLINE _yield #-}


(>>) :: Dsl context => context input -> context output -> context output
(>>) firstExpr secondExpr =
  firstExpr
    |> ignoreAndThen secondExpr
{-# INLINE (>>) #-}