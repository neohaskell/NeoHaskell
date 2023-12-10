module HaskellCompatibility.Monad (
  (>>=),
  (>>),
) where

import Dsl (Dsl (..))
import Pipe ((|>))


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