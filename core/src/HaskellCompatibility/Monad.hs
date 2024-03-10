module HaskellCompatibility.Monad (
  _andThen,
  _yield,
) where

import Control.Monad qualified as Ghc


_andThen :: (Ghc.Monad context) => (input -> context output) -> context input -> context output
_andThen continuingExpr firstExpr =
  firstExpr Ghc.>>= continuingExpr
{-# INLINE _andThen #-}


_yield :: (Ghc.Monad context) => input -> context input
_yield value =
  Ghc.return value
{-# INLINE _yield #-}
