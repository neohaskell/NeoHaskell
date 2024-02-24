module HaskellCompatibility.Monad (
  _andThen,
  _yield,
  DslMonad (..),
) where

import Control.Applicative qualified as Ghc
import Control.Monad qualified as Ghc
import Debug (todo)
import Traits.Dsl


newtype DslMonad a = DslMonad {runDslMonad :: forall dsl. Dsl dsl => dsl a}


instance Ghc.Functor DslMonad where
  fmap = todo


instance Ghc.Applicative DslMonad where
  pure x = DslMonad (yield x)
  (<*>) = todo


instance Ghc.Monad DslMonad where
  return = Ghc.pure
  (>>=) = todo


_andThen :: Ghc.Monad context => (input -> context output) -> context input -> context output
_andThen continuingExpr firstExpr =
  firstExpr Ghc.>>= continuingExpr
{-# INLINE _andThen #-}


_yield :: Ghc.Monad context => input -> context input
_yield value =
  Ghc.return value
{-# INLINE _yield #-}
