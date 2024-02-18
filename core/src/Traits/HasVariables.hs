{-# LANGUAGE AllowAmbiguousTypes #-}

module Traits.HasVariables (
  HasVariables (..),
  (<~),
) where

import Types


-- | The `HasVariables` trait allows to define mutable variables in a semantic way.
-- It allows abstracting the fact that a variable can be defined in different ways in different
-- contexts. By default, the simplest variables are defined under `Promise`, but it could be
-- possible to define variables in other contexts too.
class HasVariables context where
  type VariableOf context :: Type -> Type
  var :: context (VariableOf context a)
  set :: a -> VariableOf context a -> context ()
  get :: VariableOf context a -> context a


-- | The `<~` operator allows to set a value to a variable.
(<~) :: HasVariables context => VariableOf context a -> a -> context ()
(<~) variable value = set value variable