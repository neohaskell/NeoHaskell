{-# LANGUAGE AllowAmbiguousTypes #-}

module Traits.VariableHolder (
  VariableHolder (..),
  (<~),
) where

import Types


-- | The `VariableHolder` trait allows to define mutable variables in a semantic way.
-- It allows abstracting the fact that a variable can be defined in different ways in different
-- contexts. By default, the simplest variables are defined under `Promise`, but it could be
-- possible to define variables in other contexts too.
class VariableHolder context where
  type VariableOf context :: Type -> Type
  var :: context (VariableOf context a)
  set :: a -> VariableOf context a -> context ()
  get :: VariableOf context a -> context a


-- | The `<~` operator allows to set a value to a variable.
(<~) :: VariableHolder context => VariableOf context a -> a -> context ()
(<~) variable value = set value variable