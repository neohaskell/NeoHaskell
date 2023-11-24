module Debug.ToDo (todo) where

import Debug.Panic (unsafelyPanicAndDie)
import GHC.Stack.Types qualified as GhcStack


todo :: (GhcStack.HasCallStack) => a
todo = unsafelyPanicAndDie "TODO: Not Implemented Yet"
{-# INLINE todo #-}