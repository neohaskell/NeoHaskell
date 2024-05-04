module Debug.Panic (unsafelyPanicAndDie) where

import GHC.Err qualified as GhcErr
import GHC.Stack qualified as GhcStack
import HaskellCompatibility.String qualified as Compatibility
import Text (Text)


-- | Unsafely panics and dies with the given message.
-- Doesn't allow to recover from the error.
-- This function only to when you are sure that this branch
-- of code execution is not possible. DO NOT USE error as a
-- normal error handling mechanism.
unsafelyPanicAndDie :: (GhcStack.HasCallStack) => Text -> a
unsafelyPanicAndDie message = GhcErr.error (Compatibility.stringToCharList message)
{-# INLINE unsafelyPanicAndDie #-}