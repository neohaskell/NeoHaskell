{-# LANGUAGE TemplateHaskell #-}

module Test.CompileTime (
  -- * Compile-time assertions
  shouldNotTypecheck,
  shouldTypecheck,

  -- * Re-exports from should-not-typecheck
  NFData (..),
) where

import Control.DeepSeq (NFData (..), force)
import Control.Exception (evaluate)
import Core
import Task qualified
import Test.ShouldNotTypecheck qualified as ShouldNotTypecheck


-- | Assert that an expression should not typecheck.
-- This is useful for testing type-level constraints and validation.
-- Note: Requires {-# OPTIONS_GHC -fdefer-type-errors #-} in test files.
--
-- Example:
-- @
-- it "should not allow adding Int to String" \_ -> do
--   shouldNotTypecheck (5 + "hello")
-- @
--
-- Example with type families:
-- @
-- it "should reject invalid type family application" \_ -> do
--   shouldNotTypecheck (undefined :: MyTypeFamily Int String)
-- @
shouldNotTypecheck :: forall value. (HasCallStack, NFData value) => ((() ~ ()) => value) -> Task Text Unit
shouldNotTypecheck expression = do
  -- shouldNotTypecheck from the library returns an Assertion (IO ())
  -- We need to convert it to our Task type
  Task.fromIO (ShouldNotTypecheck.shouldNotTypecheck expression)


-- | Assert that an expression should typecheck.
-- This is the opposite of shouldNotTypecheck - it verifies that valid code compiles.
-- Note: This is mainly for documentation - if code typechecks, it compiles normally.
--
-- Example:
-- @
-- it "should allow adding two Ints" \_ -> do
--   shouldTypecheck (5 + 3)
-- @
--
-- Example with type families:
-- @
-- it "should accept valid type family application" \_ -> do
--   shouldTypecheck (undefined :: MyTypeFamily Int Int)
-- @
shouldTypecheck :: forall value. (NFData value) => value -> Task Text Unit
shouldTypecheck expression = do
  -- Force evaluation to ensure the expression is well-typed
  -- If it doesn't typecheck, compilation will fail
  _ <- Task.fromIO (evaluate (force expression))
  Task.yield unit