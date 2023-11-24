module Result (
  Result,
  ok,
  error,
) where

import Data.Either qualified as HsEither


-- | 'Result' represents a computation that might fail.
-- It is useful for error handling.
newtype Result value error = Result (HsEither.Either error value)


-- | 'ok' is a constructor for 'Result' that represents
-- a successful computation.
--
-- prop> ok value == Result (Right value)
ok :: value -> Result value error
ok value =
  Result (HsEither.Right value)
{-# INLINE ok #-}


-- | 'error' is a constructor for 'Result' that represents
-- a failed computation.
--
-- prop> error value == Result (Left value)
error :: error -> Result value error
error value =
  Result (HsEither.Left value)
{-# INLINE error #-}