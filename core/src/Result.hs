module Result (
  Result (..),
) where


-- | 'Result' represents a computation that might fail.
-- It is useful for error handling.
data Result value error
  = Ok value
  | Error error
