module Result (
  Result (..),
  applyToError,
  applyToOk,
) where


-- | 'Result' represents a computation that might fail.
-- It is useful for error handling.
data Result value error
  = Ok value
  | Error error


applyToError :: (errorInput -> errorOutput) -> Result value errorInput -> Result value errorOutput
applyToError transformation self =
  case self of
    Ok value ->
      Ok value
    Error error ->
      Error (transformation error)


applyToOk :: (valueInput -> valueOutput) -> Result valueInput error -> Result valueOutput error
applyToOk transformation self =
  case self of
    Ok value ->
      Ok (transformation value)
    Error error ->
      Error error