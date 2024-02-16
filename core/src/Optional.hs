module Optional (
  Optional (..),
  applyToContents,
) where


-- | 'Optional' represents an optional value.
data Optional value
  = Some value
  | None


applyToContents :: (valueInput -> valueOutput) -> Optional valueInput -> Optional valueOutput
applyToContents transformation self =
  case self of
    Some value ->
      Some (transformation value)
    None -> None