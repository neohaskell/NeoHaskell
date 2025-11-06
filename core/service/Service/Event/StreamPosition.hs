module Service.Event.StreamPosition (
  StreamPosition (..),
  new,
) where

import Core


newtype StreamPosition = StreamPosition Int64
  deriving (Eq, Show, Ord, Generic)


new :: Int64 -> Maybe StreamPosition
new position = do
  if position < 0
    then Nothing
    else Just (StreamPosition position)
