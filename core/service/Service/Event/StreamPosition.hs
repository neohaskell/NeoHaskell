module Service.Event.StreamPosition (
  StreamPosition (..),
  new,
) where

import Core
import Json (FromJSON, ToJSON)


newtype StreamPosition = StreamPosition Int64
  deriving (Eq, Show, Ord, Generic)


instance ToJSON StreamPosition


instance FromJSON StreamPosition


new :: Int64 -> Maybe StreamPosition
new position = do
  if position < 0
    then Nothing
    else Just (StreamPosition position)


instance Default StreamPosition where
  def = StreamPosition 0