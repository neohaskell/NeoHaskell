module Service.Event.StreamId (
  StreamId (..),
  new,
) where

import Core
import Task qualified
import Uuid qualified


newtype StreamId = StreamId Uuid
  deriving (Eq, Show, Ord, Generic)


new :: Task _ StreamId
new = do
  uuid <- Uuid.generate
  StreamId uuid |> Task.yield
