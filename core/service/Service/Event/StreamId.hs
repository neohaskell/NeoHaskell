module Service.Event.StreamId (
  StreamId (..),
  new,
  toText,
) where

import Core hiding (toText)
import Task qualified
import Uuid qualified


newtype StreamId = StreamId Uuid
  deriving (Eq, Show, Ord, Generic)


new :: Task _ StreamId
new = do
  uuid <- Uuid.generate
  StreamId uuid |> Task.yield


toText :: StreamId -> Text
toText (StreamId uuid) = Uuid.toText uuid