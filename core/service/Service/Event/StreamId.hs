module Service.Event.StreamId (
  StreamId (..),
  new,
  toText,
  fromText,
) where

import Core hiding (toText)
import Task qualified
import Uuid qualified


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


new :: Task _ StreamId
new = do
  uuid <- Uuid.generate |> Task.map Uuid.toText
  StreamId uuid |> Task.yield


toText :: StreamId -> Text
toText (StreamId id) = id


fromText :: Text -> StreamId
fromText id = StreamId id