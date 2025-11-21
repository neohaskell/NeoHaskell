module Service.Event.StreamId (
  StreamId (..),
  new,
  toText,
  fromText,
) where

import Basics
import Json qualified
import Task (Task)
import Task qualified
import Text (Text)
import Uuid qualified


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON StreamId


instance Json.FromJSON StreamId


new :: Task _ StreamId
new = do
  uuid <- Uuid.generate |> Task.map Uuid.toText
  StreamId uuid |> Task.yield


toText :: StreamId -> Text
toText (StreamId id) = id


fromText :: Text -> StreamId
fromText id = StreamId id