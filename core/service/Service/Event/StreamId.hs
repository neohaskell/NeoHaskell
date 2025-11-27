module Service.Event.StreamId (
  StreamId (..),
  ToStreamId (..),
  new,
  toText,
  fromText,
) where

import Basics
import Json qualified
import Task (Task)
import Task qualified
import Text (Text)
import Uuid (Uuid)
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


class ToStreamId idType where
  toStreamId :: idType -> StreamId


instance ToStreamId StreamId where
  toStreamId id = id


instance ToStreamId Text where
  toStreamId text = StreamId text


instance ToStreamId Uuid where
  toStreamId uuid = uuid |> Uuid.toText |> StreamId