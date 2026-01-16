module Service.Event.StreamId (
  StreamId (..),
  ToStreamId (..),
  new,
  toText,
  fromText,
  fromTextUnsafe,
  maxLength,
) where

import Basics
import Data.Hashable (Hashable)
import Json qualified
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import Uuid (Uuid)
import Uuid qualified


-- | Maximum allowed length for a StreamId (1024 characters).
--
-- StreamIds are used as keys in various places (database indexes, map keys,
-- log messages) and excessively long IDs can cause:
-- - Performance degradation in index lookups
-- - Log spam and truncation
-- - Potential DoS vectors from unbounded input
maxLength :: Int
maxLength = 1024


newtype StreamId = StreamId Text
  deriving (Eq, Show, Ord, Generic)


instance Hashable StreamId


instance Json.ToJSON StreamId


instance Json.FromJSON StreamId


new :: Task _ StreamId
new = do
  uuid <- Uuid.generate |> Task.map Uuid.toText
  StreamId uuid |> Task.yield


toText :: StreamId -> Text
toText (StreamId streamId) = streamId


-- | Create a StreamId from text with validation.
--
-- Returns @Err@ if the text exceeds 'maxLength' (1024 characters).
-- Use 'fromTextUnsafe' only when you're certain the input is valid
-- (e.g., reading from a trusted database).
fromText :: Text -> Result Text StreamId
fromText streamIdText = do
  let len = Text.length streamIdText
  if len > maxLength
    then Err [fmt|StreamId exceeds maximum length of #{maxLength} characters (got #{len})|]
    else Ok (StreamId streamIdText)


-- | Create a StreamId from text without validation.
--
-- CAUTION: Only use this when reading from trusted sources (e.g., database)
-- where the value was previously validated. For user input, use 'fromText'.
fromTextUnsafe :: Text -> StreamId
fromTextUnsafe streamIdText = StreamId streamIdText


-- | Typeclass for types that can be converted to StreamId.
--
-- Note: These conversions are considered "safe" because:
-- - StreamId → StreamId: Already validated
-- - Uuid → StreamId: UUIDs are fixed-length (36 chars), always valid
--
-- For Text, use 'fromText' which validates length.
class ToStreamId idType where
  toStreamId :: idType -> StreamId


instance ToStreamId StreamId where
  toStreamId streamId = streamId


instance ToStreamId Uuid where
  toStreamId uuid = uuid |> Uuid.toText |> StreamId