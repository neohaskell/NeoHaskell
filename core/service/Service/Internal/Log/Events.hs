module Service.Internal.Log.Events (
  LogLevel (..),
  LogRecorded (..),
) where

import Basics
import DateTime (DateTime)
import Json qualified
import Text (Text)
import Uuid (Uuid)


-- | Log severity levels for internal framework logging.
data LogLevel
  = Debug
  | Info
  | Warn
  | Error
  deriving (Show, Eq, Ord, Generic)


instance Json.ToJSON LogLevel where
  toJSON level =
    case level of
      Debug -> Json.toJSON ("debug" :: Text)
      Info -> Json.toJSON ("info" :: Text)
      Warn -> Json.toJSON ("warn" :: Text)
      Error -> Json.toJSON ("error" :: Text)


instance Json.FromJSON LogLevel where
  parseJSON = Json.withText "LogLevel" \text ->
    case text of
      "debug" -> Json.yield Debug
      "info" -> Json.yield Info
      "warn" -> Json.yield Warn
      "error" -> Json.yield Error
      other -> Json.fail [fmt|Unknown log level: #{other}|]


-- | A recorded log event with metadata.
data LogRecorded = LogRecorded
  { logId :: !Uuid,
    timestamp :: !DateTime,
    level :: !LogLevel,
    message :: !Text
  }
  deriving (Show, Eq, Generic)


instance Json.ToJSON LogRecorded


instance Json.FromJSON LogRecorded
