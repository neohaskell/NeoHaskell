module Service.Internal.Log.API (
  recordLog,
  logDebug,
  logInfo,
  logWarn,
  logError,
) where

import Basics
import Channel (Channel)
import Channel qualified
import DateTime qualified
import Service.Internal.Log.Events (LogLevel (..), LogRecorded (..))
import Task (Task)
import Text (Text)
import Uuid qualified


-- | Record a log event at a given level. Non-blocking write to channel.
{-# INLINE recordLog #-}
recordLog :: Channel LogRecorded -> LogLevel -> Text -> Task _ Unit
recordLog channel level message = do
  logId <- Uuid.generate
  timestamp <- DateTime.now
  let record = LogRecorded {logId, timestamp, level, message}
  Channel.write record channel


-- | Log a debug-level message.
{-# INLINE logDebug #-}
logDebug :: Channel LogRecorded -> Text -> Task _ Unit
logDebug channel message = recordLog channel Debug message


-- | Log an info-level message.
{-# INLINE logInfo #-}
logInfo :: Channel LogRecorded -> Text -> Task _ Unit
logInfo channel message = recordLog channel Info message


-- | Log a warn-level message.
{-# INLINE logWarn #-}
logWarn :: Channel LogRecorded -> Text -> Task _ Unit
logWarn channel message = recordLog channel Warn message


-- | Log an error-level message.
{-# INLINE logError #-}
logError :: Channel LogRecorded -> Text -> Task _ Unit
logError channel message = recordLog channel Error message
