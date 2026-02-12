module Service.Internal.Log (
  -- * Types
  LogRecorded (..),
  LogLevel (..),

  -- * Logging API
  logDebug,
  logInfo,
  logWarn,
  logError,
  recordLog,

  -- * Worker
  startWorker,
) where

import Service.Internal.Log.API (logDebug, logError, logInfo, logWarn, recordLog)
import Service.Internal.Log.Events (LogLevel (..), LogRecorded (..))
import Service.Internal.Log.Worker (startWorker)
