{-# LANGUAGE Strict #-}

-- | Settings that bound dispatcher workers, retries, and cleanup.
--
-- Use 'defaultConfig' as dispatcher settings and override fields when needed.
module Service.Integration.Dispatcher.Config (
  DispatcherConfig (..),
  defaultConfig,
) where

import Basics
import Maybe (Maybe (..))


-- | Bounds and timing for integration worker queues and processing.
data DispatcherConfig = DispatcherConfig
  { -- | Idle milliseconds before a worker is reaped. Default: 60000.
    idleTimeoutMs :: Int,
    -- | Milliseconds between reaper checks. Default: 10000.
    reaperIntervalMs :: Int,
    -- | Enables idle worker reaping. Default: True.
    enableReaper :: Bool,
    -- | Maximum queued events per worker before backpressure. Default: 100.
    workerChannelCapacity :: Int,
    -- | Milliseconds allowed to write to a full queue. Default: 5000.
    channelWriteTimeoutMs :: Int,
    -- | Per-event processing limit in milliseconds. Nothing disables it.
    -- Default: Just 30000.
    eventProcessingTimeoutMs :: Maybe Int,
    -- | Additional attempts after an IO exception. Task errors do not retry.
    -- Default: 3.
    maxEventRetries :: Int,
    -- | Initial retry delay in milliseconds; later delays double. Default: 500.
    retryBaseDelayMs :: Int
  }


defaultConfig :: DispatcherConfig
defaultConfig =
  DispatcherConfig
    { idleTimeoutMs = 60000,
      reaperIntervalMs = 10000,
      enableReaper = True,
      workerChannelCapacity = 100,
      channelWriteTimeoutMs = 5000,
      eventProcessingTimeoutMs = Just 30000,
      maxEventRetries = 3,
      retryBaseDelayMs = 500
    }
