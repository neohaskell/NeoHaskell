-- | Retry configuration for HTTP outbound integrations.
--
-- This module provides retry policies with exponential backoff
-- for handling transient failures in external API calls.
--
-- == Retry Strategy
--
-- By default, requests are retried up to 3 times with exponential backoff:
--
-- * Attempt 1: immediate
-- * Attempt 2: wait ~1 second
-- * Attempt 3: wait ~2 seconds
-- * Attempt 4: fail
--
-- Jitter (0-25% of delay) is added to prevent thundering herd.
--
-- == Retryable Errors
--
-- Only transient errors are retried by default:
--
-- * 429 Too Many Requests
-- * 500 Internal Server Error
-- * 502 Bad Gateway
-- * 503 Service Unavailable
-- * 504 Gateway Timeout
--
-- Client errors (4xx except 429) are NOT retried as they indicate
-- a problem with the request itself.
module Integration.Http.Retry
  ( -- * Retry Configuration
    Retry (..)

    -- * Preset Configurations
  , defaultRetry
  , noRetry

    -- * Custom Configuration
  , withRetries
  ) where

import Array (Array)
import Basics
import Json qualified


-- | Retry configuration for transient failures.
--
-- @
-- Http.Request
--   { ...
--   , retry = Http.Retry
--       { maxAttempts = 5
--       , initialDelayMs = 500
--       , maxDelayMs = 10000
--       , retryableStatuses = [429, 500, 502, 503, 504]
--       }
--   }
-- @
data Retry = Retry
  { -- | Maximum number of attempts (including initial request)
    --
    -- Default: 3 (1 initial + 2 retries)
    maxAttempts :: Int
  , -- | Initial delay in milliseconds before first retry
    --
    -- Default: 1000 (1 second)
    initialDelayMs :: Int
  , -- | Maximum delay in milliseconds (caps exponential growth)
    --
    -- Default: 30000 (30 seconds)
    maxDelayMs :: Int
  , -- | HTTP status codes that trigger retry
    --
    -- Default: [429, 500, 502, 503, 504]
    retryableStatuses :: Array Int
  }
  deriving (Eq, Show, Generic)


instance Json.ToJSON Retry
instance Json.FromJSON Retry


-- | Default retry configuration.
--
-- * 3 attempts (1 initial + 2 retries)
-- * 1 second initial delay
-- * 30 second max delay
-- * Retries on: 429, 500, 502, 503, 504
--
-- @
-- Http.Request
--   { ...
--   , retry = Http.defaultRetry
--   }
-- @
defaultRetry :: Retry
defaultRetry = Retry
  { maxAttempts = 3
  , initialDelayMs = 1000
  , maxDelayMs = 30000
  , retryableStatuses = [429, 500, 502, 503, 504]
  }


-- | No retries - fail immediately on any error.
--
-- Use for fire-and-forget requests where retries don't make sense:
--
-- @
-- -- Slack webhook - fire and forget
-- Http.Request
--   { ...
--   , retry = Http.noRetry
--   }
-- @
noRetry :: Retry
noRetry = Retry
  { maxAttempts = 1
  , initialDelayMs = 0
  , maxDelayMs = 0
  , retryableStatuses = []
  }


-- | Custom retry count with default backoff settings.
--
-- @
-- -- 5 attempts with default delays
-- Http.Request
--   { ...
--   , retry = Http.withRetries 5
--   }
-- @
withRetries :: Int -> Retry
withRetries n = defaultRetry { maxAttempts = n }
