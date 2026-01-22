-- | # Auth.OAuth2.RateLimiter
--
-- Simple in-memory rate limiter for OAuth2 endpoints.
--
-- = Security Purpose
--
-- Rate limiting prevents abuse of OAuth2 endpoints:
--
-- * @/connect@ - Prevents attackers from generating excessive state tokens
-- * @/callback@ - Prevents brute-force attacks on state validation
--
-- = Algorithm
--
-- Uses a sliding window counter per key (IP address, userId, or provider).
-- Requests are counted within a configurable time window.
--
-- = Limitations
--
-- * In-memory only - does not work across multiple instances
-- * No persistence - limits reset on restart
-- * For production with multiple instances, use Redis
module Auth.OAuth2.RateLimiter (
  -- * Types
  RateLimiter (..),
  RateLimitConfig (..),
  RateLimitResult (..),

  -- * Construction
  new,

  -- * Default Configs
  defaultConnectConfig,
  defaultCallbackConfig,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import Collection qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Data.Time.Clock.POSIX qualified as GhcPosix
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Prelude qualified as GhcPrelude
import Task (Task)
import Task qualified
import Text (Text)


-- | Rate limit configuration.
data RateLimitConfig = RateLimitConfig
  { -- | Maximum requests allowed in the window
    maxRequests :: Int
  , -- | Time window in seconds
    windowSeconds :: Int
  }
  deriving (Generic, Show, Eq)


-- | Result of a rate limit check.
data RateLimitResult
  = -- | Request is allowed
    Allowed
  | -- | Request is rate limited (retry after N seconds)
    Limited Int
  deriving (Generic, Show, Eq)


-- | Rate limiter interface.
data RateLimiter = RateLimiter
  { -- | Check if a request should be rate limited.
    -- Takes a key (e.g., IP address, userId) and returns the result.
    checkLimit :: Text -> Task Text RateLimitResult
  }


-- | Internal state for tracking request counts.
data RequestBucket = RequestBucket
  { -- | Timestamps of requests in the current window
    requestTimes :: Array Int
  , -- | Start of the current window (Unix seconds)
    windowStart :: Int
  }
  deriving (Generic, Show, Eq)


-- | Internal storage type.
type Storage = ConcurrentVar (Map Text RequestBucket)


-- | Default rate limit for /connect endpoint.
--
-- 10 requests per minute per key (userId).
-- This is generous for normal use but limits abuse.
defaultConnectConfig :: RateLimitConfig
defaultConnectConfig =
  RateLimitConfig
    { maxRequests = 10
    , windowSeconds = 60
    }


-- | Default rate limit for /callback endpoint.
--
-- 20 requests per minute per key (IP address).
-- Higher limit because legitimate callbacks may retry.
defaultCallbackConfig :: RateLimitConfig
defaultCallbackConfig =
  RateLimitConfig
    { maxRequests = 20
    , windowSeconds = 60
    }


-- | Create a new rate limiter with the given configuration.
--
-- Includes a background reaper that cleans up old buckets every 5 minutes.
new :: RateLimitConfig -> Task Text RateLimiter
new config = do
  storage <- ConcurrentVar.containing Map.empty
  -- Start background reaper for cleanup
  _ <- startReaper storage config.windowSeconds
  Task.yield
    RateLimiter
      { checkLimit = checkLimitImpl storage config
      }


-- | Check if a request should be rate limited.
checkLimitImpl :: Storage -> RateLimitConfig -> Text -> Task Text RateLimitResult
checkLimitImpl storage config key = do
  nowSeconds <- getCurrentTimeSeconds
  let windowStart = nowSeconds - config.windowSeconds

  -- Atomically check and update the bucket
  storage
    |> ConcurrentVar.modifyReturning
      ( \store -> do
          let maybeBucket = store |> Map.get key
          let currentBucket = case maybeBucket of
                Just bucket -> bucket
                Nothing -> RequestBucket {requestTimes = Array.empty, windowStart = nowSeconds}

          -- Filter out requests outside the window
          let validTimes =
                currentBucket.requestTimes
                  |> Collection.takeIf (\t -> t > windowStart)

          let requestCount = Array.length validTimes

          case requestCount >= config.maxRequests of
            True -> do
              -- Rate limited - calculate retry-after
              let oldestRequest = case Array.first validTimes of
                    Just t -> t
                    Nothing -> nowSeconds
              let retryAfter = oldestRequest + config.windowSeconds - nowSeconds
              Task.yield (store, Limited (max 1 retryAfter))
            False -> do
              -- Allowed - add this request
              let newTimes = validTimes |> Array.push nowSeconds
              let newBucket = RequestBucket {requestTimes = newTimes, windowStart = currentBucket.windowStart}
              let newStore = store |> Map.set key newBucket
              Task.yield (newStore, Allowed)
      )


-- | Reaper interval in milliseconds (5 minutes)
reaperIntervalMs :: Int
reaperIntervalMs = 300000


-- | Start background reaper that cleans up old buckets.
startReaper :: Storage -> Int -> Task Text Unit
startReaper storage windowSeconds = do
  let reaperLoop :: Task Text Unit
      reaperLoop = do
        AsyncTask.sleep reaperIntervalMs
          |> Task.mapError (\_ -> "reaper sleep error" :: Text)
        nowSeconds <- getCurrentTimeSeconds
        let cutoff = nowSeconds - windowSeconds

        -- Remove buckets with no recent requests
        storage
          |> ConcurrentVar.modify
            ( \store ->
                store
                  |> Map.entries
                  |> Array.reduce
                      ( \(key, bucket) acc -> do
                          let hasRecentRequests =
                                bucket.requestTimes
                                  |> Collection.takeIf (\t -> t > cutoff)
                                  |> Array.length
                                  |> (\len -> len > 0)
                          case hasRecentRequests of
                            True -> acc |> Map.set key bucket
                            False -> acc
                      )
                      Map.empty
            )
        reaperLoop

  _ <- AsyncTask.run reaperLoop
    |> Task.mapError (\_ -> "reaper start error" :: Text)
  Task.yield unit


-- | Get current time as Unix seconds.
getCurrentTimeSeconds :: forall error. Task error Int
getCurrentTimeSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: Int = GhcPrelude.floor (GhcPrelude.realToFrac posixTime :: GhcPrelude.Double)
  Task.yield seconds
