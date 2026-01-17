-- | High-performance JWKS manager for JWT authentication.
-- Provides lock-free key lookups with background refresh.
--
-- Architecture:
-- - Hot path (per request): AtomicVar.peek for lock-free reads
-- - Control plane (background): Periodic refresh, missing-key handling
--
-- Performance target: 50k req/s with lock-free key lookups.
module Auth.Jwks (
  -- * Manager
  JwksManager,
  startManager,
  stopManager,
  -- * Key lookup (hot path)
  lookupKey,
  getAllKeys,
  -- * Refresh control
  requestRefresh,
  -- * Types
  KeySnapshot (..),
) where

import Array (Array)
import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import AtomicVar (AtomicVar)
import AtomicVar qualified
import Auth.Config (AuthConfig (..))
import Auth.Discovery qualified as Discovery
import Auth.Error (DiscoveryError)
import Basics
import Control.Lens qualified as Lens
import Crypto.JOSE qualified as Jose
import Data.Int qualified as GhcInt
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Task (Task)
import Task qualified
import Data.Time.Clock.POSIX qualified as GhcPosix
import Prelude qualified
import Text (Text)
import ToText (toText)


-- | Snapshot of cached keys for lock-free reads.
data KeySnapshot = KeySnapshot
  { keysByKid :: Map Text Jose.JWK,
    -- ^ Current keys indexed by kid
    fetchedAt :: GhcInt.Int64,
    -- ^ Unix timestamp when keys were fetched
    isStale :: Bool
    -- ^ True if refresh failed and keys are beyond max stale time
  }
  deriving (Show)


-- | Empty key snapshot for initialization.
emptySnapshot :: KeySnapshot
emptySnapshot =
  KeySnapshot
    { keysByKid = Map.empty,
      fetchedAt = 0,
      isStale = True
    }


-- | JWKS Manager with background refresh.
data JwksManager = JwksManager
  { config :: AuthConfig,
    -- ^ Auth configuration (issuer, jwksUri, etc.)
    keySnapshot :: AtomicVar KeySnapshot,
    -- ^ Hot path: lock-free reads
    refreshTask :: AtomicVar (Maybe (AsyncTask Text Unit)),
    -- ^ Background refresh task (if running)
    isRunning :: AtomicVar Bool
    -- ^ Whether the manager is running
  }


-- | Start a new JWKS manager with background refresh.
-- Performs initial key fetch before returning.
startManager :: AuthConfig -> Task Text JwksManager
startManager config = do
  -- Create state
  snapshotVar <- AtomicVar.containing emptySnapshot
  refreshTaskVar <- AtomicVar.containing Nothing
  runningVar <- AtomicVar.containing True

  let manager =
        JwksManager
          { config = config,
            keySnapshot = snapshotVar,
            refreshTask = refreshTaskVar,
            isRunning = runningVar
          }

  -- Perform initial key fetch
  refreshResult <-
    refreshKeys manager
      |> Task.mapError (\err -> [fmt|Initial JWKS fetch failed: #{toText err}|])
      |> Task.asResult
  case refreshResult of
    Err err ->
      Task.throw err
    Ok () -> do
      -- Start background refresh loop
      backgroundTask <- AsyncTask.run (refreshLoop manager)
      AtomicVar.set (Just backgroundTask) refreshTaskVar
      Task.yield manager


-- | Stop the JWKS manager and cancel background refresh.
stopManager :: JwksManager -> Task Text Unit
stopManager manager = do
  -- Signal stop
  AtomicVar.set False manager.isRunning

  -- Cancel background task if running
  maybeTask <- AtomicVar.peek manager.refreshTask
  case maybeTask of
    Nothing -> Task.yield ()
    Just task -> AsyncTask.cancel task


-- | Look up a key by kid (key ID).
-- This is the hot path - uses lock-free reads.
lookupKey :: Text -> JwksManager -> Task err (Maybe Jose.JWK)
lookupKey kid manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  Task.yield (Map.get kid snapshot.keysByKid)


-- | Get all cached keys.
-- Used when no kid is specified in the token.
getAllKeys :: JwksManager -> Task err (Array Jose.JWK)
getAllKeys manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  Task.yield (Map.values snapshot.keysByKid)


-- | Request an immediate refresh (e.g., when a kid is not found).
-- This is rate-limited to prevent thundering herd.
requestRefresh :: JwksManager -> Task err Unit
requestRefresh manager = do
  -- Check if we should refresh (simple rate limiting)
  now <- getCurrentSeconds
  snapshot <- AtomicVar.peek manager.keySnapshot
  let timeSinceRefresh = now - snapshot.fetchedAt
  let cooldownSeconds = manager.config.missingKidCooldownSeconds

  -- Only refresh if cooldown has passed
  case timeSinceRefresh >= cooldownSeconds of
    False -> Task.yield () -- Still in cooldown
    True -> do
      -- Trigger refresh (ignore errors - background loop will retry)
      _ <- refreshKeys manager |> Task.asResult
      Task.yield ()


-- | Background refresh loop.
refreshLoop :: JwksManager -> Task Text Unit
refreshLoop manager = do
  Task.forever do
    -- Wait for refresh interval
    let intervalSeconds = manager.config.refreshIntervalSeconds
    AsyncTask.sleep (fromIntegral intervalSeconds * 1000)

    -- Check if still running
    running <- AtomicVar.peek manager.isRunning
    case running of
      False -> Task.throw "Manager stopped"
      True -> do
        -- Perform refresh (ignore errors, will retry next interval)
        _ <- refreshKeys manager |> Task.asResult
        Task.yield ()


-- | Refresh keys from the JWKS endpoint.
refreshKeys :: JwksManager -> Task DiscoveryError Unit
refreshKeys manager = do
  -- Fetch keys from JWKS URI
  keysResult <- Discovery.fetchJwks manager.config.jwksUri
  case keysResult of
    Err err -> Task.throw err
    Ok keys -> do
      -- Build new snapshot
      now <- getCurrentSeconds
      let newSnapshot = buildSnapshot keys now
      -- Atomically update
      AtomicVar.set newSnapshot manager.keySnapshot
      Task.yield ()


-- | Build a key snapshot from an array of JWKs.
buildSnapshot :: Array Jose.JWK -> GhcInt.Int64 -> KeySnapshot
buildSnapshot keys now = do
  let keysByKid = keys |> Array.map extractKidAndKey |> Array.dropIf isNothing |> Array.map unwrapJust |> Map.fromArray
  KeySnapshot
    { keysByKid = keysByKid,
      fetchedAt = now,
      isStale = False
    }


-- | Extract (kid, JWK) tuple from a JWK.
extractKidAndKey :: Jose.JWK -> Maybe (Text, Jose.JWK)
extractKidAndKey jwk = do
  case jwk Lens.^. Jose.jwkKid of
    Nothing -> Nothing
    Just kid -> Just (kid, jwk)


-- | Check if Maybe is Nothing (for filtering).
isNothing :: Maybe value -> Bool
isNothing val =
  case val of
    Nothing -> True
    Just _ -> False


-- | Unwrap Just (for mapping after filtering).
unwrapJust :: Maybe value -> value
unwrapJust val =
  case val of
    Just v -> v
    Nothing -> panic "unwrapJust called on Nothing"


-- | Get current Unix timestamp in seconds.
getCurrentSeconds :: Task err GhcInt.Int64
getCurrentSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: GhcInt.Int64 = Prelude.floor (Prelude.realToFrac posixTime :: Prelude.Double)
  Task.yield seconds
