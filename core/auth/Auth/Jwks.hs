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
  JwksManager (..),
  startManager,
  stopManager,
  -- * Key lookup (hot path)
  lookupKey,
  getAllKeys,
  getJwkSet,
  getJwkSetForKid,
  getJwkSetForKidWithRefresh,
  -- * Staleness check
  checkStaleness,
  -- * Refresh control
  requestRefresh,
  -- * Types
  KeySnapshot (..),
  RefreshState (..),
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
    jwkSetsByKid :: Map Text Jose.JWKSet,
    -- ^ Pre-built singleton JWKSets per kid (avoid per-request allocation)
    cachedJwkSet :: Jose.JWKSet,
    -- ^ Pre-built JWKSet with all keys (for tokens without kid)
    allKeysArray :: Array Jose.JWK,
    -- ^ Pre-built array of all keys (for tokens without kid)
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
      jwkSetsByKid = Map.empty,
      cachedJwkSet = Jose.JWKSet [],
      allKeysArray = Array.empty,
      fetchedAt = 0,
      isStale = True
    }


-- | Refresh state for atomic cooldown and mutual exclusion.
data RefreshState = RefreshState
  { lastAttemptAt :: GhcInt.Int64,
    -- ^ Unix timestamp of last refresh attempt
    refreshInProgress :: Bool
    -- ^ True if a refresh is currently running
  }
  deriving (Show)


-- | Initial refresh state.
emptyRefreshState :: RefreshState
emptyRefreshState =
  RefreshState
    { lastAttemptAt = 0,
      refreshInProgress = False
    }


-- | JWKS Manager with background refresh.
data JwksManager = JwksManager
  { config :: AuthConfig,
    -- ^ Auth configuration (issuer, jwksUri, etc.)
    keySnapshot :: AtomicVar KeySnapshot,
    -- ^ Hot path: lock-free reads
    refreshState :: AtomicVar RefreshState,
    -- ^ Atomic refresh state (cooldown + mutual exclusion)
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
  refreshStateVar <- AtomicVar.containing emptyRefreshState
  refreshTaskVar <- AtomicVar.containing Nothing
  runningVar <- AtomicVar.containing True

  let manager =
        JwksManager
          { config = config,
            keySnapshot = snapshotVar,
            refreshState = refreshStateVar,
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
  Task.yield snapshot.allKeysArray


-- | Get the cached JWKSet for signature verification.
-- This is the preferred hot path - avoids per-request JWKSet construction.
getJwkSet :: JwksManager -> Task err Jose.JWKSet
getJwkSet manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  Task.yield snapshot.cachedJwkSet


-- | Get a JWKSet containing only the key with the given kid.
-- This is the optimal path when the token contains a kid claim.
-- Uses pre-cached singleton JWKSet to avoid per-request allocation.
-- Returns the full JWKSet if kid is not found (fallback to try all keys).
getJwkSetForKid :: Text -> JwksManager -> Task err Jose.JWKSet
getJwkSetForKid kid manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  -- Use cached singleton JWKSet if available (avoids allocation)
  case Map.get kid snapshot.jwkSetsByKid of
    Just jwkSet -> Task.yield jwkSet
    Nothing -> Task.yield snapshot.cachedJwkSet


-- | Get JWKSet for kid, triggering refresh if kid not found.
-- Returns (JWKSet, kidWasFound). If kid not found:
-- 1. Triggers background refresh (rate-limited)
-- 2. Returns full JWKSet as fallback
-- Uses pre-cached singleton JWKSet to avoid per-request allocation.
getJwkSetForKidWithRefresh :: Text -> JwksManager -> Task err (Jose.JWKSet, Bool)
getJwkSetForKidWithRefresh kid manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  -- Use cached singleton JWKSet if available (avoids allocation)
  case Map.get kid snapshot.jwkSetsByKid of
    Just jwkSet -> Task.yield (jwkSet, True)
    Nothing -> do
      -- Kid not found - trigger refresh (rate-limited) and fallback to all keys
      _ <- requestRefresh manager |> Task.asResult
      Task.yield (snapshot.cachedJwkSet, False)


-- | Check if keys are stale beyond acceptable threshold.
-- Returns True if keys should not be used (triggers 503 response).
checkStaleness :: JwksManager -> Task err Bool
checkStaleness manager = do
  now <- getCurrentSeconds
  snapshot <- AtomicVar.peek manager.keySnapshot
  let age = now - snapshot.fetchedAt
  let maxStale = manager.config.maxStaleSeconds
  -- Keys are stale if: explicitly marked stale AND beyond max stale time
  Task.yield (snapshot.isStale && age > maxStale)


-- | Request an immediate refresh (e.g., when a kid is not found).
-- This is rate-limited and uses atomic state to prevent thundering herd.
-- Only one refresh can run at a time across all callers.
requestRefresh :: JwksManager -> Task err Unit
requestRefresh manager = do
  now <- getCurrentSeconds
  let cooldownSeconds = manager.config.missingKidCooldownSeconds

  -- Atomically check and acquire refresh lock
  shouldRefresh <- AtomicVar.modifyWithResult manager.refreshState \state -> do
    let timeSinceLastAttempt = now - state.lastAttemptAt
    -- Only refresh if: not in progress AND cooldown has passed
    case state.refreshInProgress || timeSinceLastAttempt < cooldownSeconds of
      True -> (state, False) -- Skip: in progress or in cooldown
      False ->
        -- Acquire lock: mark in progress and update attempt time
        let newState = RefreshState {lastAttemptAt = now, refreshInProgress = True}
         in (newState, True)

  case shouldRefresh of
    False -> Task.yield () -- Another refresh in progress or cooldown active
    True -> do
      -- Perform refresh with lock held
      _ <- refreshKeys manager |> Task.asResult
      -- Release lock (keep lastAttemptAt for cooldown)
      AtomicVar.modify
        (\state -> state {refreshInProgress = False})
        manager.refreshState
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
        -- Perform refresh - mark stale on failure
        refreshResult <- refreshKeys manager |> Task.asResult
        case refreshResult of
          Ok _ -> Task.yield ()
          Err _ -> do
            -- Mark keys as stale on refresh failure
            markKeysStale manager
            Task.yield ()


-- | Refresh keys from the JWKS endpoint.
refreshKeys :: JwksManager -> Task DiscoveryError Unit
refreshKeys manager = do
  -- Fetch keys from JWKS URI
  keysResult <- Discovery.fetchJwks manager.config.jwksUri
  case keysResult of
    Err err -> Task.throw err
    Ok keys -> do
      -- Build new snapshot (with isStale = False)
      now <- getCurrentSeconds
      let newSnapshot = buildSnapshot keys now
      -- Atomically update
      AtomicVar.set newSnapshot manager.keySnapshot
      Task.yield ()


-- | Mark current keys as stale.
-- Called when refresh fails - keeps old keys but marks them stale.
markKeysStale :: JwksManager -> Task err Unit
markKeysStale manager = do
  snapshot <- AtomicVar.peek manager.keySnapshot
  let staleSnapshot = snapshot {isStale = True}
  AtomicVar.set staleSnapshot manager.keySnapshot
  Task.yield ()


-- | Build a key snapshot from an array of JWKs.
-- Pre-builds all derived structures for lock-free hot path access.
buildSnapshot :: Array Jose.JWK -> GhcInt.Int64 -> KeySnapshot
buildSnapshot keys now = do
  -- Build kid -> key map using safe filtering (no partial functions)
  let keyPairs = keys |> Array.map extractKidAndKey |> collectJusts
  let keysByKid = Map.fromArray keyPairs
  -- Pre-build singleton JWKSets per kid (avoid per-request allocation)
  let jwkSetPairs = keyPairs |> Array.map (\(kid, key) -> (kid, Jose.JWKSet [key]))
  let jwkSetsByKid = Map.fromArray jwkSetPairs
  -- Pre-build JWKSet with all keys for signature verification
  let cachedJwkSet = Jose.JWKSet (Array.toLinkedList keys)
  KeySnapshot
    { keysByKid = keysByKid,
      jwkSetsByKid = jwkSetsByKid,
      cachedJwkSet = cachedJwkSet,
      allKeysArray = keys,
      fetchedAt = now,
      isStale = False
    }


-- | Collect Just values from an array of Maybes (safe, no partial functions).
collectJusts :: forall value. Array (Maybe value) -> Array value
collectJusts maybes =
  Array.reduce
    ( \maybeVal acc ->
        case maybeVal of
          Maybe.Nothing -> acc
          Maybe.Just val -> Array.pushBack val acc
    )
    Array.empty
    maybes


-- | Extract (kid, JWK) tuple from a JWK.
extractKidAndKey :: Jose.JWK -> Maybe (Text, Jose.JWK)
extractKidAndKey jwk =
  case jwk Lens.^. Jose.jwkKid of
    Nothing -> Nothing
    Just kid -> Just (kid, jwk)


-- | Get current Unix timestamp in seconds.
getCurrentSeconds :: Task err GhcInt.Int64
getCurrentSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: GhcInt.Int64 = Prelude.floor (Prelude.realToFrac posixTime :: Prelude.Double)
  Task.yield seconds
