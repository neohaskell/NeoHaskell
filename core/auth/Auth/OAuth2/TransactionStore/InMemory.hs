-- | In-memory TransactionStore implementation.
--
-- WARNING: This implementation is for DEVELOPMENT ONLY.
--
-- * Data is lost on process restart
-- * NOT suitable for multi-instance deployments (breaks one-time use guarantee)
-- * Not encrypted at rest
--
-- For production with multiple instances, use Redis (GETDEL for atomic consume).
--
-- = TTL Cleanup
--
-- This store includes a background reaper that runs every 60 seconds
-- to clean up expired transactions. This prevents unbounded memory growth.
--
-- = Usage
--
-- @
-- store <- TransactionStore.InMemory.new
--
-- let key = TransactionKey.fromText stateToken
-- let tx = Transaction { verifier = verifier, userId = userId, expiresAt = now + 300 }
-- store.put key tx
--
-- -- Later, in callback:
-- let key = TransactionKey.fromText stateToken
-- maybeTx <- store.consume key  -- Atomically get + delete
-- @
module Auth.OAuth2.TransactionStore.InMemory (
  new,
) where

import Array qualified
import Auth.OAuth2.TransactionStore (Transaction (..), TransactionKey, TransactionStore (..))
import Auth.OAuth2.TransactionStore.TransactionKey qualified as TransactionKey
import AsyncTask qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Log qualified
import Data.Time.Clock.POSIX qualified as GhcPosix
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Prelude qualified as GhcPrelude
import Task (Task)
import Task qualified
import Text (Text)


-- | Create a new in-memory TransactionStore.
--
-- WARNING: Development only. See module documentation.
--
-- The returned store guarantees atomic `consume` semantics within
-- a single process. Multiple instances will break the one-time use
-- guarantee - use Redis for production multi-instance deployments.
--
-- Includes a background reaper that cleans up expired transactions every 60 seconds.
--
-- @
-- store <- TransactionStore.InMemory.new
-- @
new :: Task Text TransactionStore
new = do
  storage <- ConcurrentVar.containing Map.empty
  -- Start background reaper for TTL cleanup (runs every 60 seconds)
  _ <- startReaper storage
  Task.yield
    TransactionStore
      { get = getImpl storage
      , put = putImpl storage
      , consume = consumeImpl storage
      , delete = deleteImpl storage
      }


-- | Internal storage type.
-- Uses Text representation of TransactionKey for Map storage.
type Storage = ConcurrentVar (Map Text Transaction)


getImpl :: Storage -> TransactionKey -> Task Text (Maybe Transaction)
getImpl storage key = do
  let keyText = TransactionKey.toText key
  store <- storage |> ConcurrentVar.peek
  store |> Map.get keyText |> Task.yield


putImpl :: Storage -> TransactionKey -> Transaction -> Task Text Unit
putImpl storage key tx = do
  let keyText = TransactionKey.toText key
  storage |> ConcurrentVar.modify (\store -> store |> Map.set keyText tx)


-- | Atomically consume a transaction (get + delete in one operation).
--
-- This is the critical operation for one-time use / replay prevention.
-- Uses ConcurrentVar.modifyReturning to ensure atomicity:
-- - Reads current value
-- - Deletes entry
-- - Returns the value that was read
--
-- Concurrent calls with the same key: exactly one gets the value,
-- all others get Nothing.
consumeImpl :: Storage -> TransactionKey -> Task Text (Maybe Transaction)
consumeImpl storage key = do
  let keyText = TransactionKey.toText key
  storage
    |> ConcurrentVar.modifyReturning
      ( \store -> do
          let maybeValue = store |> Map.get keyText
          let newStore = store |> Map.remove keyText
          Task.yield (newStore, maybeValue)
      )


deleteImpl :: Storage -> TransactionKey -> Task Text Unit
deleteImpl storage key = do
  let keyText = TransactionKey.toText key
  storage |> ConcurrentVar.modify (\store -> store |> Map.remove keyText)


-- | Reaper interval in milliseconds (60 seconds)
reaperIntervalMs :: Int
reaperIntervalMs = 60000


-- | Start background reaper that cleans up expired transactions.
--
-- Runs every 60 seconds, removing any transactions where expiresAt < now.
-- This prevents unbounded memory growth from abandoned OAuth flows.
startReaper :: Storage -> Task Text Unit
startReaper storage = do
  let reaperLoop :: Task Text Unit
      reaperLoop = do
        -- Sleep first, then clean (transactions start with 5 min TTL)
        AsyncTask.sleep reaperIntervalMs
          |> Task.mapError (\_ -> "reaper sleep error" :: Text)
        -- Get current time
        nowSeconds <- getCurrentTimeSeconds
        -- Remove all expired transactions atomically
        storage
          |> ConcurrentVar.modify
            ( \store ->
                store
                  |> Map.entries
                  |> Array.reduce
                      ( \(key, tx) acc ->
                          -- Use >= to keep tokens at exactly their expiry time
                          -- This matches StateToken validation (currentTime <= expiresAt)
                          case tx.expiresAt >= nowSeconds of
                            True -> acc |> Map.set key tx
                            False -> acc
                      )
                      Map.empty
            )
        -- Loop forever
        reaperLoop
  -- Run reaper in background with error logging
  let reaperWithErrorLogging :: Task Text Unit
      reaperWithErrorLogging = do
        result <- reaperLoop |> Task.asResult
        case result of
          Err err -> do
            -- Log error and restart reaper
            Log.withScope [("component", "OAuth2")] do
              Log.warn [fmt|Reaper error: #{err}, restarting...|]
                |> Task.ignoreError
            reaperWithErrorLogging
          Ok _ -> Task.yield unit
  _ <- AsyncTask.run reaperWithErrorLogging
    |> Task.mapError (\_ -> "reaper start error" :: Text)
  Task.yield unit


-- | Get current time as Unix seconds.
getCurrentTimeSeconds :: forall error. Task error Int
getCurrentTimeSeconds = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let seconds :: Int = GhcPrelude.floor (GhcPrelude.realToFrac posixTime :: GhcPrelude.Double)
  Task.yield seconds
