{-| Pause owned query replay and HTTP reads for the cryptoerasure testbed.
Use 'ReplayGateStoreConfig' only with a private gate directory in a test run.
-}
module Testbed.Erasable.Owned.ReplayGateStore (ReplayGateStoreConfig (..)) where

import AsyncTask qualified
import Array qualified
import Core
import Environment qualified
import File qualified
import Path qualified
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..), QueryObjectStoreConfig (..))
import Service.QueryObjectStore.InMemory qualified as InMemory
import Task qualified


data ReplayGateStoreConfig = ReplayGateStoreConfig


instance QueryObjectStoreConfig ReplayGateStoreConfig where
  createQueryObjectStore _ queryName = do
    store <- InMemory.new |> Task.mapError toText
    configured <- Environment.getVariable "CRYPTOERASURE_REPLAY_GATE_DIR" |> Task.asResult
    case configured of
      Err _ -> Task.yield store
      Ok gateDirectory ->
        if queryName == "OwnedProtectedView"
          then Task.yield (withReplayGate gateDirectory store)
          else Task.yield store


withReplayGate :: Text -> QueryObjectStore query -> QueryObjectStore query
withReplayGate gateDirectory store =
  store
    { atomicUpdate = \queryId update -> do
        waitAtGate gateDirectory "replay-paused" "paused" "replay-release"
        store.atomicUpdate queryId update,
      getAll = do
        snapshot <- store.getAll
        waitAtGate gateDirectory "query-captured" (snapshot |> Array.length |> toText) "query-release"
        Task.yield snapshot
    }


waitAtGate :: Text -> Text -> Text -> Text -> Task Error Unit
waitAtGate gateDirectory markerName markerContent releaseName = do
  marker <- gatePath gateDirectory markerName
  release <- gatePath gateDirectory releaseName
  File.writeText marker markerContent |> Task.mapError (\_ -> StorageError "Replay test gate unavailable")
  waitForRelease release (1500 :: Int)


gatePath :: Text -> Text -> Task Error Path.Path
gatePath directory name =
  case Path.fromText [fmt|#{directory}/#{name}|] of
    Nothing -> Task.throw (StorageError "Invalid replay test gate path")
    Just path -> Task.yield path


waitForRelease :: Path.Path -> Int -> Task Error Unit
waitForRelease release attemptsLeft = do
  released <- File.exists release |> Task.mapError (\_ -> StorageError "Replay test gate unavailable")
  Task.unless released do
    if attemptsLeft <= 0
      then Task.throw (StorageError "Replay test gate timed out")
      else do
        AsyncTask.sleep 20
        waitForRelease release (attemptsLeft - 1)
