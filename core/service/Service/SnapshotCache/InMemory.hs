module Service.SnapshotCache.InMemory (
  InMemorySnapshotCacheConfig (..),
  new,
) where

import Basics
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import Log qualified
import Maybe (Maybe)
import Service.Event.EntityName (EntityName)
import Service.Event.StreamId (StreamId)
import Service.SnapshotCache.Core (Error (..), SnapshotCache (..), SnapshotCacheConfig (..))
import Service.SnapshotCache.Snapshot (Snapshot (..), SnapshotKey (..))
import Task (Task)
import Task qualified
import ToText (toText)


data InMemorySnapshotCacheConfig = InMemorySnapshotCacheConfig
  deriving (Eq, Show, Generic)


instance SnapshotCacheConfig InMemorySnapshotCacheConfig where
  createSnapshotCache _ = new |> Task.mapError toText


-- | The snapshot store uses a ConcurrentMap for high-throughput access.
-- Each snapshot is stored directly by its key, eliminating the nested
-- ConcurrentVar structure that caused contention at scale.
data SnapshotStore state = SnapshotStore
  { snapshots :: ConcurrentMap SnapshotKey (Snapshot state)
  }


new ::
  forall state.
  Task Error (SnapshotCache state)
new = do
  snapshots <- ConcurrentMap.new
  let store = SnapshotStore {snapshots}
  Task.yield
    SnapshotCache
      { get = getImpl store,
        set = setImpl store,
        delete = deleteImpl store,
        clear = clearImpl store
      }


getImpl ::
  forall state.
  SnapshotStore state ->
  EntityName ->
  StreamId ->
  Task Error (Maybe (Snapshot state))
getImpl store entityName streamId = do
  Log.debug [fmt|Snapshot cache lookup: #{toText entityName}/#{toText streamId}|] |> Task.ignoreError
  let key = SnapshotKey entityName streamId
  ConcurrentMap.get key store.snapshots


setImpl ::
  forall state.
  SnapshotStore state ->
  Snapshot state ->
  Task Error Unit
setImpl store snapshot = do
  let key = snapshot.key
  let keyEntityName = key.entityName
  let keyStreamId = key.streamId
  Log.debug [fmt|Snapshot cache write: #{toText keyEntityName}/#{toText keyStreamId}|] |> Task.ignoreError
  ConcurrentMap.set key snapshot store.snapshots


deleteImpl ::
  forall state.
  SnapshotStore state ->
  EntityName ->
  StreamId ->
  Task Error Unit
deleteImpl store entityName streamId = do
  let key = SnapshotKey entityName streamId
  ConcurrentMap.remove key store.snapshots


clearImpl ::
  forall state.
  SnapshotStore state ->
  Task Error Unit
clearImpl store = do
  ConcurrentMap.clear store.snapshots
