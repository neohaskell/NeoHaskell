module Service.SnapshotCache.InMemory (
  InMemorySnapshotCacheConfig (..),
  new,
) where

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Lock (Lock)
import Lock qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
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


data SnapshotStore state = SnapshotStore
  { snapshots :: ConcurrentVar (Map SnapshotKey (Snapshot state)),
    lock :: Lock
  }


new ::
  forall state.
  Task Error (SnapshotCache state)
new = do
  lock <- Lock.new
  snapshots <- ConcurrentVar.containing Map.empty
  let store = SnapshotStore {snapshots, lock}
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
  let key = SnapshotKey entityName streamId
  snapshotMap <- ConcurrentVar.peek store.snapshots
  Task.yield (Map.get key snapshotMap)


setImpl ::
  forall state.
  SnapshotStore state ->
  Snapshot state ->
  Task Error Unit
setImpl store snapshot = do
  Lock.with store.lock do
    ConcurrentVar.modify
      (\snapshotMap -> Map.set snapshot.key snapshot snapshotMap)
      store.snapshots


deleteImpl ::
  forall state.
  SnapshotStore state ->
  EntityName ->
  StreamId ->
  Task Error Unit
deleteImpl store entityName streamId = do
  let key = SnapshotKey entityName streamId
  Lock.with store.lock do
    ConcurrentVar.modify
      (\snapshotMap -> Map.remove key snapshotMap)
      store.snapshots


clearImpl ::
  forall state.
  SnapshotStore state ->
  Task Error Unit
clearImpl store = do
  Lock.with store.lock do
    ConcurrentVar.modify
      (\_ -> Map.empty)
      store.snapshots
