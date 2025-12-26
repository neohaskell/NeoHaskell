module Service.SnapshotCache.InMemory (
  InMemorySnapshotCacheConfig (..),
  new,
) where

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
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


-- | Each snapshot key has its own ConcurrentVar, allowing concurrent access
-- to different entities without contention. The outer ConcurrentVar holds the
-- map of keys to their individual snapshot vars.
data SnapshotStore state = SnapshotStore
  { snapshotVars :: ConcurrentVar (Map SnapshotKey (ConcurrentVar (Snapshot state)))
  }


new ::
  forall state.
  Task Error (SnapshotCache state)
new = do
  snapshotVars <- ConcurrentVar.containing Map.empty
  let store = SnapshotStore {snapshotVars}
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
  varsMap <- ConcurrentVar.peek store.snapshotVars
  case Map.get key varsMap of
    Nothing -> Task.yield Nothing
    Just snapshotVar -> do
      snapshot <- ConcurrentVar.peek snapshotVar
      Task.yield (Just snapshot)


setImpl ::
  forall state.
  SnapshotStore state ->
  Snapshot state ->
  Task Error Unit
setImpl store snapshot = do
  let key = snapshot.key
  varsMap <- ConcurrentVar.peek store.snapshotVars
  case Map.get key varsMap of
    Just snapshotVar -> do
      -- Key exists, update the existing ConcurrentVar atomically using modify
      ConcurrentVar.modify (\_ -> snapshot) snapshotVar
    Nothing -> do
      -- Key doesn't exist, create a new ConcurrentVar for this key
      -- Use ConcurrentVar.modify to atomically check-and-insert
      newVar <- ConcurrentVar.containing snapshot
      ConcurrentVar.modify
        ( \currentMap ->
            case Map.get key currentMap of
              Just _ ->
                -- Another thread created it first, we'll update that one instead
                -- (the newVar we created will be garbage collected)
                currentMap
              Nothing ->
                Map.set key newVar currentMap
        )
        store.snapshotVars
      -- If another thread won the race, update the existing var using modify
      updatedMap <- ConcurrentVar.peek store.snapshotVars
      case Map.get key updatedMap of
        Just existingVar -> ConcurrentVar.modify (\_ -> snapshot) existingVar
        Nothing -> pass -- Should not happen, but safe to ignore


deleteImpl ::
  forall state.
  SnapshotStore state ->
  EntityName ->
  StreamId ->
  Task Error Unit
deleteImpl store entityName streamId = do
  let key = SnapshotKey entityName streamId
  ConcurrentVar.modify
    (\varsMap -> Map.remove key varsMap)
    store.snapshotVars


clearImpl ::
  forall state.
  SnapshotStore state ->
  Task Error Unit
clearImpl store = do
  ConcurrentVar.modify
    (\_ -> Map.empty)
    store.snapshotVars
