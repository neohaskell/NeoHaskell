module Service.QueryObjectStore.InMemory (
  InMemoryQueryObjectStoreConfig (..),
  new,
) where

import Array (Array)
import Basics
import ConcurrentVar (ConcurrentVar)
import Log qualified
import ConcurrentVar qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Service.QueryObjectStore.Core (Error (..), QueryObjectStore (..), QueryObjectStoreConfig (..))
import Task (Task)
import Task qualified
import ToText (toText)
import Uuid (Uuid)


-- | Configuration for creating an in-memory QueryObjectStore.
--
-- This is a simple configuration type with no options.
-- Use it for testing and development.
data InMemoryQueryObjectStoreConfig = InMemoryQueryObjectStoreConfig
  deriving (Eq, Show, Generic)


instance QueryObjectStoreConfig InMemoryQueryObjectStoreConfig where
  createQueryObjectStore _ = new |> Task.mapError toText


-- | Create a new in-memory QueryObjectStore.
--
-- The store uses a ConcurrentVar containing a Map for atomic updates.
-- This ensures that concurrent calls to 'atomicUpdate' are properly serialized.
new ::
  forall query.
  Task Error (QueryObjectStore query)
new = do
  store <- ConcurrentVar.containing Map.empty
  Task.yield
    QueryObjectStore
      { get = getImpl store,
        atomicUpdate = atomicUpdateImpl store,
        getAll = getAllImpl store
      }


getImpl ::
  forall query.
  ConcurrentVar (Map Uuid query) ->
  Uuid ->
  Task Error (Maybe query)
getImpl store queryId = do
  storeMap <- ConcurrentVar.peek store
  Task.yield (storeMap |> Map.get queryId)


atomicUpdateImpl ::
  forall query.
  ConcurrentVar (Map Uuid query) ->
  Uuid ->
  (Maybe query -> Maybe query) ->
  Task Error Unit
atomicUpdateImpl store queryId updateFn = do
  Log.debug [fmt|Query object store update for #{toText queryId}|]
    |> Task.ignoreError
  store
    |> ConcurrentVar.modify
      ( \storeMap -> do
          let currentValue = storeMap |> Map.get queryId
          let newValue = updateFn currentValue
          case newValue of
            Just query -> storeMap |> Map.set queryId query
            Nothing -> storeMap |> Map.remove queryId
      )


getAllImpl ::
  forall query.
  ConcurrentVar (Map Uuid query) ->
  Task Error (Array query)
getAllImpl store = do
  storeMap <- ConcurrentVar.peek store
  storeMap
    |> Map.values
    |> Task.yield
