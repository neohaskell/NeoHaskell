module Service.Query.Checkpoint.InMemory (
  new,
) where

import Array qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.Query.Checkpoint (CheckpointStore (..))
import Task (Task)
import Task qualified
import Text (Text)


-- | Create a new in-memory CheckpointStore.
--
-- Uses a ConcurrentVar containing a Map for thread-safe operations.
-- Suitable for testing and development.
new :: Task Text CheckpointStore
new = do
  store <- ConcurrentVar.containing Map.empty
  Task.yield
    CheckpointStore
      { getPositions = getPositionsImpl store
      , setPosition = setPositionImpl store
      , getMinPosition = getMinPositionImpl store
      }


getPositionsImpl :: ConcurrentVar (Map Text StreamPosition) -> Task Text (Map Text StreamPosition)
getPositionsImpl store = ConcurrentVar.peek store


setPositionImpl :: ConcurrentVar (Map Text StreamPosition) -> Text -> StreamPosition -> Task Text Unit
setPositionImpl store queryName position = do
  store |> ConcurrentVar.modify (\m -> m |> Map.set queryName position)


getMinPositionImpl :: ConcurrentVar (Map Text StreamPosition) -> Task Text (Maybe StreamPosition)
getMinPositionImpl store = do
  positions <- ConcurrentVar.peek store
  Task.yield (Map.values positions |> Array.minimum)
