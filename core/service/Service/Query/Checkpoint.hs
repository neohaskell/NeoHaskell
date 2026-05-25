module Service.Query.Checkpoint (
  CheckpointStore (..),
  CheckpointState (..),
  new,
) where

import Basics
import Map (Map)
import Maybe (Maybe (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Task (Task)
import Task qualified
import Text (Text)


data CheckpointStore = CheckpointStore
  { getPositions :: Task Text (Map Text StreamPosition),
    setPosition :: Text -> StreamPosition -> Task Text Unit,
    getMinPosition :: Task Text (Maybe StreamPosition)
  }


data CheckpointState
  = CheckpointValid StreamPosition
  | CheckpointStale
  | CheckpointAbsent


new :: Text -> Task Text CheckpointStore
new _connectionString = do
  Task.yield
    CheckpointStore
      { getPositions = panic "CheckpointStore.getPositions not yet implemented",
        setPosition = panic "CheckpointStore.setPosition not yet implemented",
        getMinPosition = panic "CheckpointStore.getMinPosition not yet implemented"
      }
