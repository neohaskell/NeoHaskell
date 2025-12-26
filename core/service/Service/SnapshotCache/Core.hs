module Service.SnapshotCache.Core (
  SnapshotCache (..),
  Error (..),
  SnapshotCacheConfig (..),
) where

import Basics
import Json qualified
import Maybe (Maybe)
import Service.Event.EntityName (EntityName)
import Service.Event.StreamId (StreamId)
import Service.SnapshotCache.Snapshot (Snapshot)
import Task (Task)
import Text (Text)


data Error
  = StorageError Text
  | SerializationError Text
  deriving (Eq, Show)


data SnapshotCache state = SnapshotCache
  { get :: EntityName -> StreamId -> Task Error (Maybe (Snapshot state)),
    set :: Snapshot state -> Task Error Unit,
    delete :: EntityName -> StreamId -> Task Error Unit,
    clear :: Task Error Unit
  }


class SnapshotCacheConfig config where
  createSnapshotCache ::
    (Json.FromJSON state, Json.ToJSON state) =>
    config ->
    Task Text (SnapshotCache state)
