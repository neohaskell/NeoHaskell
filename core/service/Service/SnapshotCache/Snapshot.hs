module Service.SnapshotCache.Snapshot (
  Snapshot (..),
  SnapshotKey (..),
) where

import Basics
import Json (FromJSON, ToJSON)
import Service.Event.EntityName (EntityName)
import Service.Event.StreamId (StreamId)
import Service.Event.StreamPosition (StreamPosition)


data SnapshotKey = SnapshotKey
  { entityName :: EntityName,
    streamId :: StreamId
  }
  deriving (Eq, Show, Ord, Generic)


instance ToJSON SnapshotKey


instance FromJSON SnapshotKey


data Snapshot state = Snapshot
  { key :: SnapshotKey,
    state :: state,
    position :: StreamPosition
  }
  deriving (Eq, Show, Generic)


instance (ToJSON state) => ToJSON (Snapshot state)


instance (FromJSON state) => FromJSON (Snapshot state)
