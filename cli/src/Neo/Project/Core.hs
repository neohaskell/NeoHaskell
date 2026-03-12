module Neo.Project.Core (
  ProjectEntity (..),
  ProjectEvent (..),
  ProjectInitRequestedEvent (..),
  ProjectFileCreatedEvent (..),
  GitInitializedEvent (..),
  ProjectInitCompletedEvent (..),
  ProjectInitFailedEvent (..),
  initialState,
) where

import Array qualified
import Core
import Json qualified
import Service.Command.Core (Event (..))


data ProjectEntity = ProjectEntity
  { status :: Text,
    name :: Text,
    path :: Text,
    filesCreated :: Array Text
  }
  deriving (Generic)


instance Json.FromJSON ProjectEntity


instance Json.ToJSON ProjectEntity


instance Default ProjectEntity where
  def = initialState


initialState :: ProjectEntity
initialState =
  ProjectEntity
    { status = "",
      name = "",
      path = "",
      filesCreated = Array.empty
    }


type instance NameOf ProjectEntity = "ProjectEntity"


instance Entity ProjectEntity where
  initialStateImpl = initialState
  updateImpl = update


data ProjectInitRequestedEvent = ProjectInitRequestedEvent
  { entityId :: Uuid,
    name :: Text,
    path :: Text
  }
  deriving (Generic)


instance Json.FromJSON ProjectInitRequestedEvent


instance Json.ToJSON ProjectInitRequestedEvent


data ProjectFileCreatedEvent = ProjectFileCreatedEvent
  { entityId :: Uuid,
    filePath :: Text
  }
  deriving (Generic)


instance Json.FromJSON ProjectFileCreatedEvent


instance Json.ToJSON ProjectFileCreatedEvent


data GitInitializedEvent = GitInitializedEvent
  { entityId :: Uuid
  }
  deriving (Generic)


instance Json.FromJSON GitInitializedEvent


instance Json.ToJSON GitInitializedEvent


data ProjectInitCompletedEvent = ProjectInitCompletedEvent
  { entityId :: Uuid
  }
  deriving (Generic)


instance Json.FromJSON ProjectInitCompletedEvent


instance Json.ToJSON ProjectInitCompletedEvent


data ProjectInitFailedEvent = ProjectInitFailedEvent
  { entityId :: Uuid,
    reason :: Text
  }
  deriving (Generic)


instance Json.FromJSON ProjectInitFailedEvent


instance Json.ToJSON ProjectInitFailedEvent


data ProjectEvent
  = ProjectInitRequested ProjectInitRequestedEvent
  | ProjectFileCreated ProjectFileCreatedEvent
  | GitInitialized GitInitializedEvent
  | ProjectInitCompleted ProjectInitCompletedEvent
  | ProjectInitFailed ProjectInitFailedEvent
  deriving (Generic)


getEventEntityId :: ProjectEvent -> Uuid
getEventEntityId event = case event of
  ProjectInitRequested e -> e.entityId
  ProjectFileCreated e -> e.entityId
  GitInitialized e -> e.entityId
  ProjectInitCompleted e -> e.entityId
  ProjectInitFailed e -> e.entityId


type instance EventOf ProjectEntity = ProjectEvent


type instance EntityOf ProjectEvent = ProjectEntity


instance Event ProjectEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON ProjectEvent


instance Json.ToJSON ProjectEvent


update :: ProjectEvent -> ProjectEntity -> ProjectEntity
update event entity = case event of
  ProjectInitRequested e ->
    entity
      { status = "initializing",
        name = e.name,
        path = e.path
      }
  ProjectFileCreated e ->
    entity
      { filesCreated =
          entity.filesCreated
            |> Array.push e.filePath
      }
  GitInitialized _ ->
    entity
      { status = "git-initialized"
      }
  ProjectInitCompleted _ ->
    entity
      { status = "completed"
      }
  ProjectInitFailed _ ->
    entity
      { status = "failed"
      }
