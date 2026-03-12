module Neo.Run.Core (
  AppEntity (..),
  AppEvent (..),
  RunRequestedEvent (..),
  AppStartedEvent (..),
  AppStoppedEvent (..),
  initialState,
  update,
) where

import Core
import Json qualified
import Service.Command.Core (Event (..))


data AppEntity = AppEntity
  { status :: Text,
    pid :: Maybe Int,
    projectPath :: Text
  }
  deriving (Generic)


instance Json.FromJSON AppEntity


instance Json.ToJSON AppEntity


instance Default AppEntity where
  def = initialState


initialState :: AppEntity
initialState =
  AppEntity
    { status = "",
      pid = Nothing,
      projectPath = ""
    }


type instance NameOf AppEntity = "AppEntity"


instance Entity AppEntity where
  initialStateImpl = initialState
  updateImpl = update


data RunRequestedEvent = RunRequestedEvent
  { entityId :: Uuid,
    projectPath :: Text
  }
  deriving (Generic)


instance Json.FromJSON RunRequestedEvent


instance Json.ToJSON RunRequestedEvent


data AppStartedEvent = AppStartedEvent
  { entityId :: Uuid,
    pid :: Int
  }
  deriving (Generic)


instance Json.FromJSON AppStartedEvent


instance Json.ToJSON AppStartedEvent


data AppStoppedEvent = AppStoppedEvent
  { entityId :: Uuid
  }
  deriving (Generic)


instance Json.FromJSON AppStoppedEvent


instance Json.ToJSON AppStoppedEvent


data AppEvent
  = RunRequested RunRequestedEvent
  | AppStarted AppStartedEvent
  | AppStopped AppStoppedEvent
  deriving (Generic)


getEventEntityId :: AppEvent -> Uuid
getEventEntityId event = case event of
  RunRequested e -> e.entityId
  AppStarted e -> e.entityId
  AppStopped e -> e.entityId


type instance EventOf AppEntity = AppEvent


type instance EntityOf AppEvent = AppEntity


instance Event AppEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON AppEvent


instance Json.ToJSON AppEvent


update :: AppEvent -> AppEntity -> AppEntity
update event entity = case event of
  RunRequested e ->
    entity
      { status = "requested",
        projectPath = e.projectPath
      }
  AppStarted e ->
    entity
      { status = "running",
        pid = Just e.pid
      }
  AppStopped _ ->
    entity
      { status = "stopped",
        pid = Nothing
      }
