module Neo.Build.Core (
  BuildEntity (..),
  BuildEvent (..),
  BuildStartedEvent (..),
  ModuleTranspiledEvent (..),
  BuildSucceededEvent (..),
  BuildFailedEvent (..),
  initialState,
  update,
) where

import Array qualified
import Core
import Json qualified
import Service.Command.Core (Event (..))


data BuildEntity = BuildEntity
  { status :: Text,
    projectPath :: Text,
    modulesTranspiled :: Array Text,
    errors :: Array Text,
    warnings :: Array Text
  }
  deriving (Generic)


instance Json.FromJSON BuildEntity


instance Json.ToJSON BuildEntity


instance Default BuildEntity where
  def = initialState


initialState :: BuildEntity
initialState =
  BuildEntity
    { status = "",
      projectPath = "",
      modulesTranspiled = Array.empty,
      errors = Array.empty,
      warnings = Array.empty
    }


type instance NameOf BuildEntity = "BuildEntity"


instance Entity BuildEntity where
  initialStateImpl = initialState
  updateImpl = update


data BuildStartedEvent = BuildStartedEvent
  { entityId :: Uuid,
    projectPath :: Text
  }
  deriving (Generic)


instance Json.FromJSON BuildStartedEvent


instance Json.ToJSON BuildStartedEvent


data ModuleTranspiledEvent = ModuleTranspiledEvent
  { entityId :: Uuid,
    moduleName :: Text
  }
  deriving (Generic)


instance Json.FromJSON ModuleTranspiledEvent


instance Json.ToJSON ModuleTranspiledEvent


data BuildSucceededEvent = BuildSucceededEvent
  { entityId :: Uuid,
    outputPath :: Text,
    warnings :: Array Text
  }
  deriving (Generic)


instance Json.FromJSON BuildSucceededEvent


instance Json.ToJSON BuildSucceededEvent


data BuildFailedEvent = BuildFailedEvent
  { entityId :: Uuid,
    errors :: Array Text
  }
  deriving (Generic)


instance Json.FromJSON BuildFailedEvent


instance Json.ToJSON BuildFailedEvent


data BuildEvent
  = BuildStarted BuildStartedEvent
  | ModuleTranspiled ModuleTranspiledEvent
  | BuildSucceeded BuildSucceededEvent
  | BuildFailed BuildFailedEvent
  deriving (Generic)


getEventEntityId :: BuildEvent -> Uuid
getEventEntityId event = case event of
  BuildStarted e -> e.entityId
  ModuleTranspiled e -> e.entityId
  BuildSucceeded e -> e.entityId
  BuildFailed e -> e.entityId


type instance EventOf BuildEntity = BuildEvent


type instance EntityOf BuildEvent = BuildEntity


instance Event BuildEvent where
  getEventEntityIdImpl = getEventEntityId


instance Json.FromJSON BuildEvent


instance Json.ToJSON BuildEvent


update :: BuildEvent -> BuildEntity -> BuildEntity
update event entity = case event of
  BuildStarted e ->
    entity
      { status = "building",
        projectPath = e.projectPath
      }
  ModuleTranspiled e ->
    entity
      { modulesTranspiled =
          entity.modulesTranspiled
            |> Array.push e.moduleName
      }
  BuildSucceeded e ->
    entity
      { status = "succeeded",
        warnings = e.warnings
      }
  BuildFailed e ->
    entity
      { status = "failed",
        errors = e.errors
      }
