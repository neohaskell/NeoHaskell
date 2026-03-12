module Neo.Build.Commands.StartBuild (
  StartBuild (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Neo.Build.Core
import Service.Auth (RequestContext (..))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Cli (CliTransport)


data StartBuild = StartBuild
  { projectPath :: Text
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON StartBuild


instance Json.ToJSON StartBuild


getEntityId :: StartBuild -> Maybe Uuid
getEntityId _ = Nothing


decide :: StartBuild -> Maybe BuildEntity -> RequestContext -> Decision BuildEvent
decide cmd _entity _ctx = do
  case cmd.projectPath of
    "" -> Decider.reject "Project path cannot be empty"
    _ -> do
      entityId <- Decider.generateUuid
      Decider.acceptNew
        [ BuildStarted
            (BuildStartedEvent
              { entityId = entityId,
                projectPath = cmd.projectPath
              })
        ]


type instance EntityOf StartBuild = BuildEntity


type instance TransportsOf StartBuild = '[CliTransport]


command ''StartBuild
