module Neo.Run.Commands.RunProject (
  RunProject (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Neo.Run.Core
import Service.Auth (RequestContext (..))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Cli (CliTransport)


data RunProject = RunProject
  { projectPath :: Text
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON RunProject


instance Json.ToJSON RunProject


getEntityId :: RunProject -> Maybe Uuid
getEntityId _ = Nothing


decide :: RunProject -> Maybe AppEntity -> RequestContext -> Decision AppEvent
decide cmd _entity _ctx = do
  case cmd.projectPath of
    "" -> Decider.reject "Project path cannot be empty"
    _ -> do
      entityId <- Decider.generateUuid
      Decider.acceptNew
        [ RunRequested
            (RunRequestedEvent
              { entityId = entityId,
                projectPath = cmd.projectPath
              })
        ]


type instance EntityOf RunProject = AppEntity


type instance TransportsOf RunProject = '[CliTransport]


command ''RunProject
