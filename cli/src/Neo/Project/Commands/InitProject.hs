module Neo.Project.Commands.InitProject (
  InitProject (..),
  getEntityId,
  decide,
) where

import Core
import Decider qualified
import Json qualified
import Neo.Project.Core
import Service.Auth (RequestContext (..))
import Service.Command.Core (TransportsOf)
import Service.CommandExecutor.TH (command)
import Service.Transport.Cli (CliTransport)


data InitProject = InitProject
  { name :: Text
  }
  deriving (Generic, Typeable, Show)


instance Json.FromJSON InitProject


instance Json.ToJSON InitProject


getEntityId :: InitProject -> Maybe Uuid
getEntityId _ = Nothing


decide :: InitProject -> Maybe ProjectEntity -> RequestContext -> Decision ProjectEvent
decide cmd _entity _ctx = do
  case cmd.name of
    "" -> Decider.reject "Project name cannot be empty"
    _ -> do
      entityId <- Decider.generateUuid
      Decider.acceptNew
        [ ProjectInitRequested
            (ProjectInitRequestedEvent
              { entityId = entityId,
                name = cmd.name,
                path = "./"
              })
        ]


type instance EntityOf InitProject = ProjectEntity


type instance TransportsOf InitProject = '[CliTransport]


command ''InitProject
