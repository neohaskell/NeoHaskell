{- HLINT ignore "Use camelCase" -}
module Service.ServiceDefinition.Core (
  Service,
  ServiceDefinition (..),
  ServiceRuntime (..),
  CommandDefinition (..),
  new,
  useServer,
  command,
  makeRunnable,
  __internal_runServiceMain,
) where

import Array (Array)
import Array qualified
import Basics
import Console qualified
import GHC.IO qualified as GHC
import GHC.TypeLits (KnownSymbol)
import Record (Record)
import Record qualified
import Service.Command (NameOf)
import Service.Command.Core (Command)
import Service.Definition.TypeLevel (Union)
import Service.Definition.Validation (ValidateServers)
import Service.Error (ServiceError (..))
import Service.Protocol (ApiFor, ServerApi (..))
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)


type Service commands reqServers provServers servers = ServiceDefinition commands reqServers provServers servers


-- | ServiceDefinition represents a service with server API tracking
-- It accumulates commands, required servers, provided servers, and registered servers
-- The type parameters track this information at the type level
data
  ServiceDefinition
    (commands :: Record.Row Type) -- Row type of all registered commands
    (requiredServers :: [Type]) -- Type-level list of server API types needed by commands
    (providedServers :: [Type]) -- Type-level list of server API types registered
    (servers :: Record.Row Type) -- Row type of registered server instances
  = ServiceDefinition
  { commands :: Record commands,
    serverRecord :: Record servers
  }
  deriving (Generic)


-- | ServiceRuntime is the deployed, runnable form of a service
-- It provides command lookup, execution and shutdown capabilities
data ServiceRuntime = ServiceRuntime
  { shutdown :: Task ServiceError Unit
  }


-- | Proxy to store the type of the command
data CommandDefinition (commandType :: Type) = CommandDefinition
  { requiredServers :: Array Text
  }
  deriving (Show)


-- | Create a new empty service definition
new :: ServiceDefinition '[] '[] '[] '[]
new =
  ServiceDefinition
    { commands = Record.empty,
      serverRecord = Record.empty
    }


-- | Register a server in the service definition
-- Declares "this service uses this server to expose its commands"
-- FIXME: Should fail to compile if the server already has been provided, if not we could have
-- inconsistent behavior when picking up configs
useServer ::
  forall serverApi serverName cmds reqServers provServers servers.
  ( ServerApi serverApi,
    serverName ~ ServerName serverApi,
    KnownSymbol serverName,
    IsLabel serverName (Record.Field serverName),
    Record.SubRow
      (Record.Merge cmds '[])
      (Record.Merge cmds '[]),
    Record.SubRow
      (Record.Merge servers '[serverName Record.:= serverApi])
      (Record.Merge servers '[serverName Record.:= serverApi])
  ) =>
  serverApi ->
  ServiceDefinition cmds reqServers provServers servers ->
  ServiceDefinition
    cmds
    (Union reqServers '[])
    (Union provServers '[serverApi])
    (Record.Merge servers '[serverName Record.:= serverApi])
useServer server serviceDef = do
  let serverDef :: ServiceDefinition '[] '[] '[serverApi] '[serverName Record.:= serverApi]
      serverDef =
        ServiceDefinition
          { commands = Record.empty,
            serverRecord =
              Record.empty
                |> Record.insert (fromLabel @serverName) (Record.I server)
          }
  ServiceDefinition
    { commands = Record.project (Record.merge serviceDef.commands serverDef.commands),
      serverRecord = Record.project (Record.merge serviceDef.serverRecord serverDef.serverRecord)
    }


-- | Deploy a service definition into a runnable service runtime
-- This is the validation boundary where server API checking occurs
makeRunnable ::
  forall commands reqServers provServers servers.
  (ValidateServers reqServers provServers) =>
  ServiceDefinition commands reqServers provServers servers ->
  Task ServiceError ServiceRuntime
makeRunnable _serviceDef = do
  -- TODO: Complete implementation
  -- 1. Access server configs from serviceDef.serverRecord
  -- 2. Build command routing table from serviceDef.commands
  -- 3. Create execute function that:
  --    a. Looks up command by name in commands
  --    b. Deserializes the Bytes payload to the command type
  --    c. Gets the command's ApiFor requirements
  --    d. Finds a matching server from the registered servers
  --    e. Instantiates CommandHandler with EventStore
  --    f. Executes the command's decide function
  --    g. Serializes the result back to Bytes

  Task.yield
    ServiceRuntime
      { shutdown = Task.yield unit
      }


-- | Register a command type in the service definition
command ::
  forall
    (commandType :: Type)
    (commandName :: Symbol)
    cmds
    cmds2
    (reqServers :: [Type])
    (provServers :: [Type])
    (servers :: Record.Row Type).
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName),
    cmds2 ~ (Record.Merge cmds '[commandName Record.:= CommandDefinition commandType])
  ) =>
  ServiceDefinition cmds reqServers provServers servers ->
  ServiceDefinition
    cmds2
    (Union reqServers (ApiFor commandType))
    provServers
    servers
command serviceDef = do
  let field = fromLabel @(NameOf commandType)
  let definition =
        CommandDefinition
          { requiredServers = Array.empty
          }
  serviceDef
    { commands =
        serviceDef.commands
          |> Record.insert field (Record.I definition)
    }


__internal_runServiceMain ::
  (Record.AllFields commands Show) => ServiceDefinition commands _ _ _ -> GHC.IO Unit
__internal_runServiceMain serviceDefinition = Task.runOrPanic @Text do
  serviceDefinition.commands
    -- this must be reduced using the  AllFields constraint in Record
    |> reduceWithServer
    |> Task.forEach \(commandText :: Text) -> do
      Console.print commandText
  panic "__internal_runServiceMain reached end"


reduceWithServer ::
  (Record.AllFields commands Show) => Record commands -> Array Text
reduceWithServer commands = do
  commands
    |> showAllFields


showAllFields :: (Record.AllFields r Show) => Record.ContextRecord Record.I r -> Array Text
showAllFields rec =
  rec
    |> Record.cmap (Record.Proxy @Show) (\(Record.I x) -> Record.K (toText x))
    |> Record.collapse
    |> Array.fromLinkedList