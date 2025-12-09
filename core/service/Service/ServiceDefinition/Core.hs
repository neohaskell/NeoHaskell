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
  merge,
  __internal_runServiceMain,
) where

import Array (Array, empty)
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
    IsLabel serverName (Record.Field serverName)
  ) =>
  serverApi ->
  ServiceDefinition cmds reqServers provServers servers ->
  ServiceDefinition
    (Record.Merge cmds '[])
    (Union reqServers '[])
    (Union provServers '[serverApi])
    (Record.Merge servers '[serverName Record.:= serverApi])
useServer server serviceDef = do
  let serverDef :: ServiceDefinition '[] '[] '[serverApi] '[serverName Record.:= serverApi]
      serverDef =
        ServiceDefinition
          { commands = Record.empty,
            serverRecord = Record.empty |> Record.insert (fromLabel @serverName) server
          }
  merge serviceDef serverDef


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
  -- 2. Build command routing table from serviceDef.commandNames
  -- 3. Create execute function that:
  --    a. Looks up command by name in commandNames
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


-- | Merge two ServiceDefinitions together, combining their commands and servers
-- This is used for piping operations with |>
merge ::
  forall cmds1 cmds2 reqServers1 reqServers2 provServers1 provServers2 srv1 srv2.
  ServiceDefinition cmds1 reqServers1 provServers1 srv1 ->
  ServiceDefinition cmds2 reqServers2 provServers2 srv2 ->
  ServiceDefinition
    (Record.Merge cmds1 cmds2)
    (Union reqServers1 reqServers2)
    (Union provServers1 provServers2)
    (Record.Merge srv1 srv2)
merge m1 m2 =
  ServiceDefinition
    { commands = Record.merge m1.commandNames m2.commandNames,
      serverRecord = Record.merge m1.serverRecord m2.serverRecord
    }


-- | Register a command type in the service definition
command ::
  forall (commandType :: Type) (commandName :: Symbol) cmds reqServers provServers servers.
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition cmds reqServers provServers servers ->
  ServiceDefinition
    (Record.Merge cmds '[commandName Record.:= CommandDefinition commandType])
    (Union reqServers (ApiFor commandType))
    (Union provServers '[])
    (Record.Merge servers '[])
command serviceDef = do
  let field = fromLabel @commandName
  let definition =
        CommandDefinition
          { requiredServers = Array.empty
          }
  let commandDef ::
        ServiceDefinition '[commandName Record.:= CommandDefinition commandType] (ApiFor commandType) '[] '[]
      commandDef =
        ServiceDefinition
          { commands =
              Record.empty
                |> Record.insert field definition,
            serverRecord = Record.empty
          }
  merge serviceDef commandDef


__internal_runServiceMain :: ServiceDefinition commands _ _ _ -> GHC.IO Unit
__internal_runServiceMain serviceDefinition = Task.runOrPanic do
  serviceDefinition.commands
    -- this must be reduced using the  AllFields constraint in Record
    |> Task.forEach \command -> do
      Console.print (toText command.requiredServers)
  panic "__internal_runServiceMain reached end"