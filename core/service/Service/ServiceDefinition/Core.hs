module Service.ServiceDefinition.Core (
  Service,
  ServiceDefinition (..),
  CommandDefinition (..),
  new,
  useServer,
  command,
  deploy,
  merge,
) where

import Basics
import GHC.TypeLits (KnownSymbol)
import Record (Record)
import Record qualified
import Service.Adapter (ServiceAdapter (..))
import Service.Command (NameOf)
import Service.Command.Core (Command)
import Service.Definition.TypeLevel (Union)
import Service.Definition.Validation (ValidateServers)
import Service.Error (ServiceError (..))
import Service.Protocol (ApiFor, ServerApi (..))
import Service.Runtime (ServiceRuntime (..))
import Task (Task)
import Task qualified


type Service commands reqServers provServers adapters = ServiceDefinition commands reqServers provServers adapters


-- | ServiceDefinition represents a service with server API tracking
-- It accumulates commands, required servers, provided servers, and adapters
-- The type parameters track this information at the type level
data
  ServiceDefinition
    (commands :: Record.Row Type) -- Row type of all registered commands
    (requiredServers :: [Type]) -- Type-level list of server API types needed by commands
    (providedServers :: [Type]) -- Type-level list of server API types with adapters
    (adapters :: Record.Row Type) -- Row type of adapter instances
  = ServiceDefinition
  { commandNames :: Record commands,
    adapterRecord :: Record adapters
  }
  deriving (Generic)


-- | Proxy to store the type of the command
data CommandDefinition (commandType :: Type) = CommandDefinition


-- | Create a new empty service definition
new :: ServiceDefinition '[] '[] '[] '[]
new =
  ServiceDefinition
    { commandNames = Record.empty,
      adapterRecord = Record.empty
    }


-- | Register an adapter in the service definition
-- Declares "this service uses this server adapter to expose its commands"
useServer ::
  forall adapter serverApi serverName cmds reqServers provServers adapters.
  ( ServiceAdapter adapter,
    serverApi ~ AdapterApi adapter,
    ServerApi serverApi,
    serverName ~ ServerName serverApi,
    KnownSymbol serverName,
    IsLabel serverName (Record.Field serverName)
  ) =>
  adapter ->
  ServiceDefinition cmds reqServers provServers adapters ->
  ServiceDefinition
    (Record.Merge cmds '[])
    (Union reqServers '[])
    (Union provServers '[serverApi])
    (Record.Merge adapters '[serverName Record.:= adapter])
useServer adapter serviceDef = do
  let adapterDef :: ServiceDefinition '[] '[] '[serverApi] '[serverName Record.:= adapter]
      adapterDef =
        ServiceDefinition
          { commandNames = Record.empty,
            adapterRecord = Record.empty |> Record.insert (fromLabel @serverName) adapter
          }
  merge serviceDef adapterDef


-- | Deploy a service definition into a runnable service runtime
-- This is the validation boundary where server API checking occurs
deploy ::
  forall commands reqServers provServers adapters.
  (ValidateServers reqServers provServers) =>
  ServiceDefinition commands reqServers provServers adapters ->
  Task ServiceError ServiceRuntime
deploy _serviceDef = do
  -- TODO: Complete implementation
  -- 1. Initialize all adapters from serviceDef.adapterRecord
  -- 2. Build command routing table from serviceDef.commandNames
  -- 3. Create execute function that:
  --    a. Looks up command by name in commandNames
  --    b. Deserializes the Bytes payload to the command type
  --    c. Gets the command's ApiFor requirements
  --    d. Finds a matching adapter from the initialized adapters
  --    e. Instantiates CommandHandler with EventStore
  --    f. Executes the command's decide function
  --    g. Serializes the result back to Bytes

  Task.yield
    ServiceRuntime
      { execute = \commandName _bytes -> do
          -- Look up command in serviceDef.commandNames
          -- If not found, throw CommandNotFound
          -- Otherwise, execute through appropriate adapter
          Task.throw (CommandNotFound commandName),
        shutdown = Task.yield unit
      }


-- | Merge two ServiceDefinitions together, combining their commands and adapters
-- This is used for piping operations with |>
merge ::
  forall cmds1 cmds2 reqServers1 reqServers2 provServers1 provServers2 adp1 adp2.
  ServiceDefinition cmds1 reqServers1 provServers1 adp1 ->
  ServiceDefinition cmds2 reqServers2 provServers2 adp2 ->
  ServiceDefinition (Record.Merge cmds1 cmds2) (Union reqServers1 reqServers2) (Union provServers1 provServers2) (Record.Merge adp1 adp2)
merge m1 m2 =
  ServiceDefinition
    { commandNames = Record.merge m1.commandNames m2.commandNames,
      adapterRecord = Record.merge m1.adapterRecord m2.adapterRecord
    }


-- | Register a command type in the service definition
command ::
  forall (commandType :: Type) (commandName :: Symbol) cmds reqServers provServers adapters.
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition cmds reqServers provServers adapters ->
  ServiceDefinition
    (Record.Merge cmds '[commandName Record.:= CommandDefinition commandType])
    (Union reqServers (ApiFor commandType))
    (Union provServers '[])
    (Record.Merge adapters '[])
command serviceDef = do
  let field = fromLabel @commandName
  let definition = CommandDefinition
  let commandDef ::
        ServiceDefinition '[commandName Record.:= CommandDefinition commandType] (ApiFor commandType) '[] '[]
      commandDef =
        ServiceDefinition
          { commandNames =
              Record.empty
                |> Record.insert field definition,
            adapterRecord = Record.empty
          }
  merge serviceDef commandDef
