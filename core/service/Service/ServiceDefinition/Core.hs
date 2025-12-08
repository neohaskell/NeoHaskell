module Service.ServiceDefinition.Core (
  Service,
  ServiceDefinition (..),
  CommandDefinition (..),
  new,
  expose,
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
import Service.Definition.Validation (ValidateProtocols)
import Service.Error (ServiceError (..))
import Service.Protocol (TransportProtocols)
import Service.Runtime (ServiceRuntime (..))
import Task (Task)
import Task qualified


type Service commands reqProtos provProtos adapters = ServiceDefinition commands reqProtos provProtos adapters


-- | ServiceDefinition represents a service with transport protocol tracking
-- It accumulates commands, required protocols, provided protocols, and adapters
-- The type parameters track this information at the type level
data
  ServiceDefinition
    (commands :: Record.Row Type) -- Row type of all registered commands
    (requiredProtocols :: [Symbol]) -- Type-level list of protocols needed by commands
    (providedProtocols :: [Symbol]) -- Type-level list of protocols with adapters
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
-- Declares "this service exposes its commands via this protocol"
expose ::
  forall adapter protocol cmds reqProtos provProtos adapters.
  ( ServiceAdapter adapter,
    protocol ~ AdapterProtocol adapter,
    KnownSymbol protocol,
    IsLabel protocol (Record.Field protocol)
  ) =>
  adapter ->
  ServiceDefinition cmds reqProtos provProtos adapters ->
  ServiceDefinition
    (Record.Merge cmds '[])
    (Union reqProtos '[])
    (Union provProtos '[protocol])
    (Record.Merge adapters '[protocol Record.:= adapter])
expose adapter serviceDef = do
  let adapterDef :: ServiceDefinition '[] '[] '[protocol] '[protocol Record.:= adapter]
      adapterDef =
        ServiceDefinition
          { commandNames = Record.empty,
            adapterRecord = Record.empty |> Record.insert (fromLabel @protocol) adapter
          }
  merge serviceDef adapterDef


-- | Deploy a service definition into a runnable service runtime
-- This is the validation boundary where protocol checking occurs
deploy ::
  forall commands reqProtos provProtos adapters.
  (ValidateProtocols reqProtos provProtos) =>
  ServiceDefinition commands reqProtos provProtos adapters ->
  Task ServiceError ServiceRuntime
deploy _serviceDef = do
  -- TODO: Complete implementation
  -- 1. Initialize all adapters from serviceDef.adapterRecord
  -- 2. Build command routing table from serviceDef.commandNames
  -- 3. Create execute function that:
  --    a. Looks up command by name in commandNames
  --    b. Deserializes the Bytes payload to the command type
  --    c. Gets the command's TransportProtocols
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
  forall cmds1 cmds2 req1 req2 prov1 prov2 adp1 adp2.
  ServiceDefinition cmds1 req1 prov1 adp1 ->
  ServiceDefinition cmds2 req2 prov2 adp2 ->
  ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2)
merge m1 m2 =
  ServiceDefinition
    { commandNames = Record.merge m1.commandNames m2.commandNames,
      adapterRecord = Record.merge m1.adapterRecord m2.adapterRecord
    }


-- | Register a command type in the service definition
command ::
  forall (commandType :: Type) (commandName :: Symbol) cmds reqProtos provProtos adapters.
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition cmds reqProtos provProtos adapters ->
  ServiceDefinition
    (Record.Merge cmds '[commandName Record.:= CommandDefinition commandType])
    (Union reqProtos (TransportProtocols commandType))
    (Union provProtos '[])
    (Record.Merge adapters '[])
command serviceDef = do
  let field = fromLabel @commandName
  let definition = CommandDefinition
  let commandDef ::
        ServiceDefinition '[commandName Record.:= CommandDefinition commandType] (TransportProtocols commandType) '[] '[]
      commandDef =
        ServiceDefinition
          { commandNames =
              Record.empty
                |> Record.insert field definition,
            adapterRecord = Record.empty
          }
  merge serviceDef commandDef
