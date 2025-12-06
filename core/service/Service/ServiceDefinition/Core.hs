module Service.ServiceDefinition.Core (
  Service,
  ServiceDefinition (..),
  CommandDefinition(..),
  expose,
  command,
  deploy,
  extract,
  yield,
  (>>=),
  pure,
  (<*>),
  appendServiceDefinition,
  emptyServiceDefinition,
  fmap,
  (>>),
  return,
  join,
) where

import Basics hiding (fmap, join, pure, return, (<*>), (>>), (>>=))
import Task (Task)
import Task qualified
import Default (Default)
import Default qualified
import Function (unchanged)
import GHC.TypeLits (KnownSymbol)
import Record (Record)
import Record qualified
import Service.Adapter (ServiceAdapter(..))
import Service.Command (NameOf)
import Service.Command.Core (Command)
import Service.Definition.TypeLevel (Union)
import Service.Definition.Validation (ValidateProtocols)
import Service.Error (ServiceError(..))
import Service.Protocol (TransportProtocols)
import Service.Runtime (ServiceRuntime(..))


type Service commands reqProtos provProtos adapters = ServiceDefinition commands reqProtos provProtos adapters Unit


-- | ServiceDefinition represents a service with transport protocol tracking
-- It accumulates commands, required protocols, provided protocols, and adapters
-- The type parameters track this information at the type level
data ServiceDefinition
  (commands :: Record.Row Type)           -- Row type of all registered commands
  (requiredProtocols :: [Symbol])         -- Type-level list of protocols needed by commands
  (providedProtocols :: [Symbol])         -- Type-level list of protocols with adapters
  (adapters :: Record.Row Type)           -- Row type of adapter instances
  (value :: Type)                         -- Monadic value parameter
  = ServiceDefinition
  { commandNames :: Record commands,
    adapterRecord :: Record adapters,
    value :: value
  }
  deriving (Generic)

-- | Proxy to store the type of the command
data CommandDefinition (commandType :: Type) = CommandDefinition


-- | Register an adapter in the service definition
-- Declares "this service exposes its commands via this protocol"
expose ::
  forall adapter protocol.
  ( ServiceAdapter adapter,
    protocol ~ AdapterProtocol adapter,
    KnownSymbol protocol,
    IsLabel protocol (Record.Field protocol)
  ) =>
  adapter ->
  ServiceDefinition '[] '[] '[protocol] '[protocol Record.:= adapter] Unit
expose adapter = ServiceDefinition
  { commandNames = Record.empty,
    adapterRecord = Record.empty |> Record.insert (fromLabel @protocol) adapter,
    value = unit
  }

-- | Deploy a service definition into a runnable service runtime
-- This is the validation boundary where protocol checking occurs
deploy ::
  forall commands reqProtos provProtos adapters.
  ( ValidateProtocols reqProtos provProtos
  ) =>
  ServiceDefinition commands reqProtos provProtos adapters Unit ->
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

  Task.yield ServiceRuntime
    { execute = \commandName _bytes -> do
        -- Look up command in serviceDef.commandNames
        -- If not found, throw CommandNotFound
        -- Otherwise, execute through appropriate adapter
        Task.throw (CommandNotFound commandName),
      shutdown = Task.yield unit
    }

-- | Apply a function to the value inside a ServiceDefinition
fmap :: forall cmds reqProtos provProtos adapters a b. (a -> b) -> ServiceDefinition cmds reqProtos provProtos adapters a -> ServiceDefinition cmds reqProtos provProtos adapters b
fmap f m =
  ServiceDefinition
    { value = f m.value,
      commandNames = m.commandNames,
      adapterRecord = m.adapterRecord
    }


-- | Create a ServiceDefinition with just a value
pureValue :: forall a. a -> ServiceDefinition '[] '[] '[] '[] a
pureValue = yield


-- | Apply a function wrapped in a ServiceDefinition to a value wrapped in a ServiceDefinition
applyValue :: forall cmds reqProtos provProtos adapters a b. ServiceDefinition cmds reqProtos provProtos adapters (a -> b) -> ServiceDefinition cmds reqProtos provProtos adapters a -> ServiceDefinition cmds reqProtos provProtos adapters b
applyValue fnDef valDef =
  ServiceDefinition
    { value = fnDef.value valDef.value,  -- Apply the function to the value
      commandNames = valDef.commandNames,  -- Keep the metadata from the value (both should have same type)
      adapterRecord = valDef.adapterRecord  -- Keep the adapter metadata from the value
    }


-- | Sequentially compose two ServiceDefinitions, passing the value from the first as an argument to the second
bindValue ::
  forall cmds1 cmds2 req1 req2 prov1 prov2 adp1 adp2 a b.
  ServiceDefinition cmds1 req1 prov1 adp1 a -> (a -> ServiceDefinition cmds2 req2 prov2 adp2 b) -> ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2) b
bindValue m f = do
  let result = f m.value
  ServiceDefinition
    { value = result.value,
      commandNames = Record.merge m.commandNames result.commandNames,
      adapterRecord = Record.merge m.adapterRecord result.adapterRecord
    }


-- | Combine two ServiceDefinitions, keeping the second's value and merging their metadata
appendServiceDefinition ::
  forall cmds1 cmds2 req1 req2 prov1 prov2 adp1 adp2 a.
  ServiceDefinition cmds1 req1 prov1 adp1 a -> ServiceDefinition cmds2 req2 prov2 adp2 a -> ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2) a
appendServiceDefinition m1 m2 =
  ServiceDefinition
    { value = m2.value,
      commandNames = Record.merge m1.commandNames m2.commandNames,
      adapterRecord = Record.merge m1.adapterRecord m2.adapterRecord
    }


-- | Create an empty ServiceDefinition with a default value
emptyServiceDefinition :: forall a. (Default a) => ServiceDefinition '[] '[] '[] '[] a
emptyServiceDefinition =
  ServiceDefinition
    { value = Default.def,
      commandNames = Record.empty,
      adapterRecord = Record.empty
    }


pure :: a -> ServiceDefinition '[] '[] '[] '[] a
pure = pureValue


return :: a -> ServiceDefinition '[] '[] '[] '[] a
return = pure


(<*>) :: ServiceDefinition cmds reqProtos provProtos adapters (a -> b) -> ServiceDefinition cmds reqProtos provProtos adapters a -> ServiceDefinition cmds reqProtos provProtos adapters b
(<*>) = applyValue


(>>=) :: ServiceDefinition cmds1 req1 prov1 adp1 a -> (a -> ServiceDefinition cmds2 req2 prov2 adp2 b) -> ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2) b
(>>=) = bindValue


(>>) :: ServiceDefinition cmds1 req1 prov1 adp1 a -> ServiceDefinition cmds2 req2 prov2 adp2 b -> ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2) b
(>>) m1 m2 = ServiceDefinition
  { value = m2.value,
    commandNames = Record.merge m1.commandNames m2.commandNames,
    adapterRecord = Record.merge m1.adapterRecord m2.adapterRecord
  }


-- | Flatten a nested ServiceDefinition structure
join ::
  forall cmds1 cmds2 req1 req2 prov1 prov2 adp1 adp2 a.
  ServiceDefinition cmds1 req1 prov1 adp1 (ServiceDefinition cmds2 req2 prov2 adp2 a) -> ServiceDefinition (Record.Merge cmds1 cmds2) (Union req1 req2) (Union prov1 prov2) (Record.Merge adp1 adp2) a
join m = bindValue m unchanged


-- | Create a ServiceDefinition with just a value
yield :: forall a. a -> ServiceDefinition '[] '[] '[] '[] a
yield a =
  ServiceDefinition
    { value = a,
      commandNames = Record.empty,
      adapterRecord = Record.empty
    }


-- | Register a command type in the service definition
command ::
  forall (commandType :: Type) (commandName :: Symbol).
  ( Command commandType,
    commandName ~ NameOf commandType,
    IsLabel commandName (Record.Field commandName)
  ) =>
  ServiceDefinition '[commandName Record.:= CommandDefinition commandType] (TransportProtocols commandType) '[] '[] Unit
command = do
  let field = fromLabel @commandName
  let definition = CommandDefinition
  ServiceDefinition
    { value = unit,
      commandNames =
        Record.empty
          |> Record.insert field definition,
      adapterRecord = Record.empty
    }


-- | Extract the value from a ServiceDefinition
extract :: forall cmds reqProtos provProtos adapters a. ServiceDefinition cmds reqProtos provProtos adapters a -> a
extract m = m.value
