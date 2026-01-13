{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.ServiceDefinition.Core (
  Service,
  new,
  command,
  toServiceRunner,
  ServiceRunner (..),
  TransportValue (..),

  -- * Re-exports for Application integration
  ServiceEventType,
  ServiceEntityType,
) where

import Array (Array)
import Array qualified
import Basics
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Record (Record)
import Record qualified
import Service.Transport (Transport (..), EndpointHandler)
import Service.Command (EntityOf, EventOf)
import Service.Command.Core (TransportOf, Command (..), Entity (..), Event, NameOf)
import Service.CommandExecutor qualified as CommandExecutor
import Service.Response qualified as Response
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Service.SnapshotCache.Core (SnapshotCache)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import Unsafe.Coerce qualified as GHC


-- | Extracts the common event type from all commands in a service.
-- All commands must use the same event type. If they don't, a compile-time
-- error is generated with a helpful message.
type family ServiceEventType (cmds :: Record.Row Type) :: Type where
  ServiceEventType '[] = Never
  ServiceEventType ((label 'Record.:= cmdDef) ': rest) =
    ValidateAndGetEventType (CmdEvent cmdDef) rest


-- | Extracts the common entity type from all commands in a service.
-- Since all commands share the same event type, they also share the same entity type.
type family ServiceEntityType (cmds :: Record.Row Type) :: Type where
  ServiceEntityType '[] = Never
  ServiceEntityType ((label 'Record.:= cmdDef) ': rest) = CmdEntity cmdDef


-- | Helper type family that validates all remaining commands have the same event type
-- as the first command, and returns that common event type.
type family ValidateAndGetEventType (expectedEvent :: Type) (cmds :: Record.Row Type) :: Type where
  -- Base case: no more commands to check, return the expected event type
  ValidateAndGetEventType expectedEvent '[] = expectedEvent
  -- Recursive case: check if this command's event matches, then continue
  ValidateAndGetEventType expectedEvent ((label 'Record.:= cmdDef) ': rest) =
    CheckEventMatch expectedEvent (CmdEvent cmdDef) label rest


-- | Helper type family that checks if two event types match.
-- If they match, continue validating the rest of the commands.
-- If they don't match, generate a helpful compile-time error.
type family CheckEventMatch (expectedEvent :: Type) (actualEvent :: Type) (cmdLabel :: GHC.Symbol) (rest :: Record.Row Type) :: Type where
  -- Events match: continue validating the rest
  CheckEventMatch event event label rest = ValidateAndGetEventType event rest
  -- Events don't match: generate a compile-time error
  CheckEventMatch expectedEvent actualEvent label rest =
    TypeError
      ( 'Text "Event type mismatch in service definition!"
          ':$$: 'Text ""
          ':$$: 'Text "All commands in a service must use the same event type."
          ':$$: 'Text ""
          ':$$: 'Text "Expected event type: "
          ':<>: 'ShowType expectedEvent
          ':$$: 'Text "But command '"
          ':<>: 'Text label
          ':<>: 'Text "' uses event type: "
          ':<>: 'ShowType actualEvent
          ':$$: 'Text ""
          ':$$: 'Text "To fix: Ensure all commands in this service share a common event type."
          ':$$: 'Text "This typically means all entities should use the same domain event sum type."
      )


data
  Service
    (commandRow :: Record.Row Type)
    (commandTransportNames :: [Symbol])
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect)) commandRow
  }


data TransportValue
  = forall transport.
    (Transport transport) =>
    TransportValue transport


getTransportValue ::
  TransportValue -> transport
getTransportValue (TransportValue transport) = GHC.unsafeCoerce transport


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data
  CommandDefinition
    (name :: Symbol)
    (transport :: Type)
    (cmd :: Type)
    (transportName :: Symbol)
    (event :: Type)
    (entity :: Type)
    (entityName :: Symbol)
    (entityIdType :: Type)
  = CommandDefinition
  { commandName :: Text,
    transportName :: Text
  }
  deriving (Show)


class CommandInspect definition where
  type Cmd definition
  type CmdEntity definition
  type CmdEvent definition
  type CmdTransport definition
  type TransportName definition :: Symbol


  commandName :: definition -> Text


  transportName :: definition -> Text


  createHandler ::
    definition ->
    EventStore (CmdEvent definition) ->
    Maybe (SnapshotCache (CmdEntity definition)) ->
    CmdTransport definition ->
    Record.Proxy (Cmd definition) ->
    EndpointHandler


instance
  ( Command cmd,
    Entity entity,
    Event event,
    event ~ EventOf entity,
    entity ~ EntityOf cmd,
    entity ~ EntityOf event,
    entityIdType ~ EntityIdType entity,
    StreamId.ToStreamId entityIdType,
    Eq entityIdType,
    Ord entityIdType,
    Show entityIdType,
    Json.FromJSON cmd,
    Transport transport,
    name ~ NameOf cmd,
    Record.KnownSymbol transportName,
    Record.KnownSymbol name,
    Record.KnownSymbol entityName,
    Json.FromJSON event,
    Json.ToJSON event,
    IsMultiTenant cmd ~ False,
    Record.KnownHash name
  ) =>
  CommandInspect (CommandDefinition name transport cmd transportName event entity entityName entityIdType)
  where
  type Cmd (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = cmd
  type CmdEvent (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = event
  type CmdEntity (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = entity
  type CmdTransport (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = transport
  type TransportName (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = transportName


  commandName ::
    (Record.KnownSymbol name) => CommandDefinition name transport cmd transportName event entity entityName entityIdType -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  transportName _ = getSymbolText (Record.Proxy @transportName)


  createHandler ::
    (CommandDefinition name transport cmd transportName event entity entityName entityIdType) ->
    EventStore event ->
    Maybe (SnapshotCache entity) ->
    transport ->
    Record.Proxy cmd ->
    EndpointHandler
  createHandler _ eventStore maybeCache transport cmd reqBytes respondCallback = do
    fetcher <- case maybeCache of
      Just cache ->
        EntityFetcher.newWithCache
          eventStore
          cache
          (initialStateImpl @entity)
          (updateImpl @entity)
          |> Task.mapError toText
      Nothing ->
        EntityFetcher.new
          eventStore
          (initialStateImpl @entity)
          (updateImpl @entity)
          |> Task.mapError toText
    let entityName = EntityName (getSymbolText (Record.Proxy @(entityName)))

    let handler (cmd :: cmd) = do
          result <- CommandExecutor.execute eventStore fetcher entityName cmd
          Task.yield (Response.fromExecutionResult result)

    buildHandler @transport transport cmd handler reqBytes respondCallback


type instance NameOf (CommandDefinition name transport cmd transportName event entity entityName entityIdType) = name


new :: Service '[] '[]
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty
    }




-- | Register a command type in the service definition
command ::
  forall
    (cmd :: Type)
    originalCommands
    commandName
    commandTransport
    (transportName :: Symbol)
    (commandTransportNames :: [Symbol])
    event
    entity
    entityName
    entityIdType.
  ( Command cmd,
    commandName ~ NameOf cmd,
    commandTransport ~ TransportOf cmd,
    transportName ~ NameOf commandTransport,
    entityName ~ NameOf entity,
    Record.KnownSymbol transportName,
    Record.KnownSymbol commandName,
    Record.KnownSymbol entityName,
    Record.KnownHash commandName,
    entity ~ EntityOf cmd,
    event ~ EventOf entity,
    Json.FromJSON event,
    Json.ToJSON event,
    CommandInspect (CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType)
  ) =>
  Service originalCommands commandTransportNames ->
  Service
    ( (commandName 'Record.:= CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType)
        ': originalCommands
    )
    (transportName ': commandTransportNames)
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName),
              transportName = getSymbolText (Record.Proxy @transportName)
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict ::
        Record.Dict (CommandInspect) (CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType) = Record.Dict
  let newInspectDict =
        serviceDefinition.inspectDict
          |> Record.insert cmdName inspectDict

  Service
    { commandDefinitions = cmds,
      inspectDict = newInspectDict
    }


-- | A service runner that provides command endpoints grouped by transport.
--
-- Services no longer run transports directly - instead, Application collects
-- all endpoints from all services and runs each transport once with combined endpoints.
data ServiceRunner = ServiceRunner
  { -- | Get command endpoints for this service, grouped by transport name.
    -- Returns a map from transport name to (command name -> handler).
    -- Application will merge these across all services and run transports.
    getEndpointsByTransport ::
      EventStore Json.Value ->
      Map Text TransportValue ->
      Task Text (Map Text (Map Text EndpointHandler))
  }


-- | Convert a Service to a ServiceRunner that can be used with Application.
--
-- The ServiceRunner will use the provided EventStore and transports instead of
-- creating its own. This allows multiple services to share the same EventStore
-- and transports within an Application.
toServiceRunner ::
  forall cmds commandTransportNames event entity.
  ( event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  Service cmds commandTransportNames ->
  ServiceRunner
toServiceRunner service =
  ServiceRunner
    { getEndpointsByTransport = \rawEventStore transportsMap ->
        case Record.reflectAllFields service.inspectDict of
          Record.Reflected ->
            buildEndpointsByTransport
              @cmds
              @event
              @entity
              rawEventStore
              service.commandDefinitions
              transportsMap
    }


-- | Build command endpoints for a service, grouped by transport name.
--
-- Returns a map from transport name to (command name -> handler).
-- Application will merge these across all services and run each transport once.
buildEndpointsByTransport ::
  forall (cmds :: Record.Row Type) event entity.
  ( Record.AllFields cmds (CommandInspect),
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  EventStore Json.Value ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text (Map Text (Map Text EndpointHandler))
buildEndpointsByTransport rawEventStore commandDefinitions transportsMap = do
  let eventStore = rawEventStore |> EventStore.castEventStore @event
  let maybeCache = Nothing :: Maybe (SnapshotCache entity)

  let mapper ::
        forall cmdDef cmd.
        ( CommandInspect cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I (cmdDef) ->
        Record.K (Text, Text, EndpointHandler) (cmdDef)
      mapper (Record.I x) = do
        case transportsMap |> Map.get (transportName x) of
          Nothing -> panic [fmt|The impossible happened, couldn't find transport config for #{commandName x}|]
          Just transportVal -> do
            let transport = getTransportValue transportVal
            let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
            let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
            Record.K (transportName x, commandName x, createHandler x typedEventStore typedCache transport (Record.Proxy @cmd))

  let endpoints :: Array (Text, Text, EndpointHandler) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
          |> Record.collapse
          |> Array.fromLinkedList

  -- Group by transport name
  let groupByTransport (transportNameText, cmdName, handler) acc =
        case acc |> Map.get transportNameText of
          Nothing ->
            acc |> Map.set transportNameText (Map.empty |> Map.set cmdName handler)
          Just existing ->
            acc |> Map.set transportNameText (existing |> Map.set cmdName handler)

  Task.yield (endpoints |> Array.reduce groupByTransport Map.empty)


