{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.ServiceDefinition.Core (
  Service,
  new,
  useServer,
  command,
  __internal_runServiceMain,
  useEventStore,
  useSnapshotCache,
  toServiceRunner,
  ServiceRunner (..),

  -- * Re-exports for Application integration
  ServiceEventType,
  ServiceEntityType,
  CommandInspect,
) where

import Array (Array)
import Array qualified
import Basics
import Console qualified
import GHC.IO qualified as GHC
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Record (Record)
import Record qualified
import Service.Transport (Transport (..), EndpointHandler, Endpoints (..))
import Service.Command (EntityOf, EventOf)
import Service.Command.Core (TransportOf, Command (..), Entity (..), Event, NameOf)
import Service.CommandExecutor qualified as CommandExecutor
import Service.Response qualified as Response
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore, EventStoreConfig)
import Service.EventStore.Core qualified as EventStore
import Service.SnapshotCache.Core (SnapshotCache, SnapshotCacheConfig)
import Service.SnapshotCache.Core qualified as SnapshotCache
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
    (providedTransportNames :: [Symbol])
    (eventStoreConfig :: Type)
    (snapshotCacheConfig :: Type)
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect)) commandRow,
    transports :: Map Text TransportValue,
    eventStoreConfig :: eventStoreConfig,
    snapshotCacheConfig :: Maybe snapshotCacheConfig
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


new :: Service '[] '[] '[] Unit Unit
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty,
      transports = Map.empty,
      eventStoreConfig = unit,
      snapshotCacheConfig = Nothing
    }


useServer ::
  forall transport transportName commandTransportNames providedTransportNames cmds eventStoreConfig snapshotCacheConfig.
  ( Transport transport,
    transportName ~ NameOf transport,
    Record.KnownHash transportName,
    Record.KnownSymbol transportName
  ) =>
  transport ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig ->
  Service cmds commandTransportNames (transportName ': providedTransportNames) eventStoreConfig snapshotCacheConfig
useServer transport serviceDefinition = do
  let transportNameText = getSymbolText (Record.Proxy @transportName)
  let newTransports = serviceDefinition.transports |> Map.set transportNameText (TransportValue transport)
  serviceDefinition
    { transports = newTransports
    }


useEventStore ::
  forall eventStoreConfig cmds commandTransportNames providedTransportNames snapshotCacheConfig.
  (EventStoreConfig eventStoreConfig) =>
  eventStoreConfig ->
  Service cmds commandTransportNames providedTransportNames _ snapshotCacheConfig ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig
useEventStore config serviceDefinition = do
  serviceDefinition
    { eventStoreConfig = config
    }


useSnapshotCache ::
  forall snapshotCacheConfig cmds commandTransportNames providedTransportNames eventStoreConfig.
  (SnapshotCacheConfig snapshotCacheConfig) =>
  snapshotCacheConfig ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig _ ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig
useSnapshotCache config serviceDefinition = do
  serviceDefinition
    { snapshotCacheConfig = Just config
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
    providedTransportNames
    eventStoreConfig
    snapshotCacheConfig
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
  Service originalCommands commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig ->
  Service
    ( (commandName 'Record.:= CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType)
        ': originalCommands
    )
    (transportName ': commandTransportNames)
    providedTransportNames
    eventStoreConfig
    snapshotCacheConfig
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
      transports = serviceDefinition.transports,
      inspectDict = newInspectDict,
      eventStoreConfig = serviceDefinition.eventStoreConfig,
      snapshotCacheConfig = serviceDefinition.snapshotCacheConfig
    }


__internal_runServiceMain ::
  forall cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig event entity.
  ( EventStoreConfig eventStoreConfig,
    SnapshotCacheConfig snapshotCacheConfig,
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig -> GHC.IO Unit
__internal_runServiceMain s = do
  case Record.reflectAllFields s.inspectDict of
    Record.Reflected ->
      runService @cmds @eventStoreConfig @snapshotCacheConfig @event @entity s.eventStoreConfig s.snapshotCacheConfig s.commandDefinitions s.transports
        |> Task.runOrPanic


runService ::
  forall (cmds :: Record.Row Type) eventStoreConfig snapshotCacheConfig event entity.
  ( Record.AllFields cmds (CommandInspect),
    EventStoreConfig eventStoreConfig,
    SnapshotCacheConfig snapshotCacheConfig,
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  eventStoreConfig ->
  Maybe snapshotCacheConfig ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text Unit
runService
  eventStoreConfig
  maybeSnapshotCacheConfig
  commandDefinitions
  transportsMap = do
    -- Create the EventStore ONCE at service startup
    rawEventStore <- EventStore.createEventStore eventStoreConfig
    let eventStore = rawEventStore |> EventStore.castEventStore @event

    -- Create the SnapshotCache if configured, typed to the service's entity type
    maybeCache <- case maybeSnapshotCacheConfig of
      Just config -> do
        cache <- SnapshotCache.createSnapshotCache @_ @entity config
        Task.yield (Just cache)
      Nothing -> Task.yield Nothing

    let mapper ::
          forall cmdDef cmd.
          ( CommandInspect cmdDef,
            cmd ~ Cmd cmdDef
          ) =>
          Record.I (cmdDef) ->
          Record.K ((Text, TransportValue), (Text, EndpointHandler)) (cmdDef)
        mapper (Record.I x) = do
          case transportsMap |> Map.get (transportName x) of
            Nothing -> panic [fmt|The impossible happened, couldn't find transport config for #{commandName x}|]
            Just transportVal -> do
              let transport = getTransportValue transportVal
              -- Use unsafeCoerce because ServiceEventType/ServiceEntityType guarantee
              -- event ~ CmdEvent cmdDef and entity ~ CmdEntity cmdDef,
              -- but GHC can't prove this within the cmap context
              let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
              let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
              Record.K ((transportName x, transportVal), (commandName x, createHandler x typedEventStore typedCache (transport) (Record.Proxy @cmd)))

    let xs :: Array ((Text, TransportValue), (Text, EndpointHandler)) =
          commandDefinitions
            |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
            |> Record.collapse
            |> Array.fromLinkedList

    -- Group endpoints by transport name
    let endpointsReducer ((transportNameText, transportVal), (commandNameText, handler)) endpointsMap = do
          let transport = getTransportValue transportVal
          case endpointsMap |> Map.get transportNameText of
            Nothing -> do
              -- First command for this transport, create new Endpoints
              let newEndpoints =
                    Endpoints
                      { transport = transport,
                        commandEndpoints = Map.empty |> Map.set commandNameText handler
                      }
              endpointsMap |> Map.set transportNameText newEndpoints
            Just existingEndpoints -> do
              -- Add command to existing transport endpoints
              let updatedEndpoints =
                    existingEndpoints
                      { commandEndpoints = existingEndpoints.commandEndpoints |> Map.set commandNameText handler
                      }
              endpointsMap |> Map.set transportNameText updatedEndpoints

    let endpointsByTransport =
          xs |> Array.reduce endpointsReducer Map.empty

    -- Run each transport with its endpoints
    endpointsByTransport
      |> Map.entries
      |> Task.forEach \(transportNameText, transportEndpoints) -> do
        let commandCount = Map.length transportEndpoints.commandEndpoints
        Console.print [fmt|Starting transport: #{transportNameText} with #{commandCount} commands|]

        -- Assemble the transport using the Transport's assembleTransport function
        -- Since we need to call assembleTransport with the proper type, we need to
        -- extract it from the TransportValue
        case transportsMap |> Map.get transportNameText of
          Nothing -> panic [fmt|Transport #{transportNameText} not found in transports map|]
          Just transportVal -> do
            -- We need to use the existentially quantified transport type
            case transportVal of
              TransportValue (transport :: transport) -> do
                -- Cast transportEndpoints to match the specific transport type
                let typedEndpoints = GHC.unsafeCoerce transportEndpoints :: Endpoints transport
                let runnableTransport = assembleTransport typedEndpoints

                -- Run the assembled transport
                runTransport transport runnableTransport


-- | A function that runs a service given a shared EventStore.
data ServiceRunner = ServiceRunner
  { runWithEventStore :: EventStore Json.Value -> Task Text Unit
  }


-- | Convert a Service to a ServiceRunner that can be used with Application.
--
-- The ServiceRunner will use the provided EventStore instead of creating its own.
-- This allows multiple services to share the same EventStore within an Application.
toServiceRunner ::
  forall cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig event entity.
  ( SnapshotCacheConfig snapshotCacheConfig,
    Record.AllFields cmds (CommandInspect),
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  Service cmds commandTransportNames providedTransportNames eventStoreConfig snapshotCacheConfig ->
  ServiceRunner
toServiceRunner service =
  case Record.reflectAllFields service.inspectDict of
    Record.Reflected ->
      ServiceRunner
        { runWithEventStore = \rawEventStore ->
            runServiceWithEventStore
              @cmds
              @snapshotCacheConfig
              @event
              @entity
              rawEventStore
              service.snapshotCacheConfig
              service.commandDefinitions
              service.transports
        }


-- | Run a service with a provided EventStore (instead of creating one from config).
runServiceWithEventStore ::
  forall (cmds :: Record.Row Type) snapshotCacheConfig event entity.
  ( Record.AllFields cmds (CommandInspect),
    SnapshotCacheConfig snapshotCacheConfig,
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity
  ) =>
  EventStore Json.Value ->
  Maybe snapshotCacheConfig ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text Unit
runServiceWithEventStore
  rawEventStore
  maybeSnapshotCacheConfig
  commandDefinitions
  transportsMap = do
    -- Use the provided EventStore, cast to the service's event type
    let eventStore = rawEventStore |> EventStore.castEventStore @event

    -- Create the SnapshotCache if configured, typed to the service's entity type
    maybeCache <- case maybeSnapshotCacheConfig of
      Just config -> do
        cache <- SnapshotCache.createSnapshotCache @_ @entity config
        Task.yield (Just cache)
      Nothing -> Task.yield Nothing

    let mapper ::
          forall cmdDef cmd.
          ( CommandInspect cmdDef,
            cmd ~ Cmd cmdDef
          ) =>
          Record.I (cmdDef) ->
          Record.K ((Text, TransportValue), (Text, EndpointHandler)) (cmdDef)
        mapper (Record.I x) = do
          case transportsMap |> Map.get (transportName x) of
            Nothing -> panic [fmt|The impossible happened, couldn't find transport config for #{commandName x}|]
            Just transportVal -> do
              let transport = getTransportValue transportVal
              let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
              let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
              Record.K ((transportName x, transportVal), (commandName x, createHandler x typedEventStore typedCache (transport) (Record.Proxy @cmd)))

    let xs :: Array ((Text, TransportValue), (Text, EndpointHandler)) =
          commandDefinitions
            |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
            |> Record.collapse
            |> Array.fromLinkedList

    -- Group endpoints by transport name
    let endpointsReducer ((transportNameText, transportVal), (commandNameText, handler)) endpointsMap = do
          let transport = getTransportValue transportVal
          case endpointsMap |> Map.get transportNameText of
            Nothing -> do
              let newEndpoints =
                    Endpoints
                      { transport = transport,
                        commandEndpoints = Map.empty |> Map.set commandNameText handler
                      }
              endpointsMap |> Map.set transportNameText newEndpoints
            Just existingEndpoints -> do
              let updatedEndpoints =
                    existingEndpoints
                      { commandEndpoints = existingEndpoints.commandEndpoints |> Map.set commandNameText handler
                      }
              endpointsMap |> Map.set transportNameText updatedEndpoints

    let endpointsByTransport =
          xs |> Array.reduce endpointsReducer Map.empty

    -- Run each transport with its endpoints
    endpointsByTransport
      |> Map.entries
      |> Task.forEach \(transportNameText, transportEndpoints) -> do
        let commandCount = Map.length transportEndpoints.commandEndpoints
        Console.print [fmt|Starting transport: #{transportNameText} with #{commandCount} commands|]

        case transportsMap |> Map.get transportNameText of
          Nothing -> panic [fmt|Transport #{transportNameText} not found in transports map|]
          Just transportVal -> do
            case transportVal of
              TransportValue (transport :: transport) -> do
                let typedEndpoints = GHC.unsafeCoerce transportEndpoints :: Endpoints transport
                let runnableTransport = assembleTransport typedEndpoints
                runTransport transport runnableTransport
