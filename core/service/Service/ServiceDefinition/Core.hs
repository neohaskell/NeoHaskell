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
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect)) commandRow,
    transports :: Map Text TransportValue,
    eventStoreConfig :: eventStoreConfig
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
    transport ->
    Record.Proxy cmd ->
    EndpointHandler
  createHandler _ eventStore transport cmd reqBytes respondCallback = do
    fetcher <-
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


new :: Service '[] '[] '[] Unit
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty,
      transports = Map.empty,
      eventStoreConfig = unit
    }


useServer ::
  forall transport transportName commandTransportNames providedTransportNames cmds eventStoreConfig.
  ( Transport transport,
    transportName ~ NameOf transport,
    Record.KnownHash transportName,
    Record.KnownSymbol transportName
  ) =>
  transport ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig ->
  Service cmds commandTransportNames (transportName ': providedTransportNames) eventStoreConfig
useServer transport serviceDefinition = do
  let transportNameText = getSymbolText (Record.Proxy @transportName)
  let newTransports = serviceDefinition.transports |> Map.set transportNameText (TransportValue transport)
  serviceDefinition
    { transports = newTransports
    }


useEventStore ::
  forall eventStoreConfig cmds commandTransportNames providedTransportNames.
  (EventStoreConfig eventStoreConfig) =>
  eventStoreConfig ->
  Service cmds commandTransportNames providedTransportNames _ ->
  Service cmds commandTransportNames providedTransportNames eventStoreConfig
useEventStore config serviceDefinition = do
  serviceDefinition
    { eventStoreConfig = config
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
  Service originalCommands commandTransportNames providedTransportNames eventStoreConfig ->
  Service
    ( (commandName 'Record.:= CommandDefinition commandName commandTransport cmd transportName event entity entityName entityIdType)
        ': originalCommands
    )
    (transportName ': commandTransportNames)
    providedTransportNames
    eventStoreConfig
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
      eventStoreConfig = serviceDefinition.eventStoreConfig
    }


__internal_runServiceMain ::
  forall cmds commandTransportNames providedTransportNames eventStoreConfig event.
  ( EventStoreConfig eventStoreConfig,
    event ~ ServiceEventType cmds,
    Json.FromJSON event,
    Json.ToJSON event
  ) =>
  Service cmds commandTransportNames providedTransportNames eventStoreConfig -> GHC.IO Unit
__internal_runServiceMain s = do
  case Record.reflectAllFields s.inspectDict of
    Record.Reflected ->
      runService @cmds @eventStoreConfig @event s.eventStoreConfig s.commandDefinitions s.transports
        |> Task.runOrPanic


runService ::
  forall (cmds :: Record.Row Type) eventStoreConfig event.
  ( Record.AllFields cmds (CommandInspect),
    EventStoreConfig eventStoreConfig,
    event ~ ServiceEventType cmds,
    Json.FromJSON event,
    Json.ToJSON event
  ) =>
  eventStoreConfig ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text Unit
runService
  eventStoreConfig
  commandDefinitions
  transportsMap = do
    -- Create the EventStore ONCE at service startup
    eventStore <- EventStore.createEventStore @eventStoreConfig @event eventStoreConfig

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
              -- Use unsafeCoerce because ServiceEventType guarantees event ~ CmdEvent cmdDef
              -- but GHC can't prove this within the cmap context
              let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
              Record.K ((transportName x, transportVal), (commandName x, createHandler x typedEventStore (transport) (Record.Proxy @cmd)))

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
