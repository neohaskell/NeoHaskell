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
import Maybe (Maybe (..), withDefault)
import Record (Record)
import Record qualified
import Schema (FieldSchema (..), Schema)
import Schema qualified
import Service.Auth (RequestContext)
import Service.Transport (Transport (..), EndpointHandler, EndpointSchema (..))
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
    transportName :: Text,
    commandSchema :: Schema
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


  commandSchema :: definition -> Schema


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
    Schema.ToSchema cmd,
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


  commandSchema _ = Schema.toSchema @cmd


  createHandler ::
    (CommandDefinition name transport cmd transportName event entity entityName entityIdType) ->
    EventStore event ->
    Maybe (SnapshotCache entity) ->
    transport ->
    Record.Proxy cmd ->
    EndpointHandler
  createHandler _ eventStore maybeCache transport cmd = do
    -- Build the handler that will receive RequestContext at call time
    let handler :: RequestContext -> cmd -> Task Text Response.CommandResponse
        handler requestContext cmdInstance = do
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
          result <- CommandExecutor.execute eventStore fetcher entityName requestContext cmdInstance
          Task.yield (Response.fromExecutionResult result)

    buildHandler @transport transport cmd handler


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
    Schema.ToSchema cmd,
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
              transportName = getSymbolText (Record.Proxy @transportName),
              commandSchema = Schema.toSchema @cmd
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
  { -- | Get command endpoints and schemas for this service, grouped by transport name.
    -- Returns a tuple of (handlers, schemas) where both are maps from transport name to command maps.
    -- Application will merge these across all services and run transports.
    getEndpointsByTransport ::
      EventStore Json.Value ->
      Map Text TransportValue ->
      Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text Service.Transport.EndpointSchema))
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
    Json.ToJSON entity,
    GHC.KnownSymbol (NameOf entity)
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
-- Returns a tuple of (handlers, schemas) where both are maps from transport name to command maps.
-- Application will merge these across all services and run each transport once.
buildEndpointsByTransport ::
  forall (cmds :: Record.Row Type) event entity.
  ( Record.AllFields cmds (CommandInspect),
    event ~ ServiceEventType cmds,
    entity ~ ServiceEntityType cmds,
    Json.FromJSON event,
    Json.ToJSON event,
    Json.FromJSON entity,
    Json.ToJSON entity,
    GHC.KnownSymbol (NameOf entity)
  ) =>
  EventStore Json.Value ->
  Record.ContextRecord Record.I cmds ->
  Map Text TransportValue ->
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text Service.Transport.EndpointSchema))
buildEndpointsByTransport rawEventStore commandDefinitions transportsMap = do
  let eventStore = rawEventStore |> EventStore.castEventStore @event
  let maybeCache = Nothing :: Maybe (SnapshotCache entity)
  -- Entity name for OpenAPI tag grouping (shared by all commands in this service)
  let entityNameText = getSymbolText (Record.Proxy @(NameOf entity))

  let mapper ::
        forall cmdDef cmd.
        ( CommandInspect cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I (cmdDef) ->
        Record.K (Text, Text, EndpointHandler, Schema) (cmdDef)
      mapper (Record.I x) = do
        case transportsMap |> Map.get (transportName x) of
          Nothing -> panic [fmt|The impossible happened, couldn't find transport config for #{commandName x}|]
          Just transportVal -> do
            let transport = getTransportValue transportVal
            let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
            let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
            Record.K (transportName x, commandName x, createHandler x typedEventStore typedCache transport (Record.Proxy @cmd), commandSchema x)

  let endpoints :: Array (Text, Text, EndpointHandler, Schema) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
          |> Record.collapse
          |> Array.fromLinkedList

  -- Group by transport name, detecting duplicate command handlers
  let groupByTransport (transportNameText, cmdName, handler, schema) (handlersAcc, schemasAcc) =
        let endpointSchema = Service.Transport.EndpointSchema
              { requestSchema = Just schema
              , responseSchema = commandResponseSchema
              , description = ""
              , deprecated = False
              , entityName = Just entityNameText
              }
        in case handlersAcc |> Map.get transportNameText of
          Nothing -> do
            let newHandlers = handlersAcc |> Map.set transportNameText (Map.empty |> Map.set cmdName handler)
            let newSchemas = schemasAcc |> Map.set transportNameText (Map.empty |> Map.set cmdName endpointSchema)
            (newHandlers, newSchemas)
          Just existingHandlers ->
            case existingHandlers |> Map.get cmdName of
              Just _ ->
                -- Duplicate handler detected - this is a configuration error
                panic [fmt|Duplicate command handler registered for: #{cmdName}|]
              Nothing -> do
                let newHandlers = handlersAcc |> Map.set transportNameText (existingHandlers |> Map.set cmdName handler)
                let existingSchemas = schemasAcc |> Map.get transportNameText |> Maybe.withDefault Map.empty
                let newSchemas = schemasAcc |> Map.set transportNameText (existingSchemas |> Map.set cmdName endpointSchema)
                (newHandlers, newSchemas)

  Task.yield (endpoints |> Array.reduce groupByTransport (Map.empty, Map.empty))


-- | Schema for CommandResponse union type.
-- Represents the three possible command outcomes: Accepted, Rejected, Failed.
commandResponseSchema :: Schema
commandResponseSchema = Schema.SUnion (Array.fromLinkedList
  [ ("Accepted", Schema.SObject (Array.fromLinkedList
      [ FieldSchema
          { fieldName = "entityId"
          , fieldSchema = Schema.SText
          , fieldRequired = True
          , fieldDescription = "ID of the affected entity"
          }
      , FieldSchema
          { fieldName = "events"
          , fieldSchema = Schema.SArray (Schema.SObject Array.empty)
          , fieldRequired = True
          , fieldDescription = "Events generated by the command"
          }
      ]))
  , ("Rejected", Schema.SObject (Array.fromLinkedList
      [ FieldSchema
          { fieldName = "reason"
          , fieldSchema = Schema.SText
          , fieldRequired = True
          , fieldDescription = "Reason the command was rejected"
          }
      ]))
  , ("Failed", Schema.SObject (Array.fromLinkedList
      [ FieldSchema
          { fieldName = "error"
          , fieldSchema = Schema.SText
          , fieldRequired = True
          , fieldDescription = "Error message"
          }
      ]))
  ])


