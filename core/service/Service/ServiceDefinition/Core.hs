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
import Data.Kind (Constraint)
import GHC.TypeLits (ErrorMessage (..), TypeError)
import GHC.TypeLits qualified as GHC
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..), withDefault)
import Result (Result (..))
import Record (Record)
import Record qualified
import Schema (FieldSchema (..), Schema)
import Schema qualified
import Service.Auth (RequestContext)
import Service.Transport.Internal (InternalTransport)
import Service.Transport (Transport (..), EndpointHandler, EndpointSchema (..))
import Service.Command (EntityOf, EventOf)
import Service.Command.Core (TransportsOf, NamesOf, AppendSymbols, AllKnownSymbols (..), Command (..), Entity (..), Event, NameOf)
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


type family PublicTransports (transports :: [Type]) :: [Type] where
  PublicTransports '[] = '[]
  PublicTransports (InternalTransport ': rest) = PublicTransports rest
  PublicTransports (transport ': rest) = transport ': PublicTransports rest


type family IncludesInternalTransport (transports :: [Type]) :: Bool where
  IncludesInternalTransport '[] = 'False
  IncludesInternalTransport (InternalTransport ': rest) = 'True
  IncludesInternalTransport (transport ': rest) = IncludesInternalTransport rest


type family TypeExists (target :: Type) (types :: [Type]) :: Bool where
  TypeExists target '[] = 'False
  TypeExists target (target ': rest) = 'True
  TypeExists target (value ': rest) = TypeExists target rest


type family HasDuplicatePublicTransports (transports :: [Type]) :: Bool where
  HasDuplicatePublicTransports '[] = 'False
  HasDuplicatePublicTransports (transport ': rest) =
    HasDuplicatePublicTransports' (TypeExists transport rest) rest


type family HasDuplicatePublicTransports' (hasCurrentDuplicate :: Bool) (remaining :: [Type]) :: Bool where
  HasDuplicatePublicTransports' 'True remaining = 'True
  HasDuplicatePublicTransports' 'False remaining = HasDuplicatePublicTransports remaining


type family AssertNoDuplicatePublicTransports (publicTransports :: [Type]) :: Constraint where
  AssertNoDuplicatePublicTransports publicTransports = AssertNoDuplicatePublicTransports' (HasDuplicatePublicTransports publicTransports)


type family AssertNoDuplicatePublicTransports' (hasDuplicates :: Bool) :: Constraint where
  AssertNoDuplicatePublicTransports' 'False = ()
  AssertNoDuplicatePublicTransports' 'True =
    TypeError
      ( 'Text "Invalid command transport declaration"
          ':$$: 'Text "Public transport list contains repeated transport entries."
          ':$$: 'Text "Each public transport must appear at most once in TransportsOf."
      )


type family AssertNoMixedTransports (transports :: [Type]) :: Constraint where
  AssertNoMixedTransports transports = AssertNoMixedTransports' (IncludesInternalTransport transports) (PublicTransports transports)


type family AssertNoMixedTransports' (hasInternal :: Bool) (publicTransports :: [Type]) :: Constraint where
  AssertNoMixedTransports' 'False publicTransports = AssertNoDuplicatePublicTransports publicTransports
  AssertNoMixedTransports' 'True '[] = ()
  AssertNoMixedTransports' 'True (transport ': rest) =
    TypeError
      ( 'Text "Invalid command transport declaration"
          ':$$: 'Text "A command cannot declare InternalTransport together with public transports."
          ':$$: 'Text "If you need both public and internal behavior, define two separate commands."
      )


class KnownBool (value :: Bool) where
  boolValue :: Bool


instance KnownBool 'False where
  boolValue = False


instance KnownBool 'True where
  boolValue = True


data
  CommandDefinition
    (name :: Symbol)
    (transports :: [Type])
    (cmd :: Type)
    (transportNames :: [Symbol])
    (event :: Type)
    (entity :: Type)
    (entityName :: Symbol)
    (entityIdType :: Type)
  = CommandDefinition
  { commandName :: Text,
    transportNames :: Array Text,
    commandSchema :: Schema
  }
  deriving (Show)


class CommandInspect definition where
  type Cmd definition
  type CmdEntity definition
  type CmdEvent definition


  commandName :: definition -> Text


  commandSchema :: definition -> Schema


  createHandlers ::
    definition ->
    EventStore (CmdEvent definition) ->
    Maybe (SnapshotCache (CmdEntity definition)) ->
    Map Text TransportValue ->
    Record.Proxy (Cmd definition) ->
    Array (Text, EndpointHandler)


  createDispatchHandler ::
    definition ->
    EventStore (CmdEvent definition) ->
    Maybe (SnapshotCache (CmdEntity definition)) ->
    Record.Proxy (Cmd definition) ->
    EndpointHandler


  includeInDispatchMap ::
    definition ->
    Bool


buildDispatchHandler ::
  forall command name.
  ( Command command,
    Json.FromJSON command,
    name ~ NameOf command,
    Record.KnownSymbol name
  ) =>
  Record.Proxy command ->
  (RequestContext -> command -> Task Text Response.CommandResponse) ->
  EndpointHandler
buildDispatchHandler _ handler requestContext body respond = do
  let commandName =
        GHC.symbolVal (Record.Proxy @name)
          |> Text.fromLinkedList
  let commandValue = body |> Json.decodeBytes @command
  case commandValue of
    Ok cmd -> do
      response <- handler requestContext cmd
      let responseJson = Json.encodeText response |> Text.toBytes
      respond (response, responseJson)
    Err _err -> do
      let errorResponse =
            Response.Failed
              { error = [fmt|Invalid input for command #{commandName}|]
              }
      let responseJson = Json.encodeText errorResponse |> Text.toBytes
      respond (errorResponse, responseJson)


buildCommandResponseHandler ::
  forall cmd entity event entityName name.
  ( Command cmd,
    name ~ NameOf cmd,
    Record.KnownSymbol name,
    Record.KnownHash name,
    Entity entity,
    Event event,
    event ~ EventOf entity,
    entity ~ EntityOf cmd,
    entity ~ EntityOf event,
    IsMultiTenant cmd ~ False,
    StreamId.ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Ord (EntityIdType entity),
    Show (EntityIdType entity),
    Show event,
    Record.KnownSymbol entityName,
    Json.FromJSON event,
    Json.ToJSON event
  ) =>
  EventStore event ->
  Maybe (SnapshotCache entity) ->
  RequestContext ->
  cmd ->
  Task Text Response.CommandResponse
buildCommandResponseHandler eventStore maybeCache requestContext cmdInstance = do
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
  let entityName = EntityName (getSymbolText (Record.Proxy @entityName))
  result <- CommandExecutor.execute eventStore fetcher entityName requestContext cmdInstance
  Task.yield (Response.fromExecutionResult result)


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
    Show event,
     Json.FromJSON cmd,
     Schema.ToSchema cmd,
     BuildHandlersForTransports (PublicTransports transports),
     KnownBool (IncludesInternalTransport transports),
     AssertNoMixedTransports transports,
     AllKnownSymbols transportNames,
     name ~ NameOf cmd,
     Record.KnownSymbol name,
    Record.KnownSymbol entityName,
    Json.FromJSON event,
    Json.ToJSON event,
    IsMultiTenant cmd ~ False,
    Record.KnownHash name
  ) =>
  CommandInspect (CommandDefinition name transports cmd transportNames event entity entityName entityIdType)
  where
  type Cmd (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) = cmd
  type CmdEvent (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) = event
  type CmdEntity (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) = entity


  commandName ::
    (Record.KnownSymbol name) => CommandDefinition name transports cmd transportNames event entity entityName entityIdType -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  commandSchema _ = Schema.toSchema @cmd


  createHandlers ::
    (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) ->
    EventStore event ->
    Maybe (SnapshotCache entity) ->
    Map Text TransportValue ->
    Record.Proxy cmd ->
    Array (Text, EndpointHandler)
  createHandlers _ eventStore maybeCache transportsMap cmd =
    buildHandlersForAll @(PublicTransports transports) transportsMap cmd
      (buildCommandResponseHandler @cmd @entity @event @entityName eventStore maybeCache)


  createDispatchHandler _ eventStore maybeCache cmd =
    buildDispatchHandler @cmd cmd
      (buildCommandResponseHandler @cmd @entity @event @entityName eventStore maybeCache)


  includeInDispatchMap _ = boolValue @(IncludesInternalTransport transports)


-- | Iterates over a type-level list of transports and builds handlers for each.
class BuildHandlersForTransports (transports :: [Type]) where
  buildHandlersForAll ::
    forall command name.
    ( Command command,
      Json.FromJSON command,
      name ~ NameOf command,
      Record.KnownSymbol name
    ) =>
    Map Text TransportValue ->
    Record.Proxy command ->
    (RequestContext -> command -> Task Text Response.CommandResponse) ->
    Array (Text, EndpointHandler)


instance BuildHandlersForTransports '[] where
  buildHandlersForAll _ _ _ = Array.empty


instance
  ( Transport transport,
    Record.KnownSymbol (NameOf transport),
    BuildHandlersForTransports rest
  ) =>
  BuildHandlersForTransports (transport ': rest)
  where
  buildHandlersForAll transportsMap cmdProxy handler = do
    let transportNameText =
          GHC.symbolVal (Record.Proxy @(NameOf transport))
            |> Text.fromLinkedList
    let thisHandler =
          case transportsMap |> Map.get transportNameText of
            Nothing ->
              panic [fmt|Transport not registered: #{transportNameText}|]
            Just transportVal -> do
              let typedTransport :: transport = getTransportValue transportVal
              Array.fromLinkedList
                [ ( transportNameText,
                    buildHandler @transport typedTransport cmdProxy handler
                  )
                ]
    let restHandlers = buildHandlersForAll @rest transportsMap cmdProxy handler
    Array.prepend thisHandler restHandlers


type instance NameOf (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) = name


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
    commandTransports
    (transportNames :: [Symbol])
    (commandTransportNames :: [Symbol])
    event
    entity
    entityName
    entityIdType.
  ( Command cmd,
    Schema.ToSchema cmd,
    commandName ~ NameOf cmd,
    commandTransports ~ TransportsOf cmd,
    transportNames ~ NamesOf (PublicTransports commandTransports),
    AllKnownSymbols transportNames,
    BuildHandlersForTransports (PublicTransports commandTransports),
    entityName ~ NameOf entity,
    Record.KnownSymbol commandName,
    Record.KnownSymbol entityName,
    Record.KnownHash commandName,
    entity ~ EntityOf cmd,
    event ~ EventOf entity,
    Json.FromJSON event,
    Json.ToJSON event,
    CommandInspect (CommandDefinition commandName commandTransports cmd transportNames event entity entityName entityIdType)
  ) =>
  Service originalCommands commandTransportNames ->
  Service
    ( (commandName 'Record.:= CommandDefinition commandName commandTransports cmd transportNames event entity entityName entityIdType)
        ': originalCommands
    )
    (AppendSymbols transportNames commandTransportNames)
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandTransports cmd transportNames event entity entityName entityIdType) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName),
              transportNames = allSymbolTexts @transportNames,
              commandSchema = Schema.toSchema @cmd
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict ::
        Record.Dict (CommandInspect) (CommandDefinition commandName commandTransports cmd transportNames event entity entityName entityIdType) = Record.Dict
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
    -- Returns a tuple of (public handlers, public schemas, dispatch handlers).
    -- The dispatch handlers are transport-independent command handlers used by integrations.
    getEndpointsByTransport ::
      EventStore Json.Value ->
      Map Text TransportValue ->
      Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text Service.Transport.EndpointSchema), Map Text EndpointHandler)
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
-- Returns a tuple of (public handlers, public schemas, dispatch handlers).
-- Application will merge public maps across transports and merge dispatch handlers
-- separately for integration command dispatch.
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
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text Service.Transport.EndpointSchema), Map Text EndpointHandler)
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
        Record.K (Array (Text, Text, EndpointHandler, Schema)) (cmdDef)
      mapper (Record.I x) = do
        let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
        let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
        let handlerPairs = createHandlers x typedEventStore typedCache transportsMap (Record.Proxy @cmd)
        Record.K
          ( handlerPairs
              |> Array.map (\(tName, handler) -> (tName, commandName x, handler, commandSchema x))
          )

  let dispatchMapper ::
        forall cmdDef cmd.
        ( CommandInspect cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I cmdDef ->
        Record.K (Maybe (Text, EndpointHandler)) cmdDef
      dispatchMapper (Record.I x) = do
        if includeInDispatchMap x
          then do
            let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
            let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
            let dispatchHandler = createDispatchHandler x typedEventStore typedCache (Record.Proxy @cmd)
            Record.K (Just (commandName x, dispatchHandler))
          else Record.K Nothing

  let endpoints :: Array (Text, Text, EndpointHandler, Schema) =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
          |> Record.collapse
          |> Array.fromLinkedList
          |> Array.flatten

  let dispatchHandlers =
        commandDefinitions
          |> Record.cmap (Record.Proxy @(CommandInspect)) dispatchMapper
          |> Record.collapse
          |> Array.fromLinkedList
          |> Array.reduce
              (\maybeHandler acc ->
                case maybeHandler of
                  Nothing -> acc
                  Just handler -> acc |> Array.push handler
              )
              Array.empty

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

  let groupDispatch (cmdName, handler) dispatchResult =
        case dispatchResult of
          Err err -> Err err
          Ok dispatchAcc ->
            case dispatchAcc |> Map.get cmdName of
              Just _ -> Err [fmt|Duplicate command handler registered for integration dispatch: #{cmdName}|]
              Nothing -> Ok (dispatchAcc |> Map.set cmdName handler)

  let (handlersByTransport, schemasByTransport) =
        endpoints |> Array.reduce groupByTransport (Map.empty, Map.empty)

  let dispatchMapResult = dispatchHandlers |> Array.reduce groupDispatch (Ok Map.empty)

  dispatchMap <- case dispatchMapResult of
    Err err -> Task.throw err
    Ok mapValue -> Task.yield mapValue

  Task.yield (handlersByTransport, schemasByTransport, dispatchMap)


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
