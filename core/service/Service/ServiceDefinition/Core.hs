{- HLINT ignore "Use camelCase" -}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Service.ServiceDefinition.Core (
  Service,
  new,
  command,
  toServiceRunner,
  validateCommandTransports,
  RunnableCommand (..),
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
import Log qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..), withDefault)
import Result (Result (..))
import Record (Record)
import Record qualified
import Schema (FieldSchema (..), Schema)
import Schema qualified
import Service.Auth (RequestContext (..))
import Service.Auth qualified as Auth
import Service.Transport.Internal (InternalTransport)
import Service.Transport (Transport (..), EndpointHandler, DispatchHandler, EndpointSchema (..))
import Service.Command (EntityOf, EventOf)
import Service.Command.Core (InferredTransport, TransportsOf, NamesOf, AppendSymbols, AllKnownSymbols (..), Command (..), Entity (..), Event, KnownMultiTenant (..), NameOf)
import Service.CommandExecutor.CommandExecutionFailure qualified as CommandExecutionFailure
import Service.CommandExecutor qualified as CommandExecutor
import Service.Response qualified as Response
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event.EntityName (EntityName (..))
import Service.Event.EventMetadata (EventMetadata (..))
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
import Uuid qualified


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


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


type family PublicTransports (transports :: [Type]) :: [Type] where
  PublicTransports '[InferredTransport] = '[]
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


type family ValidCommandTransports (transports :: [Type]) :: Constraint where
  ValidCommandTransports '[InferredTransport] = ()
  ValidCommandTransports transports =
    ( ExplicitTransports (TypeExists InferredTransport transports)
    , AssertNoDuplicatePublicTransports (PublicTransports transports)
    , RequireAdapters (PublicTransports transports)
    )


type family ExplicitTransports (includesInferred :: Bool) :: Constraint where
  ExplicitTransports 'False = ()
  ExplicitTransports 'True =
    TypeError ('Text "InferredTransport must be the only entry in TransportsOf.")


type family RequireAdapters (transports :: [Type]) :: Constraint where
  RequireAdapters '[] = ()
  RequireAdapters (adapter ': rest) = (Transport adapter, RequireAdapters rest)


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
    publicRouting :: PublicRouting,
    commandSchema :: Schema
  }
  deriving (Show)


class CommandInspect definition where
  type Cmd definition
  type CmdEntity definition
  type CmdEvent definition


  commandName :: definition -> Text


  commandSchema :: definition -> Schema


  commandRouting :: definition -> PublicRouting


  createHandlers ::
    definition ->
    EventStore (CmdEvent definition) ->
    Maybe (SnapshotCache (CmdEntity definition)) ->
    Array (Text, TransportValue) ->
    Record.Proxy (Cmd definition) ->
    Array (Text, EndpointHandler)


  createDispatchHandler ::
    definition ->
    EventStore (CmdEvent definition) ->
    Maybe (SnapshotCache (CmdEntity definition)) ->
    Record.Proxy (Cmd definition) ->
    DispatchHandler


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
  (Maybe EventMetadata -> RequestContext -> command -> Task Text Response.CommandResponse) ->
  DispatchHandler
buildDispatchHandler _ handler sourceEventMetadata requestContext body respond = do
  let commandName =
        GHC.symbolVal (Record.Proxy @name)
          |> Text.fromLinkedList
  let commandValue = body |> Json.decodeBytes @command
  case commandValue of
    Ok cmd -> do
      response <- handler sourceEventMetadata requestContext cmd
      let responseJson = Json.encodeText response |> Text.toBytes
      respond (response, responseJson)
    Err _err -> do
      requestId <- if requestContext.requestId == Uuid.nil
        then Uuid.generate
        else Task.yield requestContext.requestId
      let requestIdText = Uuid.toText requestId
      let correlationIdText = case sourceEventMetadata of
            Just metadata -> Maybe.withDefault (Uuid.toText metadata.eventId) metadata.correlationId
            Nothing -> requestIdText
      let sourceTraceFields = case sourceEventMetadata of
            Nothing -> []
            Just metadata -> do
              let actorFields = case metadata.relatedUserSub of
                    Just actor -> ["relatedUserSub" Json..= actor]
                    Nothing -> []
              ["causationId" Json..= Uuid.toText metadata.eventId] |> Array.append actorFields
      Log.recordIsolated Log.Warn
        ( [ "component" Json..= ("ServiceDefinition" :: Text)
          , "requestId" Json..= requestIdText
          , "correlationId" Json..= correlationIdText
          , "commandName" Json..= commandName
          , "kind" Json..= ("command_result" :: Text)
          , "result" Json..= ("rejected" :: Text)
          , "failureStage" Json..= ("decoding" :: Text)
          , "errorClass" Json..= ("InvalidCommand" :: Text)
          , "eventsAppended" Json..= (0 :: Int)
          ]
          |> Array.append sourceTraceFields
        )
        "Command payload could not be decoded"
        |> Task.ignoreError
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
    KnownMultiTenant (IsMultiTenant cmd),
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
  Maybe EventMetadata ->
  RequestContext ->
  cmd ->
  Task Text Response.CommandResponse
buildCommandResponseHandler eventStore maybeCache sourceEventMetadata requestContext cmdInstance = do
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
  let dependencies = CommandExecutor.ExecutionDependencies {eventStore, entityFetcher = fetcher, entityName}
  let source = case sourceEventMetadata of
        Nothing -> CommandExecutor.FromRequest requestContext
        Just metadata -> CommandExecutor.FromEvent requestContext metadata
  result <-
    CommandExecutor.execute dependencies source cmdInstance
      |> Task.mapError CommandExecutionFailure.message
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
     ValidCommandTransports transports,
     KnownBool (IncludesInternalTransport transports),
     AllKnownSymbols transportNames,
     name ~ NameOf cmd,
     Record.KnownSymbol name,
    Record.KnownSymbol entityName,
    Json.FromJSON event,
    Json.ToJSON event,
    KnownMultiTenant (IsMultiTenant cmd),
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


  commandRouting definition = definition.publicRouting


  createHandlers ::
    (CommandDefinition name transports cmd transportNames event entity entityName entityIdType) ->
    EventStore event ->
    Maybe (SnapshotCache entity) ->
    Array (Text, TransportValue) ->
    Record.Proxy cmd ->
    Array (Text, EndpointHandler)
  createHandlers _ eventStore maybeCache adapters cmd = do
    let execute = buildCommandResponseHandler @cmd @entity @event @entityName eventStore maybeCache Nothing
    adapters |> Array.map (\(name, TransportValue adapter) -> (name, buildHandler adapter cmd execute))


  createDispatchHandler _ eventStore maybeCache cmd =
    buildDispatchHandler @cmd cmd
      (buildCommandResponseHandler @cmd @entity @event @entityName eventStore maybeCache)


  includeInDispatchMap _ = boolValue @(IncludesInternalTransport transports)


data PublicRouting = SoleRegisteredTransport | NamedTransports (Array Text)
  deriving (Show)


resolveAdapters :: Map Text TransportValue -> Text -> PublicRouting -> Task Text (Array (Text, TransportValue))
resolveAdapters registered commandName routing =
  case routing of
    SoleRegisteredTransport -> case Array.toLinkedList (Map.entries registered) of
      [entry] -> Task.yield [entry]
      [] -> Task.throw [fmt|Command #{commandName} needs a transport. Register one with withTransport or declare InternalTransport for internal dispatch.|]
      _ -> do
        let names = Text.joinWith ", " (Map.keys registered)
        Task.throw [fmt|Command #{commandName} has multiple registered transports: #{names}. Set transports explicitly in its command declaration.|]
    NamedTransports names ->
      names |> Task.mapArray (\name -> case Map.get name registered of
        Nothing -> Task.throw [fmt|Command #{commandName} selects unregistered transport #{name}. Register it with withTransport.|]
        Just adapter -> Task.yield (name, adapter))


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
    ValidCommandTransports commandTransports,
    KnownBool (TypeExists InferredTransport commandTransports),
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
              publicRouting =
                if boolValue @(TypeExists InferredTransport commandTransports)
                  then SoleRegisteredTransport
                  else NamedTransports (allSymbolTexts @transportNames),
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
data ServiceRunner
  = ServiceRunner {getEndpointsByTransport :: BuildEndpoints}
  | DeclaredServiceRunner
      { getEndpointsByTransport :: BuildEndpoints,
        commandRoutes :: Array (Text, PublicRouting)
      }


type BuildEndpoints =
  EventStore Json.Value ->
  Map Text TransportValue ->
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text EndpointSchema), Map Text DispatchHandler)


-- | Check declared routes before creating application resources.
validateCommandTransports :: Map Text TransportValue -> ServiceRunner -> Task Text Unit
validateCommandTransports registered runner = case runner of
  ServiceRunner {} -> pass
  DeclaredServiceRunner {commandRoutes} ->
    commandRoutes |> Task.forEach (\(name, routing) -> do
      _ <- resolveAdapters registered name routing
      pass)


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
  case Record.reflectAllFields service.inspectDict of
    Record.Reflected ->
      DeclaredServiceRunner
        { commandRoutes =
            service.commandDefinitions
              |> Record.cmap (Record.Proxy @CommandInspect) (\(Record.I defn) -> Record.K (commandName defn, commandRouting defn))
              |> Record.collapse
              |> Array.fromLinkedList,
          getEndpointsByTransport = \rawEventStore transportsMap ->
            buildEndpointsByTransport @cmds @event @entity rawEventStore service.commandDefinitions transportsMap
        }


-- | Register one declared command through the existing service runtime.
class (Command command) => RunnableCommand command where
  commandRunner :: ServiceRunner
  runRuleCommand :: EventStore Json.Value -> command -> Task Text Response.CommandResponse


instance
  ( Command command,
    Schema.ToSchema command,
    Json.FromJSON command,
    Entity (EntityOf command),
    Event (EventOf (EntityOf command)),
    EntityOf (EventOf (EntityOf command)) ~ EntityOf command,
    StreamId.ToStreamId (EntityIdType (EntityOf command)),
    Eq (EntityIdType (EntityOf command)),
    Ord (EntityIdType (EntityOf command)),
    Show (EntityIdType (EntityOf command)),
    Show (EventOf (EntityOf command)),
    Json.FromJSON (EventOf (EntityOf command)),
    Json.ToJSON (EventOf (EntityOf command)),
    Json.FromJSON (EntityOf command),
    Json.ToJSON (EntityOf command),
    ValidCommandTransports (TransportsOf command),
    KnownBool (TypeExists InferredTransport (TransportsOf command)),
    KnownBool (IncludesInternalTransport (TransportsOf command)),
    AllKnownSymbols (NamesOf (PublicTransports (TransportsOf command))),
    KnownMultiTenant (IsMultiTenant command),
    Record.KnownSymbol (NameOf command),
    Record.KnownHash (NameOf command),
    Record.KnownSymbol (NameOf (EntityOf command))
  ) =>
  RunnableCommand command
  where
  commandRunner = toServiceRunner (command @command new)
  runRuleCommand rawEventStore commandValue = do
    requestContext <- Auth.trustedContext
    let eventStore = rawEventStore |> EventStore.castEventStore @(EventOf (EntityOf command))
    buildCommandResponseHandler
      @command
      @(EntityOf command)
      @(EventOf (EntityOf command))
      @(NameOf (EntityOf command))
      eventStore
      Nothing
      Nothing
      requestContext
      commandValue


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
  Task Text (Map Text (Map Text EndpointHandler), Map Text (Map Text Service.Transport.EndpointSchema), Map Text DispatchHandler)
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
        Record.K (Task Text (Array (Text, Text, EndpointHandler, Schema))) cmdDef
      mapper (Record.I x) = Record.K do
        adapters <- resolveAdapters transportsMap (commandName x) (commandRouting x)
        let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
        let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
        let handlerPairs = createHandlers x typedEventStore typedCache adapters (Record.Proxy @cmd)
        Task.yield
          ( handlerPairs
              |> Array.map (\(tName, handler) -> (tName, commandName x, handler, commandSchema x))
          )

  let dispatchMapper ::
        forall cmdDef cmd.
        ( CommandInspect cmdDef,
          cmd ~ Cmd cmdDef
        ) =>
        Record.I cmdDef ->
        Record.K (Maybe (Text, DispatchHandler)) cmdDef
      dispatchMapper (Record.I x) = do
        if includeInDispatchMap x
          then do
            let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
            let typedCache = GHC.unsafeCoerce maybeCache :: Maybe (SnapshotCache (CmdEntity cmdDef))
            let dispatchHandler = createDispatchHandler x typedEventStore typedCache (Record.Proxy @cmd)
            Record.K (Just (commandName x, dispatchHandler))
          else Record.K Nothing

  endpoints <-
    commandDefinitions
      |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
      |> Record.collapse
      |> Array.fromLinkedList
      |> Task.mapArray identity
      |> Task.map Array.flatten

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

  let groupByTransport (transportNameText, cmdName, handler, schema) (handlersAcc, schemasAcc) = do
        let endpointName =
              if transportNameText == "WebTransport"
                then cmdName |> Text.toKebabCase
                else cmdName
        let endpointSchema = Service.Transport.EndpointSchema
              { requestSchema = Just schema
              , responseSchema = commandResponseSchema
              , description = ""
              , deprecated = False
              , entityName = Just entityNameText
              }
        case handlersAcc |> Map.get transportNameText of
          Nothing -> do
            let newHandlers = handlersAcc |> Map.set transportNameText (Map.empty |> Map.set endpointName handler)
            let newSchemas = schemasAcc |> Map.set transportNameText (Map.empty |> Map.set endpointName endpointSchema)
            (newHandlers, newSchemas)
          Just existingHandlers ->
            case existingHandlers |> Map.get endpointName of
              Just _ ->
                panic [fmt|Duplicate command handler registered for: #{endpointName}|]
              Nothing -> do
                let newHandlers = handlersAcc |> Map.set transportNameText (existingHandlers |> Map.set endpointName handler)
                let existingSchemas = schemasAcc |> Map.get transportNameText |> Maybe.withDefault Map.empty
                let newSchemas = schemasAcc |> Map.set transportNameText (existingSchemas |> Map.set endpointName endpointSchema)
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
