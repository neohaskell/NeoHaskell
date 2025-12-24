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
import Service.Api.ApiBuilder (ApiBuilder (..), ApiEndpointHandler, ApiEndpoints (..))
import Service.Command (EntityOf, EventOf)
import Service.Command.Core (ApiOf, Command (..), Entity (..), Event, NameOf)
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
    (commandApiNames :: [Symbol])
    (providedApiNames :: [Symbol])
    (eventStoreConfig :: Type)
  = Service
  { commandDefinitions :: Record commandRow,
    inspectDict :: Record.ContextRecord (Record.Dict (CommandInspect)) commandRow,
    apis :: Map Text ApiBuilderValue,
    eventStoreConfig :: eventStoreConfig
  }


data ApiBuilderValue
  = forall api.
    (ApiBuilder api) =>
    ApiBuilderValue api


getApiBuilderValue ::
  ApiBuilderValue -> api
getApiBuilderValue (ApiBuilderValue api) = GHC.unsafeCoerce api


getSymbolText :: forall name. (GHC.KnownSymbol name) => Record.Proxy name -> Text
getSymbolText _ =
  GHC.symbolVal (Record.Proxy @name)
    |> Text.fromLinkedList


data
  CommandDefinition
    (name :: Symbol)
    (api :: Type)
    (cmd :: Type)
    (apiName :: Symbol)
    (event :: Type)
    (entity :: Type)
    (entityName :: Symbol)
    (entityIdType :: Type)
  = CommandDefinition
  { commandName :: Text,
    apiName :: Text
  }
  deriving (Show)


class CommandInspect definition where
  type Cmd definition
  type CmdEntity definition
  type CmdEvent definition
  type Api definition
  type ApiName definition :: Symbol


  commandName :: definition -> Text


  apiName :: definition -> Text


  createHandler ::
    definition ->
    EventStore (CmdEvent definition) ->
    Api definition ->
    Record.Proxy (Cmd definition) ->
    ApiEndpointHandler


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
    ApiBuilder api,
    name ~ NameOf cmd,
    Record.KnownSymbol apiName,
    Record.KnownSymbol name,
    Record.KnownSymbol entityName,
    Json.FromJSON event,
    Json.ToJSON event,
    IsMultiTenant cmd ~ False,
    Record.KnownHash name
  ) =>
  CommandInspect (CommandDefinition name api cmd apiName event entity entityName entityIdType)
  where
  type Cmd (CommandDefinition name api cmd apiName event entity entityName entityIdType) = cmd
  type CmdEvent (CommandDefinition name api cmd apiName event entity entityName entityIdType) = event
  type CmdEntity (CommandDefinition name api cmd apiName event entity entityName entityIdType) = entity
  type Api (CommandDefinition name api cmd apiName event entity entityName entityIdType) = api
  type ApiName (CommandDefinition name api cmd apiName event entity entityName entityIdType) = apiName


  commandName ::
    (Record.KnownSymbol name) => CommandDefinition name api cmd apiName event entity entityName entityIdType -> Text
  commandName _ = getSymbolText (Record.Proxy @name)


  apiName _ = getSymbolText (Record.Proxy @apiName)


  createHandler ::
    (CommandDefinition name api cmd apiName event entity entityName entityIdType) ->
    EventStore event ->
    api ->
    Record.Proxy cmd ->
    ApiEndpointHandler
  createHandler _ eventStore api cmd reqBytes respondCallback = do
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

    buildCommandHandler @api api cmd handler reqBytes respondCallback


type instance NameOf (CommandDefinition name api cmd apiName event entity entityName entityIdType) = name


new :: Service '[] '[] '[] Unit
new =
  Service
    { commandDefinitions = Record.empty,
      inspectDict = Record.empty,
      apis = Map.empty,
      eventStoreConfig = unit
    }


useServer ::
  forall api apiName commandApiNames providedApiNames cmds eventStoreConfig.
  ( ApiBuilder api,
    apiName ~ NameOf api,
    Record.KnownHash apiName,
    Record.KnownSymbol apiName
  ) =>
  api ->
  Service cmds commandApiNames providedApiNames eventStoreConfig ->
  Service cmds commandApiNames (apiName ': providedApiNames) eventStoreConfig
useServer api serviceDefinition = do
  let apiName = getSymbolText (Record.Proxy @apiName)
  let newApis = serviceDefinition.apis |> Map.set apiName (ApiBuilderValue api)
  serviceDefinition
    { apis = newApis
    }


useEventStore ::
  forall eventStoreConfig cmds commandApiNames providedApiNames.
  (EventStoreConfig eventStoreConfig) =>
  eventStoreConfig ->
  Service cmds commandApiNames providedApiNames _ ->
  Service cmds commandApiNames providedApiNames eventStoreConfig
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
    commandApi
    (apiName :: Symbol)
    (commandApiNames :: [Symbol])
    providedApiNames
    eventStoreConfig
    event
    entity
    entityName
    entityIdType.
  ( Command cmd,
    commandName ~ NameOf cmd,
    commandApi ~ ApiOf cmd,
    apiName ~ NameOf commandApi,
    entityName ~ NameOf entity,
    Record.KnownSymbol apiName,
    Record.KnownSymbol commandName,
    Record.KnownSymbol entityName,
    Record.KnownHash commandName,
    entity ~ EntityOf cmd,
    event ~ EventOf entity,
    Json.FromJSON event,
    Json.ToJSON event,
    CommandInspect (CommandDefinition commandName commandApi cmd apiName event entity entityName entityIdType)
  ) =>
  Service originalCommands commandApiNames providedApiNames eventStoreConfig ->
  Service
    ( (commandName 'Record.:= CommandDefinition commandName commandApi cmd apiName event entity entityName entityIdType)
        ': originalCommands
    )
    (apiName ': commandApiNames)
    providedApiNames
    eventStoreConfig
command serviceDefinition = do
  let cmdName :: Record.Field commandName = fromLabel
  let cmdVal :: Record.I (CommandDefinition commandName commandApi cmd apiName event entity entityName entityIdType) =
        Record.I
          CommandDefinition
            { commandName = getSymbolText (Record.Proxy @commandName),
              apiName = getSymbolText (Record.Proxy @apiName)
            }
  let currentCmds :: Record originalCommands = serviceDefinition.commandDefinitions
  let cmds =
        currentCmds
          |> Record.insert cmdName cmdVal
  let inspectDict ::
        Record.Dict (CommandInspect) (CommandDefinition commandName commandApi cmd apiName event entity entityName entityIdType) = Record.Dict
  let newInspectDict =
        serviceDefinition.inspectDict
          |> Record.insert cmdName inspectDict

  Service
    { commandDefinitions = cmds,
      apis = serviceDefinition.apis,
      inspectDict = newInspectDict,
      eventStoreConfig = serviceDefinition.eventStoreConfig
    }


__internal_runServiceMain ::
  forall cmds commandApiNames providedApiNames eventStoreConfig event.
  ( EventStoreConfig eventStoreConfig,
    event ~ ServiceEventType cmds,
    Json.FromJSON event,
    Json.ToJSON event
  ) =>
  Service cmds commandApiNames providedApiNames eventStoreConfig -> GHC.IO Unit
__internal_runServiceMain s = do
  case Record.reflectAllFields s.inspectDict of
    Record.Reflected ->
      runService @cmds @eventStoreConfig @event s.eventStoreConfig s.commandDefinitions s.apis
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
  Map Text ApiBuilderValue ->
  Task Text Unit
runService
  eventStoreConfig
  commandDefinitions
  apis = do
    -- Create the EventStore ONCE at service startup
    eventStore <- EventStore.createEventStore @eventStoreConfig @event eventStoreConfig

    let mapper ::
          forall cmdDef cmd.
          ( CommandInspect cmdDef,
            cmd ~ Cmd cmdDef
          ) =>
          Record.I (cmdDef) ->
          Record.K ((Text, ApiBuilderValue), (Text, ApiEndpointHandler)) (cmdDef)
        mapper (Record.I x) = do
          case apis |> Map.get (apiName x) of
            Nothing -> panic [fmt|The impossible happened, couldn't find API config for #{commandName x}|]
            Just apiBV -> do
              let api = getApiBuilderValue apiBV
              -- Use unsafeCoerce because ServiceEventType guarantees event ~ CmdEvent cmdDef
              -- but GHC can't prove this within the cmap context
              let typedEventStore = GHC.unsafeCoerce eventStore :: EventStore (CmdEvent cmdDef)
              Record.K ((apiName x, apiBV), (commandName x, createHandler x typedEventStore (api) (Record.Proxy @cmd)))

    let xs :: Array ((Text, ApiBuilderValue), (Text, ApiEndpointHandler)) =
          commandDefinitions
            |> Record.cmap (Record.Proxy @(CommandInspect)) mapper
            |> Record.collapse
            |> Array.fromLinkedList

    -- Group endpoints by API name
    let endpointsReducer ((apiName, apiBV), (commandName, handler)) endpointsMap = do
          let api = getApiBuilderValue apiBV
          case endpointsMap |> Map.get apiName of
            Nothing -> do
              -- First command for this API, create new ApiEndpoints
              let newEndpoints =
                    ApiEndpoints
                      { api = api,
                        commandEndpoints = Map.empty |> Map.set commandName handler
                      }
              endpointsMap |> Map.set apiName newEndpoints
            Just existingEndpoints -> do
              -- Add command to existing API endpoints
              let updatedEndpoints =
                    existingEndpoints
                      { commandEndpoints = existingEndpoints.commandEndpoints |> Map.set commandName handler
                      }
              endpointsMap |> Map.set apiName updatedEndpoints

    let endpointsByApi =
          xs |> Array.reduce endpointsReducer Map.empty

    -- Run each API with its endpoints
    endpointsByApi
      |> Map.entries
      |> Task.forEach \(apiName, apiEndpoints) -> do
        let commandCount = Map.length apiEndpoints.commandEndpoints
        Console.print [fmt|Starting API: #{apiName} with #{commandCount} commands|]

        -- Assemble the API using the ApiBuilder's assembleApi function
        -- Since we need to call assembleApi with the proper type, we need to
        -- extract it from the ApiBuilderValue
        case apis |> Map.get apiName of
          Nothing -> panic [fmt|API #{apiName} not found in apis map|]
          Just apiBV -> do
            -- We need to use the existentially quantified API type
            case apiBV of
              ApiBuilderValue (api :: api) -> do
                -- Cast apiEndpoints to match the specific api type
                let typedEndpoints = GHC.unsafeCoerce apiEndpoints :: ApiEndpoints api
                let runnableApi = assembleApi typedEndpoints

                -- Run the assembled API
                runApi api runnableApi