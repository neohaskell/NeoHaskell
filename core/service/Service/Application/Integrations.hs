{-# LANGUAGE AllowAmbiguousTypes #-}

-- | # Service.Application.Integrations
--
-- Integration configuration and runtime for the Application.
--
-- This module provides:
--
-- * 'withOutbound' - Register outbound integrations (react to events)
-- * 'withOutboundLifecycle' - Register stateful integrations with resource management
-- * 'withInbound' - Register inbound integrations (external -> commands)
-- * Runtime functions for starting integration workers
module Service.Application.Integrations (
  -- * Configuration
  withOutbound,
  withOutboundLifecycle,
  withInbound,

  -- * Runtime (internal)
  startIntegrationSubscriber,
  startInboundWorkers,
) where

import Array (Array)
import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import Basics
import Console qualified
import Default (Default (..))
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Map (Map)
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Entity.Core (Entity (..), EventOf)
import Service.Event (Event (..), EntityName (..))
import Service.Event.StreamId (StreamId (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.Transport (EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)
import TypeName qualified


-- | Register an outbound integration for an entity type.
--
-- Outbound integrations react to entity events and can trigger external effects
-- or emit commands to other services (Process Manager pattern).
--
-- The integration function receives the current entity state (reconstructed from
-- the event stream via replay) and the new event, and returns actions to execute.
--
-- Example:
--
-- @
-- app = Application.new
--   |> Application.withService cartService
--   |> Application.withOutbound \@CartEntity \@CartEvent cartIntegrations
-- @
createOutboundRunner ::
  forall entity event.
  ( Json.FromJSON entity
  , Json.FromJSON event
  , Json.FromJSON (EventOf entity)
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  , EventOf entity ~ event
  ) =>
  (entity -> event -> Integration.Outbound) ->
  OutboundRunner
createOutboundRunner integrationFn = do
  let typeName = TypeName.reflect @entity
  OutboundRunner
    { entityTypeName = typeName
    , processEvent = \maybeCtx eventStore rawEvent -> do
        let streamId = rawEvent.streamId
        -- Decode the event from JSON
        case Json.decode @event rawEvent.event of
          Err decodeErr -> do
            Console.print [fmt|[Integration] Failed to decode event for #{typeName} (stream: #{streamId}): #{decodeErr}|]
              |> Task.ignoreError
            Task.yield Array.empty
          Ok decodedEvent -> do
            -- Reconstruct entity state via event replay
            entity <- fetchEntityState @entity eventStore typeName streamId
            -- Call the integration function
            let outbound = integrationFn entity decodedEvent
            let actions = Integration.getActions outbound
            -- Execute all actions and collect command payloads
            actions
              |> Task.mapArray (\action -> do
                  result <- case maybeCtx of
                    Just ctx -> Integration.runActionWithContext ctx action |> Task.mapError toText
                    Nothing -> Integration.runAction action |> Task.mapError toText
                  case result of
                    Just payload -> Task.yield [payload]
                    Nothing -> Task.yield []
                )
              |> Task.map Array.flatten
    }


-- | Fetch entity state by replaying events from the event store.
--
-- Uses EntityFetcher to read all events for the given stream and apply
-- the entity's updateImpl function to reconstruct current state.
--
-- If no events exist for the stream, returns the initial state.
fetchEntityState ::
  forall entity.
  ( Json.FromJSON (EventOf entity)
  , Entity entity
  ) =>
  EventStore Json.Value ->
  Text ->
  StreamId ->
  Task Text entity
fetchEntityState eventStore typeName streamId = do
  -- Create EntityFetcher with the entity's update function
  let initialState = initialStateImpl @entity
  let updateFn = updateImpl @entity
  -- We need to decode the JSON event to the entity's event type
  -- Track decode errors alongside state to fail if any events couldn't be decoded
  let decodingReducer :: Json.Value -> (entity, Array Text) -> (entity, Array Text)
      decodingReducer jsonValue (state, errors) = do
        case Json.decode @(EventOf entity) jsonValue of
          Err decodeErr -> do
            -- Track the error - we'll fail after fetching if any errors occurred
            let errorMsg = [fmt|Failed to decode event: #{decodeErr}|]
            (state, errors |> Array.push errorMsg)
          Ok event -> (updateFn event state, errors)
  fetcherResult <-
    EntityFetcher.new eventStore (initialState, Array.empty) decodingReducer
      |> Task.mapError (\err -> [fmt|[Integration] Failed to create EntityFetcher for #{typeName}: #{toText err}|])
  result <-
    fetcherResult.fetch (EntityName typeName) streamId
      |> Task.mapError (\err -> [fmt|[Integration] Failed to fetch entity #{typeName} (stream: #{streamId}): #{toText err}|])
  case result of
    EntityFetcher.EntityNotFound -> do
      -- Entity doesn't exist yet - use initial state
      -- This can happen if the integration receives the first event for a new entity
      Task.yield initialState
    EntityFetcher.EntityFound fetched -> do
      let (entityState, decodeErrors) = fetched.state
      -- Fail if any events could not be decoded - this indicates schema drift or data corruption
      case Array.first decodeErrors of
        Just firstError -> do
          let errorCount = Array.length decodeErrors
          Task.throw [fmt|[Integration] #{errorCount} event(s) failed to decode for #{typeName} (stream: #{streamId}). First error: #{firstError}|]
        Nothing -> do
          Task.yield entityState


-- | Register an outbound integration for an entity type.
--
-- See 'createOutboundRunner' for details.
withOutbound ::
  forall entity event.
  ( Json.FromJSON entity
  , Json.FromJSON event
  , Json.FromJSON (EventOf entity)
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  , EventOf entity ~ event
  ) =>
  (entity -> event -> Integration.Outbound) ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound) ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound)
withOutbound integrationFn (runners, lifecycleRunners, inbounds) = do
  let runner = createOutboundRunner @entity @event integrationFn
  (runners |> Array.push runner, lifecycleRunners, inbounds)


-- | Register an outbound integration with lifecycle management.
--
-- Use this for integrations that need expensive resources like:
--
-- * Database connection pools
-- * Embedded interpreters (Lua, Python, etc.)
-- * gRPC channels
-- * WebSocket subscriptions
--
-- Workers are created per-entity. When a worker is idle for too long,
-- it is reaped and cleanup is called. The next event for that entity
-- will spawn a fresh worker with initialize called again.
createOutboundLifecycleRunner ::
  forall (entity :: Type) state.
  ( TypeName.Inspectable entity
  ) =>
  Lifecycle.OutboundConfig state ->
  OutboundLifecycleRunner
createOutboundLifecycleRunner config = do
  let typeName = TypeName.reflect @entity
  OutboundLifecycleRunner
    { entityTypeName = typeName
    , spawnWorkerState = \streamId -> do
        -- Initialize resources for this entity
        state <- config.initialize streamId
        -- Return a WorkerState that captures the initialized state
        Task.yield WorkerState
          { processEvent = \event -> config.processEvent state event
          , cleanup = config.cleanup state
          }
    }


-- | Register an outbound integration with lifecycle management.
--
-- See 'createOutboundLifecycleRunner' for details.
withOutboundLifecycle ::
  forall (entity :: Type) state.
  ( TypeName.Inspectable entity
  ) =>
  Lifecycle.OutboundConfig state ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound) ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound)
withOutboundLifecycle config (runners, lifecycleRunners, inbounds) = do
  let runner = createOutboundLifecycleRunner @entity config
  (runners, lifecycleRunners |> Array.push runner, inbounds)


-- | Register an inbound integration worker.
--
-- Inbound integrations listen to external sources (timers, webhooks, message queues)
-- and translate external events into domain commands.
--
-- When the Application runs, each inbound worker is started in its own background task.
-- Workers emit commands that are dispatched to the appropriate service handlers.
withInbound ::
  Integration.Inbound ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound) ->
  (Array OutboundRunner, Array OutboundLifecycleRunner, Array Integration.Inbound)
withInbound inboundIntegration (runners, lifecycleRunners, inbounds) =
  (runners, lifecycleRunners, inbounds |> Array.push inboundIntegration)


-- | Start subscription for outbound integrations.
--
-- Subscribes to all events and processes them through registered OutboundRunners.
-- Commands emitted by integrations are dispatched to the appropriate service handlers.
--
-- Uses a Dispatcher to route events to per-entity workers, ensuring sequential
-- processing within each entity while allowing parallel processing across entities.
--
-- Returns the Dispatcher so it can be shutdown when the application stops.
startIntegrationSubscriber ::
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Maybe Integration.ActionContext ->
  Task Text (Maybe Dispatcher.IntegrationDispatcher)
startIntegrationSubscriber eventStore runners lifecycleRunners commandEndpoints maybeCtx = do
  let hasRunners = not (Array.isEmpty runners)
  let hasLifecycleRunners = not (Array.isEmpty lifecycleRunners)

  if not hasRunners && not hasLifecycleRunners
    then Task.yield Nothing
    else do
      -- Create dispatcher with both runner types and EventStore for entity reconstruction
      dispatcher <- Dispatcher.newWithLifecycleConfig Dispatcher.defaultConfig eventStore runners lifecycleRunners commandEndpoints maybeCtx

      let processIntegrationEvent :: Event Json.Value -> Task Text Unit
          processIntegrationEvent rawEvent = do
            result <- Dispatcher.dispatch dispatcher rawEvent |> Task.asResult
            case result of
              Ok _ -> Task.yield unit
              Err err ->
                Console.print [fmt|[Integration] Error dispatching event: #{err}|]
                  |> Task.ignoreError

      _ <- eventStore.subscribeToAllEvents processIntegrationEvent
        |> Task.mapError (toText :: Error -> Text)
      
      -- Return the dispatcher so it can be shutdown later
      Task.yield (Just dispatcher)


-- | Start all inbound integration workers.
--
-- Each worker runs in its own background task, emitting commands that are
-- dispatched to the appropriate service handlers. Workers run indefinitely
-- with automatic restart on failure (with exponential backoff).
--
-- Returns the spawned AsyncTasks so they can be cancelled on shutdown.
startInboundWorkers ::
  Array Integration.Inbound ->
  Map Text EndpointHandler ->
  Task Text (Array (AsyncTask Text Unit))
startInboundWorkers inbounds commandEndpoints = do
  if Array.isEmpty inbounds
    then Task.yield Array.empty
    else do
      let workerCount = Array.length inbounds
      Console.print [fmt|[Integration] Starting #{workerCount} inbound worker(s)|]
      inbounds
        |> Task.mapArray \inboundIntegration -> do
            -- Start each worker in a background task with restart logic
            let workerWithRestartLoop :: Int -> Task Text Unit
                workerWithRestartLoop backoffMs = do
                  let emitCommand payload = do
                        Dispatcher.dispatchCommand commandEndpoints payload
                          |> Task.mapError (\err -> Integration.PermanentFailure err)
                  result <- Integration.runInbound inboundIntegration emitCommand
                    |> Task.mapError toText
                    |> Task.asResult
                  case result of
                    Err err -> do
                      Console.print [fmt|[Integration] Inbound worker error: #{err}. Restarting in #{backoffMs}ms...|]
                        |> Task.ignoreError
                      AsyncTask.sleep backoffMs
                      -- Exponential backoff: double the delay, max 60 seconds
                      let nextBackoff = min (backoffMs * 2) 60000
                      workerWithRestartLoop nextBackoff
                    Ok _ -> Task.yield unit
            -- Run worker in background, starting with 1 second backoff
            AsyncTask.run (workerWithRestartLoop 1000)
              |> Task.mapError toText
