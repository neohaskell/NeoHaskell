{-# LANGUAGE AllowAmbiguousTypes #-}

-- | # Service.Application.Integrations
--
-- Integration configuration and runtime for the Application.
--
-- This module provides:
--
-- * 'withOutboundLifecycle' - Register stateful integrations with resource management
-- * 'withInbound' - Register inbound integrations (external -> commands)
-- * Runtime functions for starting integration workers
module Service.Application.Integrations (
  -- * Configuration
  withOutboundLifecycle,
  withInbound,
  -- * Runner Construction
  createTypedOutboundRunner,
  createOutboundLifecycleRunner,
  -- * Runtime (internal)
  startIntegrationSubscriber,
  startInboundWorkers,
) where

import Array (Array)
import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import Basics
import Log qualified
import Default (Default (..))
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Map (Map)
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Entity.Core (Entity (..), EntityOf, EventOf)
import Service.Event (Event (..), EntityName (..))
import Service.Event.StreamId (StreamId (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Integration.Dispatcher qualified as Dispatcher
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.OutboundIntegration.Core (OutboundIntegration (..))
import Service.Transport (EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import ToText (toText)
import TypeName qualified


-- | Create an OutboundRunner from a typed OutboundIntegration handler.
--
-- This wraps the typed handler with:
-- 1. JSON decoding of the full event ADT
-- 2. Entity state reconstruction via event replay
-- 3. Dispatch to 'handleEventImpl'
--
-- The handler's entity type and event type are determined by the type
-- constraints. The event is decoded from JSON, the entity is reconstructed
-- via event replay, and handleEventImpl is called.
createTypedOutboundRunner ::
  forall handler entity event.
  ( OutboundIntegration handler
  , entity ~ EntityOf handler
  , event ~ EventOf entity
  , event ~ HandledEvent handler
  , Json.FromJSON entity
  , Json.FromJSON event
  , TypeName.Inspectable entity
  , Default entity
  , Entity entity
  ) =>
  OutboundRunner
createTypedOutboundRunner = do
  let typeName = TypeName.reflect @entity
  OutboundRunner
    { entityTypeName = typeName
    , processEvent = \ctx eventStore rawEvent -> do
        let streamId = rawEvent.streamId
        case Json.decode @event rawEvent.event of
          Err decodeErr -> do
            Log.withScope [("component", "Integration"), ("entityName", typeName), ("streamId", streamId |> toText)] do
              Log.warn [fmt|Failed to decode event: #{decodeErr}|]
                |> Task.ignoreError
            Task.yield Array.empty
          Ok decodedEvent -> do
            entity <- fetchEntityState @entity eventStore typeName streamId
            let outbound = handleEventImpl @handler entity decodedEvent
            let actions = Integration.getActions outbound
            actions
              |> Task.mapArray (\action -> do
                  result <- Integration.runAction ctx action |> Task.mapError toText
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
  let initialState = initialStateImpl @entity
  let updateFn = updateImpl @entity
  let decodingReducer :: Json.Value -> (entity, Array Text) -> (entity, Array Text)
      decodingReducer jsonValue (state, errors) = do
        case Json.decode @(EventOf entity) jsonValue of
          Err decodeErr -> do
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
      Task.yield initialState
    EntityFetcher.EntityFound fetched -> do
      let (entityState, decodeErrors) = fetched.state
      case Array.first decodeErrors of
        Just firstError -> do
          let errorCount = Array.length decodeErrors
          Task.throw [fmt|[Integration] #{errorCount} event(s) failed to decode for #{typeName} (stream: #{streamId}). First error: #{firstError}|]
        Nothing -> do
          Log.withScope [("component", "Integration"), ("entityName", typeName), ("streamId", toText streamId)] do
            Log.debug [fmt|Entity state fetched|]
              |> Task.ignoreError
          Task.yield entityState


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
        state <- config.initialize streamId
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
  Maybe Dispatcher.DispatcherConfig ->
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Integration.ActionContext ->
  Task Text (Maybe Dispatcher.IntegrationDispatcher)
startIntegrationSubscriber maybeDispatcherConfig eventStore runners lifecycleRunners commandEndpoints ctx = do
  let hasRunners = not (Array.isEmpty runners)
  let hasLifecycleRunners = not (Array.isEmpty lifecycleRunners)

  if not hasRunners && not hasLifecycleRunners
    then Task.yield Nothing
    else do
      let config = Maybe.withDefault Dispatcher.defaultConfig maybeDispatcherConfig
      dispatcher <- Dispatcher.newWithLifecycleConfig config eventStore runners lifecycleRunners commandEndpoints ctx

      let processIntegrationEvent :: Event Json.Value -> Task Text Unit
          processIntegrationEvent rawEvent = do
            result <- Dispatcher.dispatch dispatcher rawEvent |> Task.asResult
            case result of
              Ok _ -> Task.yield unit
              Err err ->
                Log.withScope [("component", "Integration"), ("entityName", toText rawEvent.entityName), ("streamId", toText rawEvent.streamId)] do
                  Log.warn [fmt|Error dispatching event: #{err}|]
                    |> Task.ignoreError

      _ <- eventStore.subscribeToAllEvents processIntegrationEvent
        |> Task.mapError (toText :: Error -> Text)

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
      Log.withScope [("component", "Integration")] do
        Log.info [fmt|Starting #{workerCount} inbound worker(s)|]
          |> Task.ignoreError
      inbounds
        |> Task.mapArray \inboundIntegration -> do
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
                      Log.withScope [("component", "Integration")] do
                        Log.warn [fmt|Inbound worker error: #{err}. Restarting in #{backoffMs}ms...|]
                          |> Task.ignoreError
                      AsyncTask.sleep backoffMs
                      let nextBackoff = min (backoffMs * 2) 60000
                      workerWithRestartLoop nextBackoff
                    Ok _ -> Task.yield unit
            AsyncTask.run (workerWithRestartLoop 1000)
              |> Task.mapError toText
