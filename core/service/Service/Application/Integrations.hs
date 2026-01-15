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
import Default (Default (..), defaultValue)
import Integration qualified
import Integration.Lifecycle qualified as Lifecycle
import Json qualified
import Map (Map)
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event (Event (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error)
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
-- The integration function receives the current entity state and the new event,
-- and returns actions to execute. Currently, the entity is reconstructed with
-- default values - full event replay will be added in a future version.
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
  , TypeName.Inspectable entity
  , Default entity
  ) =>
  (entity -> event -> Integration.Outbound) ->
  OutboundRunner
createOutboundRunner integrationFn = do
  let typeName = TypeName.reflect @entity
  OutboundRunner
    { entityTypeName = typeName
    , processEvent = \rawEvent -> do
        let streamId = rawEvent.streamId
        -- Decode the event from JSON
        case Json.decode @event rawEvent.event of
          Err decodeErr -> do
            Console.print [fmt|[Integration] Failed to decode event for #{typeName} (stream: #{streamId}): #{decodeErr}|]
              |> Task.ignoreError
            Task.yield Array.empty
          Ok decodedEvent -> do
            -- Use default entity as placeholder - full entity reconstruction
            -- via event replay will be implemented in a future version.
            -- Integrations should use entityId from the event when needed.
            let entity = defaultValue @entity
            -- Call the integration function
            let outbound = integrationFn entity decodedEvent
            let actions = Integration.getActions outbound
            -- Execute all actions and collect command payloads
            actions
              |> Task.mapArray (\action -> do
                  result <- Integration.runAction action
                    |> Task.mapError toText
                  case result of
                    Just payload -> Task.yield [payload]
                    Nothing -> Task.yield []
                )
              |> Task.map Array.flatten
    }


-- | Register an outbound integration for an entity type.
--
-- See 'createOutboundRunner' for details.
withOutbound ::
  forall entity event.
  ( Json.FromJSON entity
  , Json.FromJSON event
  , TypeName.Inspectable entity
  , Default entity
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
startIntegrationSubscriber ::
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Task Text Unit
startIntegrationSubscriber (EventStore {subscribeToAllEvents}) runners lifecycleRunners commandEndpoints = do
  let hasRunners = not (Array.isEmpty runners)
  let hasLifecycleRunners = not (Array.isEmpty lifecycleRunners)

  if not hasRunners && not hasLifecycleRunners
    then Task.yield unit
    else do
      -- Create dispatcher with both runner types
      dispatcher <- Dispatcher.newWithLifecycle runners lifecycleRunners commandEndpoints

      let processIntegrationEvent :: Event Json.Value -> Task Text Unit
          processIntegrationEvent rawEvent = do
            result <- Dispatcher.dispatch dispatcher rawEvent |> Task.asResult
            case result of
              Ok _ -> Task.yield unit
              Err err ->
                Console.print [fmt|[Integration] Error dispatching event: #{err}|]
                  |> Task.ignoreError

      subscribeToAllEvents processIntegrationEvent
        |> Task.mapError (toText :: Error -> Text)
        |> Task.map (\_ -> unit)


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
