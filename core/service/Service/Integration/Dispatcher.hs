-- | # Integration Dispatcher
--
-- Routes events to per-entity workers to ensure sequential processing
-- within each entity while allowing parallel processing across entities.
--
-- == Architecture
--
-- @
-- Event from Global Subscription
--         |
--         v
--   IntegrationDispatcher
--         |
--         v
--   Route by StreamId
--         |
--    /----|----\\
--   v     v     v
-- Worker Worker Worker
--  (A)    (B)    (C)
-- @
--
-- Each entity gets its own worker with a dedicated Channel. Events for
-- the same entity are processed sequentially by that entity's worker.
-- Different entities process in parallel.
--
-- == Lifecycle Management
--
-- Workers with lifecycle support can hold expensive resources (DB connections,
-- interpreters, etc.). The dispatcher manages their lifecycle:
--
-- 1. **Initialize**: Called when worker spawns for an entity
-- 2. **Process**: Called for each event
-- 3. **Cleanup**: Called when worker is reaped (idle) or on shutdown
--
-- A background reaper task periodically checks for idle workers and cleans
-- them up to prevent resource leaks.
module Service.Integration.Dispatcher (
  -- * Types
  IntegrationDispatcher,
  DispatcherConfig (..),

  -- * Re-exports from Types
  OutboundRunner (..),
  OutboundLifecycleRunner (..),
  WorkerState (..),

  -- * Construction
  new,
  newWithLifecycle,
  newWithLifecycleConfig,

  -- * Operations
  dispatch,
  shutdown,
  dispatchCommand,
) where

import Array (Array)
import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console qualified
import Data.Time.Clock.POSIX qualified as GhcPosix
import GHC.Real qualified as GhcReal
import Integration qualified
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event (Event (..))
import Service.Event.StreamId (StreamId)
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.Transport (EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Message type for worker channels (poison pill pattern).
--
-- Workers block on Channel.read, so we can't interrupt them with external signals.
-- Instead, we send a Stop message to gracefully terminate them.
data WorkerMessage value
  = ProcessEvent value
  | Stop
  deriving (Show)


-- | Configuration for the dispatcher.
data DispatcherConfig = DispatcherConfig
  { -- | How long a worker can be idle before being reaped (milliseconds).
    -- Default: 60000 (60 seconds)
    idleTimeoutMs :: Int,
    -- | How often the reaper checks for idle workers (milliseconds).
    -- Default: 10000 (10 seconds)
    reaperIntervalMs :: Int,
    -- | Whether to enable the reaper. Set to False for testing.
    -- Default: True
    enableReaper :: Bool
  }


-- | Default dispatcher configuration.
defaultConfig :: DispatcherConfig
defaultConfig =
  DispatcherConfig
    { idleTimeoutMs = 60000,
      reaperIntervalMs = 10000,
      enableReaper = True
    }


-- | Worker for a specific entity (stateless).
data EntityWorker = EntityWorker
  { channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit
  }


-- | Worker for a specific entity with lifecycle management.
data LifecycleEntityWorker = LifecycleEntityWorker
  { channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit,
    lastActivityTime :: ConcurrentVar Int,
    workerStates :: Array WorkerState -- One per lifecycle runner
  }


-- | Dispatcher that routes events to per-entity workers.
data IntegrationDispatcher = IntegrationDispatcher
  { entityWorkers :: ConcurrentVar (Map StreamId EntityWorker),
    lifecycleEntityWorkers :: ConcurrentVar (Map StreamId LifecycleEntityWorker),
    outboundRunners :: Array OutboundRunner,
    lifecycleRunners :: Array OutboundLifecycleRunner,
    commandEndpoints :: Map Text EndpointHandler,
    shutdownSignal :: ConcurrentVar Bool,
    config :: DispatcherConfig,
    reaperTask :: ConcurrentVar (Maybe (AsyncTask Text Unit))
  }


-- | Create a new integration dispatcher (stateless runners only).
--
-- The dispatcher starts with no workers. Workers are created on-demand
-- when events arrive for new entities.
new ::
  Array OutboundRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher
new runners endpoints = newWithLifecycleConfig defaultConfig runners [] endpoints


-- | Create a new integration dispatcher with lifecycle runners.
--
-- Supports both stateless and lifecycle-managed runners.
newWithLifecycle ::
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher
newWithLifecycle runners lifecycleRunners endpoints =
  newWithLifecycleConfig defaultConfig runners lifecycleRunners endpoints


-- | Create a new integration dispatcher with custom configuration.
newWithLifecycleConfig ::
  DispatcherConfig ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher
newWithLifecycleConfig dispatcherConfig runners lifecycleRunners endpoints = do
  workers <- ConcurrentVar.containing Map.empty
  lifecycleWorkers <- ConcurrentVar.containing Map.empty
  shutdownSignal <- ConcurrentVar.containing False
  reaperTaskVar <- ConcurrentVar.containing Nothing

  let dispatcher =
        IntegrationDispatcher
          { entityWorkers = workers,
            lifecycleEntityWorkers = lifecycleWorkers,
            outboundRunners = runners,
            lifecycleRunners = lifecycleRunners,
            commandEndpoints = endpoints,
            shutdownSignal = shutdownSignal,
            config = dispatcherConfig,
            reaperTask = reaperTaskVar
          }

  -- Start reaper if we have lifecycle runners and reaper is enabled
  if Array.isEmpty lifecycleRunners || not dispatcherConfig.enableReaper
    then pass
    else do
      reaper <- startReaper dispatcher
      reaperTaskVar |> ConcurrentVar.modify (\_ -> Just reaper)

  Task.yield dispatcher


-- | Dispatch an event to the appropriate entity worker.
--
-- If no worker exists for this entity, one is created. Events are queued
-- to the worker's channel for sequential processing.
dispatch ::
  IntegrationDispatcher ->
  Event Json.Value ->
  Task Text Unit
dispatch dispatcher event = do
  let streamId = event.streamId

  -- Handle stateless workers
  if Array.isEmpty dispatcher.outboundRunners
    then pass
    else do
      workers <- dispatcher.entityWorkers |> ConcurrentVar.peek
      worker <- case Map.get streamId workers of
        Just existingWorker -> Task.yield existingWorker
        Nothing -> do
          newWorker <- spawnStatelessWorker dispatcher streamId
          dispatcher.entityWorkers
            |> ConcurrentVar.modify (Map.set streamId newWorker)
          Task.yield newWorker
      worker.channel |> Channel.write (ProcessEvent event)

  -- Handle lifecycle workers
  if Array.isEmpty dispatcher.lifecycleRunners
    then pass
    else do
      lifecycleWorkers <- dispatcher.lifecycleEntityWorkers |> ConcurrentVar.peek
      lifecycleWorker <- case Map.get streamId lifecycleWorkers of
        Just existingWorker -> Task.yield existingWorker
        Nothing -> do
          newWorker <- spawnLifecycleWorker dispatcher streamId
          dispatcher.lifecycleEntityWorkers
            |> ConcurrentVar.modify (Map.set streamId newWorker)
          Task.yield newWorker
      -- Update last activity time
      currentTime <- getCurrentTimeMs
      lifecycleWorker.lastActivityTime |> ConcurrentVar.set currentTime
      lifecycleWorker.channel |> Channel.write (ProcessEvent event)


-- | Get current time in milliseconds.
getCurrentTimeMs :: Task Text Int
getCurrentTimeMs = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let millis = round (GhcReal.realToFrac posixTime * 1000 :: Float)
  Task.yield millis


-- | Spawn a stateless worker for an entity.
spawnStatelessWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text EntityWorker
spawnStatelessWorker dispatcher _streamId = do
  workerChannel <- Channel.new

  let workerLoop :: Task Text Unit
      workerLoop = do
        message <- workerChannel |> Channel.read
        case message of
          Stop ->
            -- Exit gracefully
            Task.yield unit
          ProcessEvent event -> do
            processStatelessEvent dispatcher event
            workerLoop

  workerTask <- AsyncTask.run workerLoop
  Task.yield
    EntityWorker
      { channel = workerChannel,
        workerTask = workerTask
      }


-- | Spawn a lifecycle worker for an entity.
spawnLifecycleWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text LifecycleEntityWorker
spawnLifecycleWorker dispatcher streamId = do
  workerChannel <- Channel.new
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime

  -- Initialize all lifecycle runners
  states <-
    dispatcher.lifecycleRunners
      |> Task.mapArray (\runner -> runner.spawnWorkerState streamId)

  let workerLoop :: Task Text Unit
      workerLoop = do
        message <- workerChannel |> Channel.read
        case message of
          Stop -> do
            -- Cleanup on stop (reap or shutdown)
            states |> Task.forEach (\state -> state.cleanup |> Task.ignoreError)
          ProcessEvent event -> do
            -- Update activity time
            newTime <- getCurrentTimeMs
            lastActivity |> ConcurrentVar.set newTime
            -- Process event through all lifecycle runners
            processLifecycleEvent states dispatcher.commandEndpoints event
            workerLoop

  workerTask <- AsyncTask.run workerLoop
  Task.yield
    LifecycleEntityWorker
      { channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        workerStates = states
      }


-- | Process an event through stateless runners.
processStatelessEvent ::
  IntegrationDispatcher ->
  Event Json.Value ->
  Task Text Unit
processStatelessEvent dispatcher event = do
  let streamId = event.streamId
  dispatcher.outboundRunners
    |> Task.forEach \runner -> do
      result <- runner.processEvent event |> Task.asResult
      case result of
        Ok commands -> do
          commands
            |> Task.forEach \payload -> do
              dispatchCommand dispatcher.commandEndpoints payload
        Err err -> do
          Console.print [fmt|[Dispatcher] Error processing event (stream: #{streamId}): #{err}|]
            |> Task.ignoreError


-- | Process an event through lifecycle runners.
processLifecycleEvent ::
  Array WorkerState ->
  Map Text EndpointHandler ->
  Event Json.Value ->
  Task Text Unit
processLifecycleEvent states endpoints event = do
  let streamId = event.streamId
  states
    |> Task.forEach \state -> do
      result <- state.processEvent event |> Task.asResult
      case result of
        Ok commands -> do
          commands
            |> Task.forEach \payload -> do
              dispatchCommand endpoints payload
        Err err -> do
          Console.print [fmt|[Dispatcher] Error processing lifecycle event (stream: #{streamId}): #{err}|]
            |> Task.ignoreError


-- | Dispatch a command to the appropriate handler.
dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit
dispatchCommand endpoints payload = do
  let cmdType = payload.commandType
  case Map.get cmdType endpoints of
    Just handler -> do
      let cmdBytes = Json.encodeText payload.commandData |> Text.toBytes
      let responseCallback _ = Task.yield unit
      handler cmdBytes responseCallback
        |> Task.mapError (\err -> [fmt|Command dispatch failed for #{cmdType}: #{err}|])
    Nothing -> do
      Console.print [fmt|[Integration] No handler found for command: #{cmdType}|]
        |> Task.ignoreError


-- | Start the reaper task that cleans up idle lifecycle workers.
startReaper ::
  IntegrationDispatcher ->
  Task Text (AsyncTask Text Unit)
startReaper dispatcher = do
  let reaperLoop :: Task Text Unit
      reaperLoop = do
        -- Sleep for the reaper interval
        AsyncTask.sleep dispatcher.config.reaperIntervalMs
          |> Task.mapError (\_ -> "reaper sleep error" :: Text)

        -- Check shutdown
        shouldShutdown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
        if shouldShutdown
          then Task.yield unit
          else do
            -- Check for idle workers
            currentTime <- getCurrentTimeMs
            workers <- dispatcher.lifecycleEntityWorkers |> ConcurrentVar.peek

            -- Find and clean up idle workers
            let entries = Map.entries workers
            entries |> Task.forEach \(streamId, worker) -> do
              lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
              let idleTime = currentTime - lastActivity
              if idleTime > dispatcher.config.idleTimeoutMs
                then do
                  -- Send Stop message to trigger cleanup and exit
                  worker.channel |> Channel.write Stop
                  -- Remove from map so a new worker is spawned next time
                  dispatcher.lifecycleEntityWorkers
                    |> ConcurrentVar.modify (Map.remove streamId)
                else pass

            -- Continue reaping
            reaperLoop

  AsyncTask.run reaperLoop


-- | Gracefully shutdown the dispatcher.
--
-- Signals all workers to stop by sending Stop messages.
-- Note: This is an async operation - workers may not have stopped when this returns.
-- For production use, the application framework handles proper shutdown coordination.
shutdown ::
  IntegrationDispatcher ->
  Task Text Unit
shutdown dispatcher = do
  -- Signal shutdown (stops the reaper on next iteration)
  dispatcher.shutdownSignal |> ConcurrentVar.set True

  -- Send Stop to all stateless workers
  statelessWorkers <- dispatcher.entityWorkers |> ConcurrentVar.peek
  Map.entries statelessWorkers |> Task.forEach \(_streamId, worker) -> do
    worker.channel |> Channel.write Stop

  -- Send Stop to all lifecycle workers (triggers cleanup)
  lifecycleWorkers <- dispatcher.lifecycleEntityWorkers |> ConcurrentVar.peek
  Map.entries lifecycleWorkers |> Task.forEach \(_streamId, worker) -> do
    worker.channel |> Channel.write Stop
