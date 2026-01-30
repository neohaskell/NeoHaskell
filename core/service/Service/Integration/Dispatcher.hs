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
  defaultConfig,

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
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import AtomicVar (AtomicVar)
import AtomicVar qualified
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Console qualified
import Control.Concurrent.Async qualified as GhcAsync
import Data.Either (Either (..))
import Data.Time.Clock.POSIX qualified as GhcPosix
import GHC.Float (Double)
import GHC.Real qualified as GhcReal
import Integration qualified
import Json qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Uuid (Uuid)
import Uuid qualified
import Result (Result (..))
import Service.Auth qualified as Auth
import Service.Event (Event (..))
import Service.Event.StreamId (StreamId)
import Service.EventStore.Core (EventStore)
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.Transport (EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified


-- | Run a task with an optional timeout.
--
-- If a timeout is specified and expires before the task completes,
-- returns @Err "timeout"@. Otherwise returns @Ok result@.
runWithTimeout ::
  forall err result.
  (Show err) =>
  Maybe Int -> -- ^ Timeout in milliseconds (Nothing = no timeout)
  Task err result ->
  Task Text (Result Text result)
runWithTimeout maybeTimeoutMs task = case maybeTimeoutMs of
  Nothing -> do
    -- No timeout, just run the task and wrap result
    result <- task |> Task.asResult
    case result of
      Ok value -> Task.yield (Ok value)
      Err err -> do
        let errText = show err |> Text.fromLinkedList
        Task.yield (Err errText)
  Just timeoutMs -> do
    -- Race the task against a sleep
    raceResult <- Task.fromIO do
      GhcAsync.race
        (Task.runResult task)
        (do
          -- Sleep returns Task _ Unit, use runResult and ignore result
          _ <- AsyncTask.sleep timeoutMs |> Task.runResult
          pure unit
        )
    case raceResult of
      Left taskResult -> case taskResult of
        Ok value -> Task.yield (Ok value)
        Err err -> do
          let errText = show err |> Text.fromLinkedList
          Task.yield (Err errText)
      Right _ -> Task.yield (Err "timeout")


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
    enableReaper :: Bool,
    -- | Capacity of worker channels (bounded). Events queue up to this limit
    -- before backpressure is applied. Default: 100
    -- Sizing rule of thumb: capacity = processing_rate * acceptable_latency
    workerChannelCapacity :: Int,
    -- | Timeout in milliseconds for writing to a full channel.
    -- If exceeded, event is logged as dropped. Default: 5000 (5 seconds)
    channelWriteTimeoutMs :: Int,
    -- | Timeout in milliseconds for processing a single event.
    -- If exceeded, the event processing is cancelled and the worker continues
    -- with the next event. Default: Nothing (no timeout).
    -- This prevents slow integrations from blocking a worker indefinitely.
    eventProcessingTimeoutMs :: Maybe Int
  }


-- | Default dispatcher configuration.
defaultConfig :: DispatcherConfig
defaultConfig =
  DispatcherConfig
    { idleTimeoutMs = 60000,
      reaperIntervalMs = 10000,
      enableReaper = True,
      workerChannelCapacity = 100,
      channelWriteTimeoutMs = 5000,
      eventProcessingTimeoutMs = Nothing
    }


-- | Worker for a specific entity (stateless).
data EntityWorker = EntityWorker
  { -- | Unique identifier for this worker instance.
    -- Used by the reaper to ensure it only removes the specific worker it checked.
    workerId :: Uuid,
    channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit,
    -- | Last activity time in milliseconds (for idle reaping).
    lastActivityTime :: ConcurrentVar Int
  }


-- | Status of a lifecycle worker for coordinating with the reaper.
--
-- This prevents a race condition where the reaper removes a worker from the map
-- while dispatch is simultaneously writing to that worker's channel.
data WorkerStatus
  = -- | Worker is active and accepting events
    Active
  | -- | Worker is draining its queue and will exit soon.
    -- New events should spawn a new worker instead of using this one.
    Draining
  deriving (Eq, Show)


-- | Worker for a specific entity with lifecycle management.
data LifecycleEntityWorker = LifecycleEntityWorker
  { -- | Unique identifier for this worker instance.
    -- Used by the reaper to ensure it only removes the specific worker it checked,
    -- not a newly-spawned replacement.
    workerId :: Uuid,
    channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit,
    lastActivityTime :: ConcurrentVar Int,
    workerStates :: Array WorkerState, -- One per lifecycle runner
    -- | Status for reaper coordination. When Draining, the dispatcher will
    -- spawn a new worker instead of using this one.
    -- Uses AtomicVar (TVar-based) so status can be read inside STM transactions
    -- for ConcurrentMap.getOrInsertIfM predicates.
    status :: AtomicVar WorkerStatus
  }


-- | Dispatcher that routes events to per-entity workers.
--
-- Uses ConcurrentMap for worker storage to enable fine-grained concurrent access.
-- This eliminates the MVar contention bottleneck that would otherwise serialize
-- all worker lookups at high throughput (50k+ events/second).
data IntegrationDispatcher = IntegrationDispatcher
  { entityWorkers :: ConcurrentMap StreamId EntityWorker,
    lifecycleEntityWorkers :: ConcurrentMap StreamId LifecycleEntityWorker,
    eventStore :: EventStore Json.Value,
    outboundRunners :: Array OutboundRunner,
    lifecycleRunners :: Array OutboundLifecycleRunner,
    commandEndpoints :: Map Text EndpointHandler,
    shutdownSignal :: ConcurrentVar Bool,
    config :: DispatcherConfig,
    reaperTask :: ConcurrentVar (Maybe (AsyncTask Text Unit)),
    context :: Integration.ActionContext
  }


-- | Create a new integration dispatcher (stateless runners only).
--
-- The dispatcher starts with no workers. Workers are created on-demand
-- when events arrive for new entities.
new ::
  EventStore Json.Value ->
  Array OutboundRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher
new store runners endpoints = do
  emptyStore <- InMemorySecretStore.new
  let emptyContext = Integration.ActionContext
        { Integration.secretStore = emptyStore
        , Integration.providerRegistry = Map.empty
        , Integration.fileAccess = Nothing
        }
  newWithLifecycleConfig defaultConfig store runners [] endpoints emptyContext


-- | Create a new integration dispatcher with lifecycle runners.
--
-- Supports both stateless and lifecycle-managed runners.
newWithLifecycle ::
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Task Text IntegrationDispatcher
newWithLifecycle store runners lifecycleRunners endpoints = do
  emptyStore <- InMemorySecretStore.new
  let emptyContext = Integration.ActionContext
        { Integration.secretStore = emptyStore
        , Integration.providerRegistry = Map.empty
        , Integration.fileAccess = Nothing
        }
  newWithLifecycleConfig defaultConfig store runners lifecycleRunners endpoints emptyContext


-- | Create a new integration dispatcher with custom configuration.
newWithLifecycleConfig ::
  DispatcherConfig ->
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text EndpointHandler ->
  Integration.ActionContext ->
  Task Text IntegrationDispatcher
newWithLifecycleConfig dispatcherConfig store runners lifecycleRunners endpoints actionContext = do
  workers <- ConcurrentMap.new
  lifecycleWorkers <- ConcurrentMap.new
  shutdownSignal <- ConcurrentVar.containing False
  reaperTaskVar <- ConcurrentVar.containing Nothing

  let dispatcher =
        IntegrationDispatcher
          { entityWorkers = workers,
            lifecycleEntityWorkers = lifecycleWorkers,
            eventStore = store,
            outboundRunners = runners,
            lifecycleRunners = lifecycleRunners,
            commandEndpoints = endpoints,
            shutdownSignal = shutdownSignal,
            config = dispatcherConfig,
            reaperTask = reaperTaskVar,
            context = actionContext
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
--
-- Uses atomic get-or-create to prevent race conditions when multiple events
-- for the same entity arrive simultaneously.
dispatch ::
  IntegrationDispatcher ->
  Event Json.Value ->
  Task Text Unit
dispatch dispatcher event = do
  -- Check shutdown signal first - reject new events during shutdown
  isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
  if isShuttingDown
    then do
      let streamId = event.streamId
      Console.print [fmt|[Dispatcher] Rejected event for stream #{streamId} - dispatcher is shutting down|]
        |> Task.ignoreError
    else do
      let streamId = event.streamId

      -- Handle stateless workers
      if Array.isEmpty dispatcher.outboundRunners
        then pass
        else do
          worker <- getOrCreateStatelessWorker dispatcher streamId
          writeWorkerMessageWithTimeout dispatcher streamId (ProcessEvent event) worker.channel

      -- Handle lifecycle workers
      if Array.isEmpty dispatcher.lifecycleRunners
        then pass
        else do
          lifecycleWorker <- getOrCreateLifecycleWorker dispatcher streamId
          -- Update last activity time
          currentTime <- getCurrentTimeMs
          _ <- lifecycleWorker.lastActivityTime |> ConcurrentVar.swap currentTime
          writeWorkerMessageWithTimeout dispatcher streamId (ProcessEvent event) lifecycleWorker.channel


writeWorkerMessageWithTimeout ::
  IntegrationDispatcher ->
  StreamId ->
  WorkerMessage (Event Json.Value) ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
writeWorkerMessageWithTimeout dispatcher streamId message channel = do
  writeResult <-
    Channel.tryWriteWithTimeout dispatcher.config.channelWriteTimeoutMs message channel
  case writeResult of
    Ok _ -> pass
    Err err -> do
      let messageLabel = workerMessageLabel message
      Console.print
        [fmt|[Dispatcher] Dropped #{messageLabel} for stream #{streamId} (channel write timeout: #{err})|]
        |> Task.ignoreError


workerMessageLabel ::
  forall value.
  WorkerMessage value ->
  Text
workerMessageLabel message = case message of
  ProcessEvent _ -> [fmt|event|]
  Stop -> [fmt|stop|]


-- | Stop a stateless worker gracefully or force-cancel if channel is full.
--
-- This ensures workers are always cleaned up even if their channel is full.
-- First attempts to send Stop via the channel. If that times out (channel full),
-- falls back to AsyncTask.cancel to forcefully terminate the worker.
stopStatelessWorkerOrCancel ::
  IntegrationDispatcher ->
  StreamId ->
  EntityWorker ->
  Task Text Unit
stopStatelessWorkerOrCancel dispatcher streamId worker = do
  writeResult <-
    Channel.tryWriteWithTimeout dispatcher.config.channelWriteTimeoutMs Stop worker.channel
  case writeResult of
    Ok _ -> pass
    Err err -> do
      Console.print
        [fmt|[Dispatcher] Stop message timed out for stream #{streamId} (#{err}), force-cancelling worker|]
        |> Task.ignoreError
      -- Force-cancel the worker since we can't stop it gracefully
      AsyncTask.cancel worker.workerTask


-- | Stop a lifecycle worker gracefully or force-cancel if channel is full.
--
-- Similar to stopStatelessWorkerOrCancel but also attempts cleanup.
-- If Stop can't be delivered, we force-cancel but cleanup may be skipped.
stopLifecycleWorkerOrCancel ::
  IntegrationDispatcher ->
  StreamId ->
  LifecycleEntityWorker ->
  Task Text Unit
stopLifecycleWorkerOrCancel dispatcher streamId worker = do
  writeResult <-
    Channel.tryWriteWithTimeout dispatcher.config.channelWriteTimeoutMs Stop worker.channel
  case writeResult of
    Ok _ -> pass
    Err err -> do
      Console.print
        [fmt|[Dispatcher] Stop message timed out for lifecycle worker stream #{streamId} (#{err}), force-cancelling (cleanup may be skipped)|]
        |> Task.ignoreError
      -- Force-cancel - note that cleanup callbacks won't be called in this case
      -- This is a trade-off: we prevent memory leaks but may leak external resources
      AsyncTask.cancel worker.workerTask


-- | Get an existing stateless worker or create a new one atomically.
--
-- This prevents the race condition where two threads could both see no worker
-- exists and both create one, causing one to be orphaned (memory leak) and
-- events to be lost.
--
-- Implementation uses optimistic spawning with ConcurrentMap.getOrInsert:
-- 1. Pre-spawn a worker outside the STM transaction (allows effects)
-- 2. Atomically check-and-insert using STM
-- 3. If another thread won the race, clean up the discarded candidate
--
-- Using ConcurrentMap instead of ConcurrentVar (MVar) eliminates the global
-- lock bottleneck, allowing concurrent access to different entity keys.
getOrCreateStatelessWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text EntityWorker
getOrCreateStatelessWorker dispatcher streamId = do
  -- First, optimistically spawn a new worker (outside STM transaction)
  -- This may be discarded if another thread wins the race
  candidateWorker <- spawnStatelessWorker dispatcher streamId

  -- Atomically check if a worker exists, insert ours if not
  (actualWorker, maybeDiscarded) <-
    dispatcher.entityWorkers
      |> ConcurrentMap.getOrInsert streamId candidateWorker

  -- Clean up discarded candidate if we lost the race
  case maybeDiscarded of
    Just discarded -> stopStatelessWorkerOrCancel dispatcher streamId discarded
    Nothing -> pass

  Task.yield actualWorker


-- | Get an existing lifecycle worker or create a new one atomically.
--
-- This function handles the reaper race condition by checking worker status:
-- - If existing worker is Active, return it
-- - If existing worker is Draining, spawn new worker to replace it
-- - If no worker exists, spawn new one
--
-- The two-phase approach (check status, then replace if needed) is safe because:
-- 1. If status is Active, we use existing worker (correct)
-- 2. If status is Draining, we atomically replace it (getOrInsertIf)
-- 3. Concurrent replacements are serialized by STM
getOrCreateLifecycleWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text LifecycleEntityWorker
getOrCreateLifecycleWorker dispatcher streamId = do
  -- First, check if there's an existing worker and its status
  maybeExisting <- dispatcher.lifecycleEntityWorkers |> ConcurrentMap.get streamId

  case maybeExisting of
    Just existingWorker -> do
      -- Check if the existing worker is being drained by the reaper
      existingStatus <- existingWorker.status |> AtomicVar.peek
      case existingStatus of
        Active ->
          -- Worker is active, use it
          Task.yield existingWorker
        Draining -> do
          -- Worker is draining, spawn new one to replace it
          candidateWorker <- spawnLifecycleWorker dispatcher streamId
          -- Atomically replace the draining worker using getOrInsertIfM
          -- The predicate reads status inside STM, allowing replacement only if still Draining
          let shouldReplace existing = do
                s <- AtomicVar.peekSTM existing.status
                pure (s == Draining)
          (actualWorker, maybeDiscarded) <-
            dispatcher.lifecycleEntityWorkers
              |> ConcurrentMap.getOrInsertIfM streamId candidateWorker shouldReplace
          -- Clean up discarded worker (either our candidate or the old draining one)
          case maybeDiscarded of
            Just discarded -> stopLifecycleWorkerOrCancel dispatcher streamId discarded
            Nothing -> pass
          Task.yield actualWorker
    Nothing -> do
      -- No worker exists, spawn one
      candidateWorker <- spawnLifecycleWorker dispatcher streamId
      (actualWorker, maybeDiscarded) <-
        dispatcher.lifecycleEntityWorkers
          |> ConcurrentMap.getOrInsert streamId candidateWorker
      -- Clean up discarded candidate if we lost the race
      case maybeDiscarded of
        Just discarded -> stopLifecycleWorkerOrCancel dispatcher streamId discarded
        Nothing -> pass
      Task.yield actualWorker


-- | Get current time in milliseconds.
getCurrentTimeMs :: Task Text Int
getCurrentTimeMs = do
  posixTime <- GhcPosix.getPOSIXTime |> Task.fromIO
  let millis = round (GhcReal.realToFrac posixTime * 1000 :: Double)
  Task.yield millis


-- | Spawn a stateless worker for an entity.
--
-- Uses bounded channels to provide backpressure when the worker can't keep up
-- with event production. Channel capacity is configured via DispatcherConfig.
spawnStatelessWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text EntityWorker
spawnStatelessWorker dispatcher streamId = do
  -- Generate unique ID for this worker instance
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime

  let workerLoop :: Task Text Unit
      workerLoop = do
        message <- workerChannel |> Channel.read
        case message of
          Stop -> do
            -- Drain remaining events before exiting
            drainStatelessWorker dispatcher workerChannel
          ProcessEvent event -> do
            -- Update activity time
            newTime <- getCurrentTimeMs
            _ <- lastActivity |> ConcurrentVar.swap newTime
            -- Process with optional timeout
            processResult <- runWithTimeout
              dispatcher.config.eventProcessingTimeoutMs
              (processStatelessEvent dispatcher event)
            case processResult of
              Ok _ -> pass
              Err err -> do
                Console.print [fmt|[Dispatcher] Event processing failed for stream #{streamId}: #{err}|]
                  |> Task.ignoreError
            workerLoop

  -- Wrap worker loop in exception boundary
  let safeWorkerLoop :: Task Text Unit
      safeWorkerLoop = do
        result <- workerLoop |> Task.asResult
        case result of
          Ok _ -> pass
          Err err -> do
            -- Worker crashed - log and remove from map
            Console.print [fmt|[Dispatcher] Stateless worker crashed for stream #{streamId}: #{err}|]
              |> Task.ignoreError
            -- Remove from map so new events will spawn a new worker
            dispatcher.entityWorkers |> ConcurrentMap.remove streamId

  workerTask <- AsyncTask.run safeWorkerLoop
  Task.yield
    EntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity
      }


-- | Spawn a lifecycle worker for an entity.
--
-- Uses bounded channels to provide backpressure when the worker can't keep up
-- with event production. Channel capacity is configured via DispatcherConfig.
spawnLifecycleWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text LifecycleEntityWorker
spawnLifecycleWorker dispatcher streamId = do
  -- Generate unique ID for this worker instance
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime
  workerStatus <- AtomicVar.containing Active

  -- Initialize all lifecycle runners
  states <-
    dispatcher.lifecycleRunners
      |> Task.mapArray (\runner -> runner.spawnWorkerState streamId)

  let workerLoop :: Task Text Unit
      workerLoop = do
        message <- workerChannel |> Channel.read
        case message of
          Stop -> do
            -- Drain remaining events before cleanup
            drainLifecycleWorker states dispatcher.commandEndpoints workerChannel
            -- Cleanup on stop (reap or shutdown)
            states |> Task.forEach (\state -> state.cleanup |> Task.ignoreError)
          ProcessEvent event -> do
            -- Update activity time
            newTime <- getCurrentTimeMs
            lastActivity |> ConcurrentVar.modify (\_ -> newTime)
            -- Process event through all lifecycle runners with optional timeout
            processResult <- runWithTimeout
              dispatcher.config.eventProcessingTimeoutMs
              (processLifecycleEvent states dispatcher.commandEndpoints event)
            case processResult of
              Ok _ -> pass
              Err err -> do
                Console.print [fmt|[Dispatcher] Lifecycle event processing failed for stream #{streamId}: #{err}|]
                  |> Task.ignoreError
            workerLoop

  -- Wrap worker loop in exception boundary
  let safeWorkerLoop :: Task Text Unit
      safeWorkerLoop = do
        result <- workerLoop |> Task.asResult
        case result of
          Ok _ -> pass
          Err err -> do
            -- Worker crashed - log, attempt cleanup, and remove from map
            Console.print [fmt|[Dispatcher] Lifecycle worker crashed for stream #{streamId}: #{err}|]
              |> Task.ignoreError
            -- Attempt cleanup even on crash (best effort)
            states |> Task.forEach (\state -> state.cleanup |> Task.ignoreError)
            -- Remove from map so new events will spawn a new worker
            dispatcher.lifecycleEntityWorkers |> ConcurrentMap.remove streamId

  workerTask <- AsyncTask.run safeWorkerLoop
  Task.yield
    LifecycleEntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        workerStates = states,
        status = workerStatus
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
      result <- runner.processEvent dispatcher.context dispatcher.eventStore event |> Task.asResult
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


-- | Drain remaining events from a stateless worker's channel before exiting.
--
-- This ensures no events are lost when a worker is stopped (either by reaper
-- or shutdown). Events queued after the Stop message was sent but before
-- the worker received it will still be processed.
drainStatelessWorker ::
  IntegrationDispatcher ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
drainStatelessWorker dispatcher workerChannel = do
  maybeMessage <- workerChannel |> Channel.tryRead
  case maybeMessage of
    Nothing -> 
      -- Channel empty, done draining
      Task.yield unit
    Just message -> case message of
      Stop -> 
        -- Another Stop, ignore and continue draining
        drainStatelessWorker dispatcher workerChannel
      ProcessEvent event -> do
        -- Process the remaining event
        processStatelessEvent dispatcher event
        -- Continue draining
        drainStatelessWorker dispatcher workerChannel


-- | Drain remaining events from a lifecycle worker's channel before cleanup.
--
-- Similar to drainStatelessWorker but uses lifecycle event processing.
drainLifecycleWorker ::
  Array WorkerState ->
  Map Text EndpointHandler ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
drainLifecycleWorker states endpoints workerChannel = do
  maybeMessage <- workerChannel |> Channel.tryRead
  case maybeMessage of
    Nothing ->
      -- Channel empty, done draining
      Task.yield unit
    Just message -> case message of
      Stop ->
        -- Another Stop, ignore and continue draining
        drainLifecycleWorker states endpoints workerChannel
      ProcessEvent event -> do
        -- Process the remaining event
        processLifecycleEvent states endpoints event
        -- Continue draining
        drainLifecycleWorker states endpoints workerChannel


-- | Dispatch a command to the appropriate handler.
-- Uses an anonymous RequestContext since integrations are system-triggered.
dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit
dispatchCommand endpoints payload = do
  let cmdType = payload.commandType
  case Map.get cmdType endpoints of
    Just handler -> do
      -- Create anonymous context for system-triggered commands
      requestContext <- Auth.anonymousContext
      let cmdBytes = Json.encodeText payload.commandData |> Text.toBytes
      let responseCallback _ = Task.yield unit
      handler requestContext cmdBytes responseCallback
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
            
            -- Use chunked iteration to reduce STM contention.
            -- Process 50 workers per batch to allow other operations to interleave.
            let reaperChunkSize = 50
            
            -- Reap idle LIFECYCLE workers
            dispatcher.lifecycleEntityWorkers
              |> ConcurrentMap.forEachChunked reaperChunkSize \streamId worker -> do
                -- Find and clean up idle workers
                -- Reaper race condition fix:
                -- 1. Mark worker as Draining (so dispatcher knows not to use it)
                -- 2. Atomically remove from map ONLY IF it's still the same worker
                --    (uses removeIf to prevent removing a newly-spawned replacement)
                -- 3. Send Stop (worker drains queue then exits)
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                if idleTime > dispatcher.config.idleTimeoutMs
                  then do
                    -- Capture the worker ID we're trying to remove
                    let capturedWorkerId = worker.workerId
                    -- Step 1: Mark as Draining so dispatcher won't use this worker
                    worker.status |> AtomicVar.set Draining
                    -- Step 2: Atomically remove ONLY IF the worker ID matches
                    -- If a new worker was spawned between our check and remove,
                    -- it will have a different ID and won't be removed
                    let isSameWorker currentWorker =
                          currentWorker.workerId == capturedWorkerId
                    removed <- dispatcher.lifecycleEntityWorkers
                      |> ConcurrentMap.removeIf streamId isSameWorker
                    -- Step 3: Send Stop only if we actually removed the worker
                    case removed of
                      Just removedWorker ->
                        stopLifecycleWorkerOrCancel dispatcher streamId removedWorker
                      Nothing ->
                        -- Worker was replaced, don't send Stop to the new one
                        pass
                  else pass

            -- Reap idle STATELESS workers (same logic, simpler - no Draining status)
            dispatcher.entityWorkers
              |> ConcurrentMap.forEachChunked reaperChunkSize \streamId worker -> do
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                if idleTime > dispatcher.config.idleTimeoutMs
                  then do
                    -- Capture the worker ID we're trying to remove
                    let capturedWorkerId = worker.workerId
                    -- Atomically remove ONLY IF the worker ID matches
                    let isSameWorker currentWorker =
                          currentWorker.workerId == capturedWorkerId
                    removed <- dispatcher.entityWorkers
                      |> ConcurrentMap.removeIf streamId isSameWorker
                    -- Send Stop only if we actually removed the worker
                    case removed of
                      Just removedWorker ->
                        stopStatelessWorkerOrCancel dispatcher streamId removedWorker
                      Nothing ->
                        -- Worker was replaced, don't send Stop to the new one
                        pass
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
  _ <- dispatcher.shutdownSignal |> ConcurrentVar.swap True

  -- Send Stop to all stateless workers
  statelessWorkerEntries <- dispatcher.entityWorkers |> ConcurrentMap.entries
  statelessWorkerEntries |> Task.forEach \(streamId, worker) -> do
    stopStatelessWorkerOrCancel dispatcher streamId worker

  -- Send Stop to all lifecycle workers (triggers cleanup)
  lifecycleWorkerEntries <- dispatcher.lifecycleEntityWorkers |> ConcurrentMap.entries
  lifecycleWorkerEntries |> Task.forEach \(streamId, worker) -> do
    stopLifecycleWorkerOrCancel dispatcher streamId worker
