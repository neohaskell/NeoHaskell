
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
  Setup (..),

  -- * Re-exports from Types
  OutboundRunner (..),
  OutboundLifecycleRunner (..),
  WorkerState (..),

  -- * Construction
  new,
  newWithLifecycle,
  newWithLifecycleConfig,
  newWithLifecycleConfigWithSource,

  -- * Operations
  dispatch,
  shutdown,
  dispatchCommand,
  dispatchCommandWithSource,
) where

import Array (Array)
import Array qualified
import AsyncTask (AsyncTask)
import AsyncTask qualified
import Auth.SecretStore.InMemory qualified as InMemorySecretStore
import Basics
import Channel (Channel)
import Channel qualified
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import DateTime qualified
import Log qualified
import Service.Response (CommandResponse (..))
import Float qualified
import Int qualified
import Integration qualified
import Json qualified
import LinkedList qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Uuid (Uuid)
import Uuid qualified
import Result (Result (..))
import Service.Auth qualified as Auth
import Service.Event (Event (..))
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.EntityName qualified as EntityName
import Service.Event.StreamId (StreamId)
import Service.EventStore.Core (EventStore)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.Dispatcher.Config (DispatcherConfig (..), defaultConfig)
import Service.Integration.Dispatcher.Setup (Setup (..))
import Service.Integration.Dispatcher.Setup qualified as Setup
import Service.Integration.Dispatcher.EventLogContext (EventLogContext)
import Service.Integration.Dispatcher.EventLogContext qualified as EventLogContext
import Service.Integration.Dispatcher.IntegrationResult (IntegrationResult (..), RunnerKind (..))
import Service.Integration.Dispatcher.RunnerExecution qualified as RunnerExecution
import Service.Integration.Dispatcher.WorkerStopKind (WorkerStopKind (..))
import Service.Integration.Dispatcher.WorkerStopKind qualified as WorkerStopKind
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.Integration.Dispatcher.WorkerQueueControl (WorkerQueueControl)
import Service.Integration.Dispatcher.WorkerQueueControl qualified as WorkerQueueControl
import Service.Transport (DispatchHandler, EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


data ProcessingTimeoutFailure
  = ProcessingFailed
  | ProcessingTimedOut


runWithTimeout ::
  forall err result.
  ConcurrentVar Bool ->
  Maybe Int ->
  Task err result ->
  Task Text (Result ProcessingTimeoutFailure result)
runWithTimeout timeoutReached maybeTimeoutMs task = case maybeTimeoutMs of
  Nothing -> do
    result <- task |> Task.asResult
    case result of
      Ok value -> Task.yield (Ok value)
      Err _ -> Task.yield (Err ProcessingFailed)
  Just timeoutMs -> do
    raceResult <- AsyncTask.race
      (Task.asResult task)
      (do
        AsyncTask.sleep timeoutMs
        timeoutReached |> ConcurrentVar.modify (\_ -> True)
      )
    case raceResult of
      AsyncTask.LeftWon taskResult -> case taskResult of
        Ok value -> Task.yield (Ok value)
        Err _ -> Task.yield (Err ProcessingFailed)
      AsyncTask.RightWon _ -> Task.yield (Err ProcessingTimedOut)


-- | Message type for worker channels (poison pill pattern).
--
-- Workers block on Channel.read, so we can't interrupt them with external signals.
-- Instead, we send a Stop message to gracefully terminate them.
data WorkerMessage value
  = ProcessEvent value
  | Stop
  deriving (Show)


data LifecycleRunnerWorkerState = LifecycleRunnerWorkerState
  { lifecycleRunnerOrdinal :: Int,
    lifecycleRunnerEntityTypeName :: Text,
    lifecycleRunnerState :: WorkerState
  }


-- | Worker for a specific entity (stateless).
data EntityWorker = EntityWorker
  { -- | Unique identifier for this worker instance.
    -- Used by the reaper to ensure it only removes the specific worker it checked.
    workerId :: Uuid,
    channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit,
    -- | Last activity time in milliseconds (for idle reaping).
    lastActivityTime :: ConcurrentVar Int,
    queueStatus :: WorkerQueueControl
  }


-- | Worker for a specific entity with lifecycle management.
data LifecycleEntityWorker = LifecycleEntityWorker
  { -- | Unique identifier for this worker instance.
    -- Used by the reaper to ensure it only removes the specific worker it checked,
    -- not a newly-spawned replacement.
    workerId :: Uuid,
    channel :: Channel (WorkerMessage (Event Json.Value)),
    workerTask :: AsyncTask Text Unit,
    lastActivityTime :: ConcurrentVar Int,
    workerStates :: Array LifecycleRunnerWorkerState,
    queueStatus :: WorkerQueueControl
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
    commandEndpoints :: Map Text DispatchHandler,
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
  emptyRefreshLocks <- ConcurrentMap.new
  let emptyContext = Integration.ActionContext
        { Integration.secretStore = emptyStore
        , Integration.providerRegistry = Integration.fromMap Map.empty
        , Integration.refreshLocks = emptyRefreshLocks
        , Integration.fileAccess = Nothing
        , Integration.outboundDispatch = DispatchRegistry.empty
        , Integration.eraseErasableKey = Nothing
        }
  newWithLifecycleConfig
    Setup
      { settings = defaultConfig,
        eventStore = store,
        outboundRunners = runners,
        lifecycleRunners = [],
        commandEndpoints = endpoints,
        actionContext = emptyContext
      }


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
  emptyRefreshLocks <- ConcurrentMap.new
  let emptyContext = Integration.ActionContext
        { Integration.secretStore = emptyStore
        , Integration.providerRegistry = Integration.fromMap Map.empty
        , Integration.refreshLocks = emptyRefreshLocks
        , Integration.fileAccess = Nothing
        , Integration.outboundDispatch = DispatchRegistry.empty
        , Integration.eraseErasableKey = Nothing
        }
  newWithLifecycleConfig
    Setup
      { settings = defaultConfig,
        eventStore = store,
        outboundRunners = runners,
        lifecycleRunners,
        commandEndpoints = endpoints,
        actionContext = emptyContext
      }


-- | Create a new integration dispatcher with custom configuration.
newWithLifecycleConfig ::
  Setup EndpointHandler ->
  Task Text IntegrationDispatcher
newWithLifecycleConfig setup =
  newWithLifecycleConfigWithSource
    setup
      { Setup.commandEndpoints = setup.commandEndpoints |> Map.mapValues (\handler _sourceEventMetadata -> handler)
      }


newWithLifecycleConfigWithSource ::
  Setup DispatchHandler ->
  Task Text IntegrationDispatcher
newWithLifecycleConfigWithSource setup = do
  workers <- ConcurrentMap.new
  lifecycleWorkers <- ConcurrentMap.new
  shutdownSignal <- ConcurrentVar.containing False
  reaperTaskVar <- ConcurrentVar.containing Nothing

  let dispatcher =
        IntegrationDispatcher
          { entityWorkers = workers,
            lifecycleEntityWorkers = lifecycleWorkers,
            eventStore = setup.eventStore,
            outboundRunners = setup.outboundRunners,
            lifecycleRunners = setup.lifecycleRunners,
            commandEndpoints = setup.commandEndpoints,
            shutdownSignal = shutdownSignal,
            config = setup.settings,
            reaperTask = reaperTaskVar,
            context = setup.actionContext
          }

  Task.unless (not setup.settings.enableReaper) do
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
dispatch dispatcher event =
   Log.withScope [("component", "Dispatcher")] do
      -- Check shutdown signal first - reject new events during shutdown
      isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
      if isShuttingDown
        then do
          let logContext = EventLogContext.fromEvent event
          EventLogContext.logResult logContext Dropped
        else do
          let streamId = event.streamId
          let eventEntityName = event.entityName |> EntityName.toText
          Log.debug [fmt|Routing event to workers for stream #{toText streamId}|]
            |> Task.ignoreError

          let matchingStatelessRunners =
                dispatcher.outboundRunners
                  |> Array.takeIf (\runner -> runner.entityTypeName == eventEntityName)
          let matchingLifecycleRunners =
                dispatcher.lifecycleRunners
                  |> Array.takeIf (\runner -> runner.entityTypeName == eventEntityName)
          if Array.isEmpty matchingStatelessRunners && Array.isEmpty matchingLifecycleRunners
            then do
              let logContext = EventLogContext.fromEvent event
              EventLogContext.logResult logContext Skipped
            else do
              didQueueStateless <- if Array.isEmpty matchingStatelessRunners
                then Task.yield True
                else queueStatelessEvent dispatcher streamId event

              didQueueLifecycle <- if Array.isEmpty matchingLifecycleRunners
                then Task.yield True
                else queueLifecycleEvent dispatcher streamId eventEntityName event

              Task.when (not didQueueStateless || not didQueueLifecycle) do
                let logContext = EventLogContext.fromEvent event
                EventLogContext.logResult logContext Dropped


data WorkerEnqueueResult
  = WorkerEventEnqueued
  | WorkerEventWriteTimedOut
  | WorkerQueueClosed


data WorkerQueueTarget = WorkerQueueTarget
  { queueControl :: WorkerQueueControl,
    queueChannel :: Channel (WorkerMessage (Event Json.Value))
  }


queueWorkerEventWithTimeout ::
  IntegrationDispatcher ->
  StreamId ->
  Event Json.Value ->
  WorkerQueueTarget ->
  Task Text WorkerEnqueueResult
queueWorkerEventWithTimeout dispatcher streamId event target = do
  isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
  if isShuttingDown
    then Task.yield WorkerQueueClosed
    else do
      maybeWriteResult <- WorkerQueueControl.withAdmission target.queueControl do
        Channel.tryWriteWithTimeout
          dispatcher.config.channelWriteTimeoutMs
          (ProcessEvent event)
          target.queueChannel
      case maybeWriteResult of
        Nothing -> Task.yield WorkerQueueClosed
        Just writeResult -> case writeResult of
          Ok _ -> Task.yield WorkerEventEnqueued
          Err _ -> do
            Log.warn [fmt|Dropped event for stream #{toText streamId} (channel write timeout)|]
              |> Task.ignoreError
            Task.yield WorkerEventWriteTimedOut


queueStatelessEvent ::
  IntegrationDispatcher ->
  StreamId ->
  Event Json.Value ->
  Task Text Bool
queueStatelessEvent dispatcher streamId event = do
  worker <- getOrCreateStatelessWorker dispatcher streamId
  enqueueResult <-
    queueWorkerEventWithTimeout dispatcher streamId event
      WorkerQueueTarget
        { queueControl = worker.queueStatus,
          queueChannel = worker.channel
        }
  case enqueueResult of
    WorkerEventEnqueued -> Task.yield True
    WorkerEventWriteTimedOut -> Task.yield False
    WorkerQueueClosed -> do
      isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
      if isShuttingDown
        then do
          withWorkerCleanupHandoff
            ( do
                _ <- dispatcher.entityWorkers
                  |> ConcurrentMap.removeIf streamId (\currentWorker -> currentWorker.workerId == worker.workerId)
                Task.yield (Just worker)
            )
            (stopStatelessWorkerOrCancel dispatcher streamId)
            (stopStatelessWorkerOrCancel dispatcher streamId)
          Task.yield False
        else queueStatelessEvent dispatcher streamId event


queueLifecycleEvent ::
  IntegrationDispatcher ->
  StreamId ->
  Text ->
  Event Json.Value ->
  Task Text Bool
queueLifecycleEvent dispatcher streamId entityTypeName event = do
  worker <- getOrCreateLifecycleWorker dispatcher streamId entityTypeName
  currentTime <- getCurrentTimeMs
  _ <- worker.lastActivityTime |> ConcurrentVar.swap currentTime
  enqueueResult <-
    queueWorkerEventWithTimeout dispatcher streamId event
      WorkerQueueTarget
        { queueControl = worker.queueStatus,
          queueChannel = worker.channel
        }
  case enqueueResult of
    WorkerEventEnqueued -> Task.yield True
    WorkerEventWriteTimedOut -> Task.yield False
    WorkerQueueClosed -> do
      isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
      if isShuttingDown
        then do
          withWorkerCleanupHandoff
            ( do
                _ <- dispatcher.lifecycleEntityWorkers
                  |> ConcurrentMap.removeIf streamId (\currentWorker -> currentWorker.workerId == worker.workerId)
                Task.yield (Just worker)
            )
            (stopLifecycleWorkerOrCancel dispatcher streamId)
            (stopLifecycleWorkerOrCancel dispatcher streamId)
          Task.yield False
        else queueLifecycleEvent dispatcher streamId entityTypeName event


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
  stopWorkerOrCancel
    WorkerStopConfig
      { stopStreamId = streamId,
        stopKind = StatelessWorker,
        stopWriteTimeoutMs = dispatcher.config.channelWriteTimeoutMs
      }
    WorkerStopData
      { stopQueueStatus = worker.queueStatus,
        stopTask = worker.workerTask,
        stopChannel = worker.channel
      }


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
  stopWorkerOrCancel
    WorkerStopConfig
      { stopStreamId = streamId,
        stopKind = LifecycleWorker,
        stopWriteTimeoutMs = dispatcher.config.channelWriteTimeoutMs
      }
    WorkerStopData
      { stopQueueStatus = worker.queueStatus,
        stopTask = worker.workerTask,
        stopChannel = worker.channel
      }


data WorkerStopConfig = WorkerStopConfig
  { stopStreamId :: StreamId,
    stopKind :: WorkerStopKind,
    stopWriteTimeoutMs :: Int
  }


data WorkerStopData = WorkerStopData
  { stopQueueStatus :: WorkerQueueControl,
    stopTask :: AsyncTask Text Unit,
    stopChannel :: Channel (WorkerMessage (Event Json.Value))
  }


stopWorkerOrCancel ::
  WorkerStopConfig ->
  WorkerStopData ->
  Task Text Unit
stopWorkerOrCancel config worker = Task.withCancellationDeferred \restore -> do
  claimed <- WorkerQueueControl.claimStop worker.stopQueueStatus
  Task.when claimed do
    let forceStop = forceWorkerStop worker.stopQueueStatus worker.stopTask worker.stopChannel
    let attemptStop = do
          restore (WorkerQueueControl.waitForWriters worker.stopQueueStatus)
          writeResult <-
            restore
              ( Channel.tryWriteWithTimeout
                  config.stopWriteTimeoutMs
                  Stop
                  worker.stopChannel
              )
          case writeResult of
            Ok _ -> Task.yield True
            Err _ -> Task.yield False
    stopResult <- Task.onFailure (\_ -> forceStop) attemptStop
    if stopResult
      then WorkerQueueControl.finishStop worker.stopQueueStatus
      else do
        forceStop
        let workerLabel = WorkerStopKind.label config.stopKind
        let streamIdText = toText config.stopStreamId
        Log.warn
          [fmt|Stop message timed out for #{workerLabel} stream #{streamIdText}, force-cancelling worker|]
          |> Task.ignoreError


forceWorkerStop ::
  WorkerQueueControl ->
  AsyncTask Text Unit ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
forceWorkerStop queueStatus workerTask workerChannel = do
  -- Admitted writes use a timeout. Worker cancellation still assumes callbacks respond to cancellation.
  abandonedEvents <- Task.withCancellationBlocked do
    WorkerQueueControl.waitForWriters queueStatus
    AsyncTask.cancel workerTask
    drainedEvents <- drainAbandonedWorkerQueue workerChannel
    WorkerQueueControl.finishStop queueStatus
    Task.yield drainedEvents
  abandonedEvents
    |> Task.forEach \event -> do
      let logContext = EventLogContext.fromEvent event
      EventLogContext.logResult logContext Dropped
        |> Task.ignoreError


withWorkerCleanupHandoff ::
  Task Text (Maybe worker) ->
  (worker -> Task Text Unit) ->
  (worker -> Task Text Unit) ->
  Task Text Unit
withWorkerCleanupHandoff removeWorker ~afterRemoval ~cleanupWorker = Task.withCancellationDeferred \restore -> do
  maybeWorker <- removeWorker
  case maybeWorker of
    Nothing -> Task.yield unit
    Just worker -> do
      let cleanup = Task.evaluate (cleanupWorker worker) |> Task.andThen identity
      let runAfterRemoval = do
            callback <- Task.evaluate (afterRemoval worker)
            restore callback
      Task.onFailure (\_ -> cleanup) runAfterRemoval


withWorkerCandidateHandoff ::
  Task Text worker ->
  (worker -> Task Text (worker, Maybe worker)) ->
  (worker -> Task Text Unit) ->
  Task Text worker
withWorkerCandidateHandoff ~createCandidate ~publishCandidate ~cleanupWorker = Task.withCancellationDeferred \restore -> do
  candidate <- createCandidate
  let publish = Task.evaluate (publishCandidate candidate) |> Task.andThen identity
  (actualWorker, maybeDiscarded) <-
    publish
      |> Task.onFailure (\_ -> cleanupOwnedWorker restore candidate cleanupWorker)
  case maybeDiscarded of
    Nothing -> Task.yield actualWorker
    Just discardedWorker -> do
      cleanupOwnedWorker restore discardedWorker cleanupWorker
      Task.yield actualWorker


cleanupOwnedWorker ::
  (Task Text Unit -> Task Text Unit) ->
  worker ->
  (worker -> Task Text Unit) ->
  Task Text Unit
cleanupOwnedWorker restore worker ~cleanupWorker = do
  let attempt = do
        callback <- Task.evaluate (cleanupWorker worker)
        restore callback
  let retry _ = Task.evaluate (cleanupWorker worker) |> Task.andThen identity
  Task.onFailure retry attempt


drainAbandonedWorkerQueue ::
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text (Array (Event Json.Value))
drainAbandonedWorkerQueue workerChannel = drain []
  where
    drain reversedEvents = do
      maybeMessage <- workerChannel |> Channel.tryRead
      case maybeMessage of
        Nothing -> Task.yield (reversedEvents |> LinkedList.reverse |> Array.fromLinkedList)
        Just Stop -> drain reversedEvents
        Just (ProcessEvent event) -> drain (event : reversedEvents)


getOrCreateStatelessWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Task Text EntityWorker
getOrCreateStatelessWorker dispatcher streamId = do
  withWorkerCandidateHandoff
    (spawnStatelessWorker dispatcher streamId)
    (\candidate ->
      dispatcher.entityWorkers
        |> ConcurrentMap.getOrInsertIfTransaction
          streamId
          candidate
          (\existing -> WorkerQueueControl.shouldReplace existing.queueStatus)
    )
    (stopStatelessWorkerOrCancel dispatcher streamId)


-- | Get an existing lifecycle worker or create a new one atomically.
--
getOrCreateLifecycleWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Text -> -- ^ Entity type name for filtering which lifecycle runners to initialize
  Task Text LifecycleEntityWorker
getOrCreateLifecycleWorker dispatcher streamId entityTypeName = do
  -- First, check if there's an existing worker and its status
  maybeExisting <- dispatcher.lifecycleEntityWorkers |> ConcurrentMap.get streamId

  case maybeExisting of
    Just existingWorker -> do
      accepting <- WorkerQueueControl.isAccepting existingWorker.queueStatus
      if accepting
        then Task.yield existingWorker
        else createLifecycleWorkerCandidate dispatcher streamId entityTypeName
    Nothing -> createLifecycleWorkerCandidate dispatcher streamId entityTypeName


createLifecycleWorkerCandidate ::
  IntegrationDispatcher ->
  StreamId ->
  Text ->
  Task Text LifecycleEntityWorker
createLifecycleWorkerCandidate dispatcher streamId entityTypeName =
  withWorkerCandidateHandoff
    (spawnLifecycleWorker dispatcher streamId entityTypeName)
    (\candidate ->
      dispatcher.lifecycleEntityWorkers
        |> ConcurrentMap.getOrInsertIfTransaction
          streamId
          candidate
          (\existing -> WorkerQueueControl.shouldReplace existing.queueStatus)
    )
    (stopLifecycleWorkerOrCancel dispatcher streamId)


getCurrentTimeMs :: Task Text Int
getCurrentTimeMs = DateTime.now |> Task.map DateTime.toEpochMilliseconds


data StatelessRunContext = StatelessRunContext
  { statelessDispatcher :: IntegrationDispatcher,
    statelessStreamId :: StreamId,
    statelessChannel :: Channel (WorkerMessage (Event Json.Value)),
    statelessLastActivity :: ConcurrentVar Int
  }


data LifecycleRunContext = LifecycleRunContext
  { lifecycleDispatcher :: IntegrationDispatcher,
    lifecycleStreamId :: StreamId,
    lifecycleChannel :: Channel (WorkerMessage (Event Json.Value)),
    lifecycleLastActivity :: ConcurrentVar Int,
    lifecycleWorkerStates :: Array LifecycleRunnerWorkerState
  }


spawnStatelessWorker :: IntegrationDispatcher -> StreamId -> Task Text EntityWorker
spawnStatelessWorker dispatcher streamId = do
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime
  queueStatus <- WorkerQueueControl.new
  let context = StatelessRunContext
        { statelessDispatcher = dispatcher,
          statelessStreamId = streamId,
          statelessChannel = workerChannel,
          statelessLastActivity = lastActivity
        }
  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Starting worker for stream #{toText streamId}|]
      |> Task.ignoreError
  workerTask <- AsyncTask.runWithCancellationEnabled (runStatelessWorkerSafely context)
  Task.yield
    EntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        queueStatus = queueStatus
      }


runStatelessWorkerSafely :: StatelessRunContext -> Task Text Unit
runStatelessWorkerSafely context = do
  result <- runStatelessWorker context |> Task.asResultSafe
  case result of
    Ok _ -> pass
    Err _ -> do
      context.statelessDispatcher.entityWorkers
        |> ConcurrentMap.remove context.statelessStreamId
      let streamIdText = toText context.statelessStreamId
      Log.warn [fmt|Stateless worker crashed for stream #{streamIdText}|]
        |> Task.ignoreError


runStatelessWorker :: StatelessRunContext -> Task Text Unit
runStatelessWorker context = do
  message <- context.statelessChannel |> Channel.read
  case message of
    Stop -> drainStatelessWorker context.statelessDispatcher context.statelessChannel
    ProcessEvent event -> do
      processStatelessWorkerEvent context event
      runStatelessWorker context


processStatelessWorkerEvent :: StatelessRunContext -> Event Json.Value -> Task Text Unit
processStatelessWorkerEvent context event = do
  newTime <- getCurrentTimeMs
  _ <- context.statelessLastActivity |> ConcurrentVar.swap newTime
  let logContext = EventLogContext.fromEvent event
  timeoutReached <- ConcurrentVar.containing False
  processResult <- EventLogContext.withScope logContext do
    runWithTimeout
      timeoutReached
      context.statelessDispatcher.config.eventProcessingTimeoutMs
      ( EventLogContext.withScope logContext
          (processStatelessEvent context.statelessDispatcher logContext timeoutReached event)
      )
  case processResult of
    Ok _ -> pass
    Err _ -> do
      let streamIdText = toText context.statelessStreamId
      Log.warn [fmt|Event processing failed for stream #{streamIdText}|]
        |> Task.ignoreError


spawnLifecycleWorker :: IntegrationDispatcher -> StreamId -> Text -> Task Text LifecycleEntityWorker
spawnLifecycleWorker dispatcher streamId entityTypeName = do
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime
  queueStatus <- WorkerQueueControl.new
  states <- spawnLifecycleStates dispatcher streamId entityTypeName
  let context = LifecycleRunContext
        { lifecycleDispatcher = dispatcher,
          lifecycleStreamId = streamId,
          lifecycleChannel = workerChannel,
          lifecycleLastActivity = lastActivity,
          lifecycleWorkerStates = states
        }
  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Starting lifecycle worker for stream #{toText streamId}|]
      |> Task.ignoreError
  workerTask <- AsyncTask.runWithCancellationEnabled (runLifecycleWorkerSafely context)
  Task.yield
    LifecycleEntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        workerStates = states,
        queueStatus = queueStatus
      }


spawnLifecycleStates :: IntegrationDispatcher -> StreamId -> Text -> Task Text (Array LifecycleRunnerWorkerState)
spawnLifecycleStates dispatcher streamId entityTypeName =
  dispatcher.lifecycleRunners
    |> Array.indexed
    |> Array.takeIf (\(_, runner) -> runner.entityTypeName == entityTypeName)
    |> Task.mapArray (\(runnerOrdinal, runner) -> do
      state <- runner.spawnWorkerState streamId
      Task.yield
        LifecycleRunnerWorkerState
          { lifecycleRunnerOrdinal = runnerOrdinal,
            lifecycleRunnerEntityTypeName = runner.entityTypeName,
            lifecycleRunnerState = state
          }
    )


runLifecycleWorkerSafely :: LifecycleRunContext -> Task Text Unit
runLifecycleWorkerSafely context = do
  result <- runLifecycleWorker context |> Task.asResultSafe
  case result of
    Ok _ -> pass
    Err _ -> do
      context.lifecycleDispatcher.lifecycleEntityWorkers
        |> ConcurrentMap.remove context.lifecycleStreamId
      let streamIdText = toText context.lifecycleStreamId
      Log.warn [fmt|Lifecycle worker crashed for stream #{streamIdText}|]
        |> Task.ignoreError
      cleanupLifecycleStates context.lifecycleWorkerStates


runLifecycleWorker :: LifecycleRunContext -> Task Text Unit
runLifecycleWorker context = do
  message <- context.lifecycleChannel |> Channel.read
  case message of
    Stop -> do
      drainLifecycleWorker
        context.lifecycleDispatcher.config
        context.lifecycleWorkerStates
        context.lifecycleDispatcher.commandEndpoints
        context.lifecycleChannel
      cleanupLifecycleStates context.lifecycleWorkerStates
    ProcessEvent event -> do
      processLifecycleWorkerEvent context event
      runLifecycleWorker context


processLifecycleWorkerEvent :: LifecycleRunContext -> Event Json.Value -> Task Text Unit
processLifecycleWorkerEvent context event = do
  newTime <- getCurrentTimeMs
  context.lifecycleLastActivity |> ConcurrentVar.modify (\_ -> newTime)
  let logContext = EventLogContext.fromEvent event
  timeoutReached <- ConcurrentVar.containing False
  processResult <- EventLogContext.withScope logContext do
    runWithTimeout
      timeoutReached
      context.lifecycleDispatcher.config.eventProcessingTimeoutMs
      ( EventLogContext.withScope logContext
          (processLifecycleEvent event
            LifecycleProcessingContext
              { lifecycleConfig = context.lifecycleDispatcher.config,
                lifecycleStates = context.lifecycleWorkerStates,
                lifecycleEndpoints = context.lifecycleDispatcher.commandEndpoints,
                lifecycleLogContext = logContext,
                lifecycleTimeoutReached = timeoutReached
              })
      )
  case processResult of
    Ok _ -> pass
    Err _ -> do
      let streamIdText = toText context.lifecycleStreamId
      Log.warn [fmt|Lifecycle event processing failed for stream #{streamIdText}|]
        |> Task.ignoreError


cleanupLifecycleStates :: Array LifecycleRunnerWorkerState -> Task err Unit
cleanupLifecycleStates states =
  states |> Task.forEach (\runnerState -> runnerState.lifecycleRunnerState.cleanup |> Task.ignoreError)


data RetryRequest = RetryRequest
  { retryLogContext :: EventLogContext,
    retryConfig :: DispatcherConfig,
    retryEntityType :: Text,
    retryStreamId :: StreamId
  }


-- Only IO exceptions retry. Task errors stop, and the caller dispatches commands
-- after this boundary so a retried runner cannot dispatch a command twice.
executeWithRetry ::
  forall result.
  Task Text result ->
  RetryRequest ->
  Task Text (Maybe result)
executeWithRetry action request = do
  let maxRetries = request.retryConfig.maxEventRetries
  let baseDelay = request.retryConfig.retryBaseDelayMs
  let streamIdText = toText request.retryStreamId
  let entityType = request.retryEntityType
  let retryLoop attempt = do
        let innerAction :: Task Text (Maybe result)
            innerAction = do
              innerResult <- action |> Task.asResult
              case innerResult of
                Ok value -> Task.yield (Just value)
                Err _ -> do
                  Log.warn [fmt|[Integration] Failed to process event for #{entityType} (stream: #{streamIdText})|]
                    |> Task.ignoreError
                  Task.yield Nothing
        outerResult <- innerAction |> Task.asResultSafe
        case outerResult of
          Ok maybeValue -> Task.yield maybeValue
          Err _ -> do
            let attemptNum = attempt + 1
            if attempt >= maxRetries
              then do
                Log.warn [fmt|[Integration] Outbound processing failed for #{entityType} (stream: #{streamIdText}) after #{attemptNum} attempt(s)|]
                  |> Task.ignoreError
                Task.yield Nothing
              else do
                let attemptFloat = Int.toFloat attempt
                let candidateDelay = Float.toInt (Int.toFloat baseDelay * (2.0 ^ attemptFloat))
                let delay = min candidateDelay 60000 -- Cap at 60s to prevent overflow
                EventLogContext.logRetry request.retryLogContext
                  EventLogContext.RetryDetails
                    { attempt = attemptNum,
                      maxRetries = maxRetries,
                      delayMs = delay
                    }
                AsyncTask.sleep delay
                retryLoop (attempt + 1)
  retryLoop 0


-- | Process an event through stateless runners.
--
-- Only runners whose 'entityTypeName' matches the event's 'entityName' are
-- invoked (fixes issue #323). IO exceptions are retried with exponential
-- backoff via 'executeWithRetry' (fixes issue #571).
processStatelessEvent ::
  IntegrationDispatcher ->
  EventLogContext ->
  ConcurrentVar Bool ->
  Event Json.Value ->
  Task Text Unit
processStatelessEvent dispatcher logContext timeoutReached event = do
  let streamId = event.streamId
  let eventEntityName = event.entityName |> EntityName.toText
  let sourceEventMetadata = event.metadata
  runnerExecutions <-
    dispatcher.outboundRunners
      |> Array.indexed
      |> Array.takeIf (\(_, runner) -> runner.entityTypeName == eventEntityName)
      |> Task.mapArray \(runnerOrdinal, runner) ->
        RunnerExecution.new OutboundKind runnerOrdinal runner
  let processRunner execution = do
        let runner = execution.runner
        let runnerEntityType = runner.entityTypeName
        let runnerTask = executeWithRetry
              (runner.processEvent dispatcher.context dispatcher.eventStore event)
              RetryRequest
                { retryLogContext = logContext,
                  retryConfig = dispatcher.config,
                  retryEntityType = runnerEntityType,
                  retryStreamId = streamId
                }
        let dispatchPayload payload =
              dispatchCommandWithSource dispatcher.commandEndpoints (Just sourceEventMetadata) payload
        RunnerExecution.run execution runnerTask dispatchPayload
  Task.finally
    (RunnerExecution.logAll logContext timeoutReached runnerExecutions)
    (runnerExecutions |> Task.forEach processRunner)


data LifecycleProcessingContext = LifecycleProcessingContext
  { lifecycleConfig :: DispatcherConfig,
    lifecycleStates :: Array LifecycleRunnerWorkerState,
    lifecycleEndpoints :: Map Text DispatchHandler,
    lifecycleLogContext :: EventLogContext,
    lifecycleTimeoutReached :: ConcurrentVar Bool
  }


-- | Process an event through lifecycle runners.
-- Only runners whose entity type matches the event's entity name are invoked.
-- IO exceptions are retried with exponential backoff.
processLifecycleEvent :: Event Json.Value -> LifecycleProcessingContext -> Task Text Unit
processLifecycleEvent event context = do
  let streamId = event.streamId
  let eventEntityName = event.entityName |> EntityName.toText
  let sourceEventMetadata = event.metadata
  runnerExecutions <-
    context.lifecycleStates
      |> Array.takeIf (\runnerState -> runnerState.lifecycleRunnerEntityTypeName == eventEntityName)
      |> Task.mapArray \runnerState ->
        RunnerExecution.new
          LifecycleKind
          runnerState.lifecycleRunnerOrdinal
          runnerState
  let processRunner execution = do
        let runnerState = execution.runner
        let runnerEntityType = runnerState.lifecycleRunnerEntityTypeName
        let runnerTask = executeWithRetry
              (runnerState.lifecycleRunnerState.processEvent event)
              RetryRequest
                { retryLogContext = context.lifecycleLogContext,
                  retryConfig = context.lifecycleConfig,
                  retryEntityType = runnerEntityType,
                  retryStreamId = streamId
                }
        let dispatchPayload payload =
              dispatchCommandWithSource context.lifecycleEndpoints (Just sourceEventMetadata) payload
        RunnerExecution.run execution runnerTask dispatchPayload
  Task.finally
    (RunnerExecution.logAll context.lifecycleLogContext context.lifecycleTimeoutReached runnerExecutions)
    (runnerExecutions |> Task.forEach processRunner)


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
        let logContext = EventLogContext.fromEvent event
        timeoutReached <- ConcurrentVar.containing False
        EventLogContext.withScope logContext do
          processStatelessEvent dispatcher logContext timeoutReached event
        -- Continue draining
        drainStatelessWorker dispatcher workerChannel


-- | Drain remaining events from a lifecycle worker's channel before cleanup.
--
-- Similar to drainStatelessWorker but uses lifecycle event processing.
drainLifecycleWorker ::
  DispatcherConfig ->
  Array LifecycleRunnerWorkerState ->
  Map Text DispatchHandler ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
drainLifecycleWorker config states endpoints workerChannel = do
  maybeMessage <- workerChannel |> Channel.tryRead
  case maybeMessage of
    Nothing ->
      -- Channel empty, done draining
      Task.yield unit
    Just message -> case message of
      Stop ->
        -- Another Stop, ignore and continue draining
        drainLifecycleWorker config states endpoints workerChannel
      ProcessEvent event -> do
        let logContext = EventLogContext.fromEvent event
        timeoutReached <- ConcurrentVar.containing False
        EventLogContext.withScope logContext do
          processLifecycleEvent event
            LifecycleProcessingContext
              { lifecycleConfig = config,
                lifecycleStates = states,
                lifecycleEndpoints = endpoints,
                lifecycleLogContext = logContext,
                lifecycleTimeoutReached = timeoutReached
              }
        -- Continue draining
        drainLifecycleWorker config states endpoints workerChannel


-- | Dispatch a command to the appropriate handler.
-- Uses an anonymous RequestContext since integrations are system-triggered.
dispatchCommand ::
  Map Text EndpointHandler ->
  Integration.CommandPayload ->
  Task Text Unit
dispatchCommand endpoints payload =
  dispatchCommandWithSource
    (endpoints |> Map.mapValues (\handler _sourceEventMetadata -> handler))
    Nothing
    payload


dispatchCommandWithSource ::
  Map Text DispatchHandler ->
  Maybe EventMetadata ->
  Integration.CommandPayload ->
  Task Text Unit
dispatchCommandWithSource endpoints sourceEventMetadata payload = do
  let cmdType = payload.commandType
  case Map.get cmdType endpoints of
    Just handler -> do
      -- System-triggered commands (inbound timers, outbound integration
      -- callbacks) run on the framework's behalf, not on behalf of an
      -- external caller. Use trustedContext so the dispatcher's
      -- canExecuteImpl gate bypasses them — otherwise inbound integrations
      -- like the periodic timer would be rejected by the default
      -- authenticatedAccess gate.
      requestContext <- Auth.trustedContext
      let cmdBytes = Json.encodeText payload.commandData |> Text.toBytes
      let responseCallback (response, _) = do
            case response of
              Failed {} -> do
                Log.warn "Command dispatch failed" |> Task.ignoreError
                Task.throw "Command dispatch failed"
              Rejected {} -> do
                Log.warn "Command dispatch rejected" |> Task.ignoreError
                Task.throw "Command dispatch failed"
              Unauthorized {} -> do
                Log.warn "Command dispatch unauthorized" |> Task.ignoreError
                Task.throw "Command dispatch failed"
              Accepted {} -> do
                Log.debug [fmt|Command dispatched successfully: #{cmdType}|] |> Task.ignoreError
                Task.yield unit
      handler sourceEventMetadata requestContext cmdBytes responseCallback
        |> Task.mapError (\_ -> "Command dispatch failed")
    Nothing -> do
      Log.warn "Command dispatch handler unavailable"
        |> Task.ignoreError
      Task.throw "Command dispatch handler unavailable"


startReaper ::
  IntegrationDispatcher ->
  Task Text (AsyncTask Text Unit)
startReaper dispatcher = AsyncTask.run (reaperLoop dispatcher)


reaperLoop :: IntegrationDispatcher -> Task Text Unit
reaperLoop dispatcher = do
  AsyncTask.sleep dispatcher.config.reaperIntervalMs
  shouldShutdown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
  Task.unless shouldShutdown do
    currentTime <- getCurrentTimeMs
    let reaperChunkSize = 50
    dispatcher.lifecycleEntityWorkers
      |> ConcurrentMap.forEachChunked reaperChunkSize (reapIdleLifecycleWorker dispatcher currentTime)
    dispatcher.entityWorkers
      |> ConcurrentMap.forEachChunked reaperChunkSize (reapIdleStatelessWorker dispatcher currentTime)
    reaperLoop dispatcher


reapIdleLifecycleWorker ::
  IntegrationDispatcher -> Int -> StreamId -> LifecycleEntityWorker -> Task Text Unit
reapIdleLifecycleWorker dispatcher currentTime streamId worker = do
  lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
  Task.when (currentTime - lastActivity > dispatcher.config.idleTimeoutMs) do
    let capturedWorkerId = worker.workerId
    let markSameWorkerDraining currentWorker = do
          if currentWorker.workerId == capturedWorkerId
            then WorkerQueueControl.markDraining currentWorker.queueStatus
            else pure False
    withWorkerCleanupHandoff
      (dispatcher.lifecycleEntityWorkers
        |> ConcurrentMap.removeIfTransaction streamId markSameWorkerDraining
      )
      (\removedWorker -> do
        Log.debug [fmt|Reaped idle lifecycle worker for stream #{toText streamId}|]
          |> Task.ignoreError
        stopLifecycleWorkerOrCancel dispatcher streamId removedWorker
      )
      (stopLifecycleWorkerOrCancel dispatcher streamId)


reapIdleStatelessWorker ::
  IntegrationDispatcher -> Int -> StreamId -> EntityWorker -> Task Text Unit
reapIdleStatelessWorker dispatcher currentTime streamId worker = do
  lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
  Task.when (currentTime - lastActivity > dispatcher.config.idleTimeoutMs) do
    let capturedWorkerId = worker.workerId
    let markSameWorkerDraining currentWorker = do
          if currentWorker.workerId == capturedWorkerId
            then WorkerQueueControl.markDraining currentWorker.queueStatus
            else pure False
    withWorkerCleanupHandoff
      (dispatcher.entityWorkers
        |> ConcurrentMap.removeIfTransaction streamId markSameWorkerDraining
      )
      (\removedWorker -> do
        Log.debug [fmt|Reaped idle worker for stream #{toText streamId}|]
          |> Task.ignoreError
        stopStatelessWorkerOrCancel dispatcher streamId removedWorker
      )
      (stopStatelessWorkerOrCancel dispatcher streamId)


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
