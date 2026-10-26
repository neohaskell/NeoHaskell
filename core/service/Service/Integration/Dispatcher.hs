{-# LANGUAGE Strict #-}

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
import Control.Concurrent.Async qualified as GhcAsync
import Control.Concurrent.STM qualified as GhcSTM
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Log qualified
import Service.Response (CommandResponse (..))
import Data.Either (Either (..))
import Data.Time.Clock.POSIX qualified as GhcPosix
import Float qualified
import GHC.Float (Double)
import GHC.Real qualified as GhcReal
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
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore)
import Service.Integration.DispatchRegistry qualified as DispatchRegistry
import Service.Integration.Types (OutboundRunner (..), OutboundLifecycleRunner (..), WorkerState (..))
import Service.Transport (DispatchHandler, EndpointHandler)
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | Run a task with an optional timeout.
--
-- If a timeout is specified and expires before the task completes,
-- returns @Err "timeout"@. Otherwise returns @Ok result@.
runWithTimeout ::
  forall err result.
  ConcurrentVar Bool ->
  Maybe Int -> -- ^ Timeout in milliseconds (Nothing = no timeout)
  Task err result ->
  Task Text (Result Text result)
runWithTimeout timeoutReached maybeTimeoutMs task = case maybeTimeoutMs of
  Nothing -> do
    -- No timeout, just run the task and wrap result
    result <- task |> Task.asResult
    case result of
      Ok value -> Task.yield (Ok value)
      Err _ -> Task.yield (Err "processing failed")
  Just timeoutMs -> do
    -- Race the task against a sleep
    raceResult <- Task.fromIO do
      GhcAsync.race
        (Task.runResult task)
        (do
          -- Sleep returns Task _ Unit, use runResult and ignore result
          _ <- AsyncTask.sleep timeoutMs |> Task.runResult
          _ <- timeoutReached |> ConcurrentVar.modify (\_ -> True) |> Task.runResult
          pure unit
        )
    case raceResult of
      Left taskResult -> case taskResult of
        Ok value -> Task.yield (Ok value)
        Err _ -> Task.yield (Err "processing failed")
      Right _ -> Task.yield (Err "timeout")


-- | Message type for worker channels (poison pill pattern).
--
-- Workers block on Channel.read, so we can't interrupt them with external signals.
-- Instead, we send a Stop message to gracefully terminate them.
data WorkerMessage value
  = ProcessEvent value
  | Stop
  deriving (Show)


data IntegrationLogContext = IntegrationLogContext
  { integrationEventId :: Text,
    integrationStreamId :: Text,
    integrationEntityName :: Text,
    integrationCorrelationId :: Text,
    integrationCausationId :: Maybe Text,
    integrationRelatedUserSub :: Maybe Text
  }


data IntegrationRunnerKind
  = OutboundRunnerKind
  | LifecycleRunnerKind


data IntegrationRunnerOutcome
  = IntegrationProcessed
  | IntegrationFailure IntegrationRunnerFailure


data IntegrationRunnerFailure
  = IntegrationRunnerProcessingFailure
  | IntegrationCallbackDispatchFailure
  | IntegrationRunnerTimeout
  | IntegrationRunnerCancellation
  | IntegrationRunnerNotStarted


data IntegrationResult
  = IntegrationSkipped
  | IntegrationDropped
  | IntegrationRunnerResult IntegrationRunnerKind Int IntegrationRunnerOutcome Int


data IntegrationRunnerLogState
  = IntegrationRunnerPending
  | IntegrationRunnerProcessing Int
  | IntegrationRunnerCompleted IntegrationRunnerOutcome Int


data IntegrationRunnerExecution runner = IntegrationRunnerExecution
  { integrationExecutionKind :: IntegrationRunnerKind,
    integrationExecutionOrdinal :: Int,
    integrationExecutionRunner :: runner,
    integrationExecutionLogState :: ConcurrentVar IntegrationRunnerLogState
  }


data LifecycleRunnerWorkerState = LifecycleRunnerWorkerState
  { lifecycleRunnerOrdinal :: Int,
    lifecycleRunnerEntityTypeName :: Text,
    lifecycleRunnerState :: WorkerState
  }


integrationLogContext :: Event Json.Value -> IntegrationLogContext
integrationLogContext event =
  IntegrationLogContext
    { integrationEventId = event.metadata.eventId |> Uuid.toText,
      integrationStreamId = event.streamId |> StreamId.toText,
      integrationEntityName = event.entityName |> EntityName.toText,
      integrationCorrelationId = integrationSourceCorrelationId event,
      integrationCausationId = event.metadata.causationId,
      integrationRelatedUserSub = event.metadata.relatedUserSub
    }


integrationSourceCorrelationId :: Event Json.Value -> Text
integrationSourceCorrelationId event = case event.metadata.correlationId of
  Just correlationId -> correlationId
  Nothing -> event.metadata.eventId |> Uuid.toText


integrationLogScopeFields :: IntegrationLogContext -> Array (Text, Text)
integrationLogScopeFields logContext =
  Array.fromLinkedList
    [ ("component", "Dispatcher"),
      ("eventId", logContext.integrationEventId),
      ("streamId", logContext.integrationStreamId),
      ("entityName", logContext.integrationEntityName),
      ("correlationId", logContext.integrationCorrelationId)
    ]
    |> appendIntegrationScopeField "causationId" logContext.integrationCausationId
    |> appendIntegrationScopeField "relatedUserSub" logContext.integrationRelatedUserSub


appendIntegrationScopeField :: Text -> Maybe Text -> Array (Text, Text) -> Array (Text, Text)
appendIntegrationScopeField fieldName maybeValue fields = case maybeValue of
  Just value -> fields |> Array.append (Array.fromLinkedList [(fieldName, value)])
  Nothing -> fields


withIntegrationEventScope :: IntegrationLogContext -> Task err value -> Task err value
withIntegrationEventScope logContext task =
  Log.withScope (integrationLogScopeFields logContext) task


integrationRunnerKindText :: IntegrationRunnerKind -> Text
integrationRunnerKindText runnerKind = case runnerKind of
  OutboundRunnerKind -> "outbound"
  LifecycleRunnerKind -> "lifecycle"


integrationOutcomeText :: IntegrationRunnerOutcome -> Text
integrationOutcomeText outcome = case outcome of
  IntegrationProcessed -> "processed"
  IntegrationFailure failure -> case failure of
    IntegrationRunnerProcessingFailure -> "failed"
    IntegrationCallbackDispatchFailure -> "failed"
    IntegrationRunnerTimeout -> "timed_out"
    IntegrationRunnerCancellation -> "cancelled"
    IntegrationRunnerNotStarted -> "cancelled"


integrationFailureFields :: IntegrationRunnerOutcome -> Array (Text, Json.Value)
integrationFailureFields outcome = case outcome of
  IntegrationProcessed -> Array.empty
  IntegrationFailure failure ->
    Array.fromLinkedList
      [ ("failureStage" Json..= integrationFailureStageText failure),
        ("errorClass" Json..= integrationFailureErrorClassText failure)
      ]


integrationFailureStageText :: IntegrationRunnerFailure -> Text
integrationFailureStageText failure = case failure of
  IntegrationRunnerProcessingFailure -> "runner_processing"
  IntegrationCallbackDispatchFailure -> "callback_dispatch"
  IntegrationRunnerTimeout -> "timeout"
  IntegrationRunnerCancellation -> "cancellation"
  IntegrationRunnerNotStarted -> "cancellation"


integrationFailureErrorClassText :: IntegrationRunnerFailure -> Text
integrationFailureErrorClassText failure = case failure of
  IntegrationRunnerProcessingFailure -> "RunnerProcessingFailure"
  IntegrationCallbackDispatchFailure -> "CallbackDispatchFailure"
  IntegrationRunnerTimeout -> "Timeout"
  IntegrationRunnerCancellation -> "ExecutionInterrupted"
  IntegrationRunnerNotStarted -> "RunnerNotStarted"


integrationOutcomeMessage :: IntegrationRunnerOutcome -> Text
integrationOutcomeMessage outcome = case outcome of
  IntegrationProcessed -> "Integration runner processed event"
  IntegrationFailure failure -> case failure of
    IntegrationRunnerProcessingFailure -> "Integration runner failed to process event"
    IntegrationCallbackDispatchFailure -> "Integration runner callback dispatch failed"
    IntegrationRunnerTimeout -> "Integration runner timed out"
    IntegrationRunnerCancellation -> "Integration runner was cancelled"
    IntegrationRunnerNotStarted -> "Integration runner was not started"


writeIntegrationResult ::
  IntegrationLogContext ->
  Log.Level ->
  Text ->
  Int ->
  Array (Text, Json.Value) ->
  Text ->
  Task Text Unit
writeIntegrationResult logContext level resultName commandsEmitted runnerFields message = do
  let commonFields =
        Array.fromLinkedList
          [ ("kind" Json..= ("integration_result" :: Text)),
            ("result" Json..= resultName),
            ("commandsEmitted" Json..= commandsEmitted)
          ]
  let fields = commonFields |> Array.append runnerFields
  withIntegrationEventScope logContext do
    Log.record level fields message |> Task.ignoreError


logIntegrationRetry :: IntegrationLogContext -> Int -> Int -> Int -> Task Text Unit
logIntegrationRetry logContext attempt maxRetries delayMs = do
  let fields =
        Array.fromLinkedList
          [ ("kind" Json..= ("retry" :: Text)),
            ("attempt" Json..= attempt),
            ("maxRetries" Json..= maxRetries),
            ("delayMs" Json..= delayMs)
          ]
  withIntegrationEventScope logContext do
    Log.record Log.Warn fields "Retrying integration runner after transient failure"
      |> Task.ignoreError


logIntegrationResult :: IntegrationLogContext -> IntegrationResult -> Task Text Unit
logIntegrationResult logContext result = case result of
  IntegrationSkipped ->
    writeIntegrationResult logContext Log.Info "skipped" 0 Array.empty "No integration runner matched the event"
  IntegrationDropped ->
    writeIntegrationResult logContext Log.Warn "dropped" 0 Array.empty "Integration event was not delivered"
  IntegrationRunnerResult runnerKind runnerOrdinal outcome commandsEmitted -> do
    let resultName = integrationOutcomeText outcome
    let runnerFields =
          Array.fromLinkedList
            [ ("runnerKind" Json..= integrationRunnerKindText runnerKind),
              ("runnerOrdinal" Json..= runnerOrdinal)
            ]
            |> Array.append (integrationFailureFields outcome)
    let level = case outcome of
          IntegrationProcessed -> Log.Info
          IntegrationFailure _ -> Log.Warn
    let message = integrationOutcomeMessage outcome
    writeIntegrationResult logContext level resultName commandsEmitted runnerFields message


newIntegrationRunnerExecution ::
  IntegrationRunnerKind ->
  Int ->
  runner ->
  Task Text (IntegrationRunnerExecution runner)
newIntegrationRunnerExecution runnerKind runnerOrdinal runner = do
  runnerLogState <- ConcurrentVar.containing IntegrationRunnerPending
  Task.yield
    IntegrationRunnerExecution
      { integrationExecutionKind = runnerKind,
        integrationExecutionOrdinal = runnerOrdinal,
        integrationExecutionRunner = runner,
        integrationExecutionLogState = runnerLogState
      }


logIntegrationRunnerExecutions ::
  IntegrationLogContext ->
  ConcurrentVar Bool ->
  Array (IntegrationRunnerExecution runner) ->
  Task Text Unit
logIntegrationRunnerExecutions logContext timeoutReached executions =
  executions |> Task.forEach (logIntegrationRunnerExecution logContext timeoutReached)


logIntegrationRunnerExecution ::
  IntegrationLogContext ->
  ConcurrentVar Bool ->
  IntegrationRunnerExecution runner ->
  Task Text Unit
logIntegrationRunnerExecution logContext timeoutReached execution = do
  runnerState <- execution.integrationExecutionLogState |> ConcurrentVar.peek
  case runnerState of
    IntegrationRunnerPending ->
      logIntegrationResult
        logContext
        ( IntegrationRunnerResult
            execution.integrationExecutionKind
            execution.integrationExecutionOrdinal
            (IntegrationFailure IntegrationRunnerNotStarted)
            0
        )
    IntegrationRunnerProcessing commandCount -> do
      didTimeout <- timeoutReached |> ConcurrentVar.peek
      let failure = if didTimeout then IntegrationRunnerTimeout else IntegrationRunnerCancellation
      logIntegrationResult
        logContext
        ( IntegrationRunnerResult
            execution.integrationExecutionKind
            execution.integrationExecutionOrdinal
            (IntegrationFailure failure)
            commandCount
        )
    IntegrationRunnerCompleted outcome commandCount ->
      logIntegrationResult
        logContext
        ( IntegrationRunnerResult
            execution.integrationExecutionKind
            execution.integrationExecutionOrdinal
            outcome
            commandCount
        )


runIntegrationRunner ::
  IntegrationRunnerExecution runner ->
  Task Text (Maybe (Array Integration.CommandPayload)) ->
  (Integration.CommandPayload -> Task Text Unit) ->
  Task Text Unit
runIntegrationRunner execution runnerTask dispatchPayload = do
  let runnerLogState = execution.integrationExecutionLogState
  let completeRunner outcome = do
        runnerLogState
          |> ConcurrentVar.modify (\runnerState -> case runnerState of
            IntegrationRunnerPending -> IntegrationRunnerCompleted outcome 0
            IntegrationRunnerProcessing commandCount -> IntegrationRunnerCompleted outcome commandCount
            IntegrationRunnerCompleted _ commandCount -> IntegrationRunnerCompleted outcome commandCount
          )
  runnerLogState
    |> ConcurrentVar.modify (\runnerState -> case runnerState of
      IntegrationRunnerPending -> IntegrationRunnerProcessing 0
      existing -> existing
    )
  let processRunner = do
        maybeCommandsResult <- runnerTask |> Task.asResultSafe
        case maybeCommandsResult of
          Err _ -> completeRunner (IntegrationFailure IntegrationRunnerProcessingFailure)
          Ok Nothing -> completeRunner (IntegrationFailure IntegrationRunnerProcessingFailure)
          Ok (Just commands) -> do
            let dispatchCommandPayload payload = do
                  dispatchPayload payload
                  runnerLogState
                    |> ConcurrentVar.modify (\state -> case state of
                      IntegrationRunnerProcessing commandCount -> IntegrationRunnerProcessing (commandCount + 1)
                      completed -> completed
                    )
            let reportDispatchFailure failure = case failure of
                  Task.Interrupted -> Task.yield unit
                  Task.TypedFailure _ -> completeRunner (IntegrationFailure IntegrationCallbackDispatchFailure)
                  Task.RuntimeFailure -> completeRunner (IntegrationFailure IntegrationCallbackDispatchFailure)
            let dispatchCommands =
                  Task.evaluate (commands |> Task.forEach dispatchCommandPayload)
                    |> Task.andThen identity
            dispatchCommands |> Task.onFailure reportDispatchFailure
            completeRunner IntegrationProcessed
        Task.yield unit
  processRunner


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
    eventProcessingTimeoutMs :: Maybe Int,
    -- | Maximum number of retry attempts for transient IO failures in outbound
    -- event processing. Only IO exceptions (e.g., network errors) are retried;
    -- application-level errors (e.g., JSON decode failures) are not.
    -- Default: 3 (total attempts = initial + 3 retries)
    maxEventRetries :: Int,
    -- | Base delay in milliseconds for exponential backoff between retries.
    -- Actual delays: baseDelay * 2^attempt (e.g., 500ms, 1000ms, 2000ms).
    -- Default: 500
    retryBaseDelayMs :: Int
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
      eventProcessingTimeoutMs = Just 30000,
      maxEventRetries = 3,
      retryBaseDelayMs = 500
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


data WorkerQueueStatus
  = WorkerAccepting Int
  | WorkerDraining Int
  | WorkerStopping Int
  | WorkerStopped
  deriving (Eq, Show)


newtype WorkerQueueControl = WorkerQueueControl (GhcSTM.TVar WorkerQueueStatus)


newWorkerQueueControl :: Task Text WorkerQueueControl
newWorkerQueueControl = do
  status <- GhcSTM.newTVarIO (WorkerAccepting 0) |> Task.fromIO
  Task.yield (WorkerQueueControl status)


workerQueueStatusSTM :: WorkerQueueControl -> GhcSTM.STM WorkerQueueStatus
workerQueueStatusSTM (WorkerQueueControl status) =
  GhcSTM.readTVar status


workerQueueStatus :: WorkerQueueControl -> Task Text WorkerQueueStatus
workerQueueStatus control =
  GhcSTM.atomically (workerQueueStatusSTM control)
    |> Task.fromIO


acquireWorkerWrite :: WorkerQueueControl -> Task Text Bool
acquireWorkerWrite (WorkerQueueControl status) =
  GhcSTM.atomically do
    currentStatus <- GhcSTM.readTVar status
    case currentStatus of
      WorkerAccepting writers -> do
        GhcSTM.writeTVar status (WorkerAccepting (writers + 1))
        pure True
      _ -> pure False
  |> Task.fromIO


releaseWorkerWrite :: WorkerQueueControl -> Task Text Unit
releaseWorkerWrite (WorkerQueueControl status) =
  GhcSTM.atomically do
    currentStatus <- GhcSTM.readTVar status
    case currentStatus of
      WorkerAccepting writers -> GhcSTM.writeTVar status (WorkerAccepting (writers - 1))
      WorkerDraining writers -> GhcSTM.writeTVar status (WorkerDraining (writers - 1))
      WorkerStopping writers -> GhcSTM.writeTVar status (WorkerStopping (writers - 1))
      WorkerStopped -> pure ()
  |> Task.fromIO


markWorkerDrainingSTM :: WorkerQueueControl -> GhcSTM.STM Bool
markWorkerDrainingSTM (WorkerQueueControl status) = do
  currentStatus <- GhcSTM.readTVar status
  case currentStatus of
    WorkerAccepting writers -> do
      GhcSTM.writeTVar status (WorkerDraining writers)
      pure True
    _ -> pure False


workerShouldBeReplacedSTM :: WorkerQueueControl -> GhcSTM.STM Bool
workerShouldBeReplacedSTM control = do
  currentStatus <- workerQueueStatusSTM control
  case currentStatus of
    WorkerAccepting _ -> pure False
    _ -> pure True


claimWorkerStop :: WorkerQueueControl -> Task Text Bool
claimWorkerStop (WorkerQueueControl status) =
  GhcSTM.atomically do
    currentStatus <- GhcSTM.readTVar status
    case currentStatus of
      WorkerAccepting writers -> do
        GhcSTM.writeTVar status (WorkerStopping writers)
        pure True
      WorkerDraining writers -> do
        GhcSTM.writeTVar status (WorkerStopping writers)
        pure True
      _ -> pure False
  |> Task.fromIO


waitForWorkerWriters :: WorkerQueueControl -> Task Text Unit
waitForWorkerWriters (WorkerQueueControl status) =
  GhcSTM.atomically do
    currentStatus <- GhcSTM.readTVar status
    case currentStatus of
      WorkerStopping 0 -> pure ()
      WorkerStopping _ -> GhcSTM.retry
      _ -> pure ()
  |> Task.fromIO


finishWorkerStop :: WorkerQueueControl -> Task Text Unit
finishWorkerStop (WorkerQueueControl status) =
  GhcSTM.atomically do
    currentStatus <- GhcSTM.readTVar status
    case currentStatus of
      WorkerStopping 0 -> GhcSTM.writeTVar status WorkerStopped
      _ -> pure ()
  |> Task.fromIO


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
  emptyRefreshLocks <- ConcurrentMap.new
  let emptyContext = Integration.ActionContext
        { Integration.secretStore = emptyStore
        , Integration.providerRegistry = Integration.fromMap Map.empty
        , Integration.refreshLocks = emptyRefreshLocks
        , Integration.fileAccess = Nothing
        , Integration.outboundDispatch = DispatchRegistry.empty
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
newWithLifecycleConfig dispatcherConfig store runners lifecycleRunners endpoints actionContext =
  newWithLifecycleConfigWithSource
    dispatcherConfig
    store
    runners
    lifecycleRunners
    (endpoints |> Map.mapValues (\handler _sourceEventMetadata -> handler))
    actionContext


newWithLifecycleConfigWithSource ::
  DispatcherConfig ->
  EventStore Json.Value ->
  Array OutboundRunner ->
  Array OutboundLifecycleRunner ->
  Map Text DispatchHandler ->
  Integration.ActionContext ->
  Task Text IntegrationDispatcher
newWithLifecycleConfigWithSource dispatcherConfig store runners lifecycleRunners endpoints actionContext = do
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
  Task.unless (not dispatcherConfig.enableReaper) do
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
          let logContext = integrationLogContext event
          logIntegrationResult logContext IntegrationDropped
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
              let logContext = integrationLogContext event
              logIntegrationResult logContext IntegrationSkipped
            else do
              didQueueStateless <- if Array.isEmpty matchingStatelessRunners
                then Task.yield True
                else queueStatelessEvent dispatcher streamId event

              didQueueLifecycle <- if Array.isEmpty matchingLifecycleRunners
                then Task.yield True
                else queueLifecycleEvent dispatcher streamId eventEntityName event

              Task.when (not didQueueStateless || not didQueueLifecycle) do
                let logContext = integrationLogContext event
                logIntegrationResult logContext IntegrationDropped


data WorkerEnqueueResult
  = WorkerEventEnqueued
  | WorkerEventWriteTimedOut
  | WorkerQueueClosed


queueWorkerEventWithTimeout ::
  IntegrationDispatcher ->
  StreamId ->
  Event Json.Value ->
  WorkerQueueControl ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text WorkerEnqueueResult
queueWorkerEventWithTimeout dispatcher streamId event queueStatus workerChannel = Task.withCancellationDeferred \restore -> do
  isShuttingDown <- dispatcher.shutdownSignal |> ConcurrentVar.peek
  if isShuttingDown
    then Task.yield WorkerQueueClosed
    else do
      admitted <- acquireWorkerWrite queueStatus
      if not admitted
        then Task.yield WorkerQueueClosed
        else do
          writeResult <-
            Task.finally (releaseWorkerWrite queueStatus) do
              restore
                ( Channel.tryWriteWithTimeout
                    dispatcher.config.channelWriteTimeoutMs
                    (ProcessEvent event)
                    workerChannel
                )
          case writeResult of
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
    queueWorkerEventWithTimeout dispatcher streamId event worker.queueStatus worker.channel
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
    queueWorkerEventWithTimeout dispatcher streamId event worker.queueStatus worker.channel
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
    dispatcher
    streamId
    "worker"
    worker.queueStatus
    worker.workerTask
    worker.channel


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
    dispatcher
    streamId
    "lifecycle worker"
    worker.queueStatus
    worker.workerTask
    worker.channel


stopWorkerOrCancel ::
  IntegrationDispatcher ->
  StreamId ->
  Text ->
  WorkerQueueControl ->
  AsyncTask Text Unit ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
stopWorkerOrCancel dispatcher streamId workerLabel queueStatus workerTask workerChannel = Task.withCancellationDeferred \restore -> do
  claimed <- claimWorkerStop queueStatus
  if not claimed
    then Task.yield unit
    else do
      let forceStop = forceWorkerStop queueStatus workerTask workerChannel
      let attemptStop = do
            restore (waitForWorkerWriters queueStatus)
            writeResult <-
              restore
                ( Channel.tryWriteWithTimeout
                    dispatcher.config.channelWriteTimeoutMs
                    Stop
                    workerChannel
                )
            case writeResult of
              Ok _ -> Task.yield True
              Err _ -> Task.yield False
      stopResult <- Task.onFailure (\_ -> forceStop) attemptStop
      if stopResult
        then finishWorkerStop queueStatus
        else do
          forceStop
          Log.warn
            [fmt|Stop message timed out for #{workerLabel} stream #{toText streamId}, force-cancelling worker|]
            |> Task.ignoreError


forceWorkerStop ::
  WorkerQueueControl ->
  AsyncTask Text Unit ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Unit
forceWorkerStop queueStatus workerTask workerChannel = do
  -- Admitted writes use a timeout. Worker cancellation still assumes callbacks respond to cancellation.
  abandonedEvents <- Task.withCancellationBlocked do
    waitForWorkerWriters queueStatus
    AsyncTask.cancel workerTask
    drainedEvents <- drainAbandonedWorkerQueue workerChannel
    finishWorkerStop queueStatus
    Task.yield drainedEvents
  abandonedEvents
    |> Task.forEach \event -> do
      let logContext = integrationLogContext event
      logIntegrationResult logContext IntegrationDropped
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
        |> ConcurrentMap.getOrInsertIfM
          streamId
          candidate
          (\existing -> workerShouldBeReplacedSTM existing.queueStatus)
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
      existingStatus <- workerQueueStatus existingWorker.queueStatus
      case existingStatus of
        WorkerAccepting _ ->
          Task.yield existingWorker
        _ -> createLifecycleWorkerCandidate dispatcher streamId entityTypeName
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
        |> ConcurrentMap.getOrInsertIfM
          streamId
          candidate
          (\existing -> workerShouldBeReplacedSTM existing.queueStatus)
    )
    (stopLifecycleWorkerOrCancel dispatcher streamId)


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
  queueStatus <- newWorkerQueueControl

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
            let logContext = integrationLogContext event
            timeoutReached <- ConcurrentVar.containing False
            -- Process with optional timeout
            processResult <- withIntegrationEventScope logContext do
              runWithTimeout
                timeoutReached
                dispatcher.config.eventProcessingTimeoutMs
                ( withIntegrationEventScope logContext
                    (processStatelessEvent dispatcher logContext timeoutReached event)
                )
            case processResult of
              Ok _ -> pass
              Err _ -> do
                Log.warn [fmt|Event processing failed for stream #{toText streamId}|]
                  |> Task.ignoreError
            workerLoop

  -- Wrap worker loop in exception boundary
  let safeWorkerLoop :: Task Text Unit
      safeWorkerLoop = do
        result <- workerLoop |> Task.asResultSafe
        case result of
          Ok _ -> pass
          Err _ -> do
            -- Remove from map FIRST so new events can spawn a fresh worker
            dispatcher.entityWorkers |> ConcurrentMap.remove streamId
            -- Log after removal to ensure map is cleaned up even if logging fails
            Log.warn [fmt|Stateless worker crashed for stream #{toText streamId}|]
              |> Task.ignoreError

  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Starting worker for stream #{toText streamId}|]
      |> Task.ignoreError

  workerTask <- AsyncTask.runWithCancellationEnabled safeWorkerLoop
  Task.yield
    EntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        queueStatus = queueStatus
      }


-- | Spawn a lifecycle worker for an entity.
--
-- Uses bounded channels to provide backpressure when the worker can't keep up
-- with event production. Channel capacity is configured via DispatcherConfig.
spawnLifecycleWorker ::
  IntegrationDispatcher ->
  StreamId ->
  Text -> -- ^ Entity type name — only lifecycle runners matching this type are initialized
  Task Text LifecycleEntityWorker
spawnLifecycleWorker dispatcher streamId entityTypeName = do
  -- Generate unique ID for this worker instance
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime
  queueStatus <- newWorkerQueueControl

  -- Initialize only lifecycle runners matching the entity type for this stream.
  -- This avoids expensive resource allocation (DB connections, interpreters) for
  -- runners that will never process events on this stream.
  states <-
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

  let workerLoop :: Task Text Unit
      workerLoop = do
        message <- workerChannel |> Channel.read
        case message of
          Stop -> do
            -- Drain remaining events before cleanup
            drainLifecycleWorker dispatcher.config states dispatcher.commandEndpoints workerChannel
            -- Cleanup on stop (reap or shutdown)
            states |> Task.forEach (\runnerState -> runnerState.lifecycleRunnerState.cleanup |> Task.ignoreError)
          ProcessEvent event -> do
            -- Update activity time
            newTime <- getCurrentTimeMs
            lastActivity |> ConcurrentVar.modify (\_ -> newTime)
            let logContext = integrationLogContext event
            timeoutReached <- ConcurrentVar.containing False
            -- Process event through matching lifecycle runners with optional timeout
            processResult <- withIntegrationEventScope logContext do
              runWithTimeout
                timeoutReached
                dispatcher.config.eventProcessingTimeoutMs
                ( withIntegrationEventScope logContext
                    (processLifecycleEvent dispatcher.config states dispatcher.commandEndpoints logContext timeoutReached event)
                )
            case processResult of
              Ok _ -> pass
              Err _ -> do
                Log.warn [fmt|Lifecycle event processing failed for stream #{toText streamId}|]
                  |> Task.ignoreError
            workerLoop

  -- Wrap worker loop in exception boundary
  let safeWorkerLoop :: Task Text Unit
      safeWorkerLoop = do
        result <- workerLoop |> Task.asResultSafe
        case result of
          Ok _ -> pass
          Err _ -> do
            -- Remove from map FIRST so new events can spawn a fresh worker
            dispatcher.lifecycleEntityWorkers |> ConcurrentMap.remove streamId
            -- Log after removal to ensure map is cleaned up even if logging fails
            Log.warn [fmt|Lifecycle worker crashed for stream #{toText streamId}|]
              |> Task.ignoreError
            -- Attempt cleanup even on crash (best effort, after removal)
            states |> Task.forEach (\runnerState -> runnerState.lifecycleRunnerState.cleanup |> Task.ignoreError)

  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Starting lifecycle worker for stream #{toText streamId}|]
      |> Task.ignoreError

  workerTask <- AsyncTask.runWithCancellationEnabled safeWorkerLoop
  Task.yield
    LifecycleEntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        workerStates = states,
        queueStatus = queueStatus
      }


-- | Execute an action with retry for transient IO failures, returning the
-- result on success or 'Nothing' on failure.
--
-- Uses a two-layer error catching strategy:
--
-- 1. Inner layer ('Task.asResult'): catches 'Task.throw' errors (e.g., JSON decode
--    failures). These are non-retryable application errors — logged and returned as
--    'Nothing' immediately (no retry).
-- 2. Outer layer ('Task.asResultSafe'): catches raw IO exceptions (e.g., network
--    errors). These are transient and retried with exponential backoff.
--
-- The caller receives 'Just value' on success and dispatches side effects (e.g.,
-- command sends) outside the retry boundary. This prevents duplicate side effects
-- when retries occur.
--
-- Retry delays follow: @baseDelay * 2^attempt@ (e.g., 500ms, 1000ms, 2000ms).
executeWithRetry ::
  forall result.
  IntegrationLogContext ->
  DispatcherConfig ->
  Text -> -- ^ Entity type name (for logging)
  StreamId -> -- ^ Stream ID (for logging)
  Task Text result -> -- ^ Action to execute (should be pure/idempotent)
  Task Text (Maybe result)
executeWithRetry logContext config entityType streamId action = do
  let maxRetries = config.maxEventRetries
  let baseDelay = config.retryBaseDelayMs
  let streamIdText = toText streamId
  let retryLoop attempt = do
        -- Inner layer: catch Task.throw errors (non-retryable)
        let innerAction :: Task Text (Maybe result)
            innerAction = do
              innerResult <- action |> Task.asResult
              case innerResult of
                Ok value -> Task.yield (Just value)
                Err _ -> do
                  Log.warn [fmt|[Integration] Failed to process event for #{entityType} (stream: #{streamIdText})|]
                    |> Task.ignoreError
                  Task.yield Nothing
        -- Outer layer: catch IO exceptions (retryable)
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
                logIntegrationRetry logContext attemptNum maxRetries delay
                AsyncTask.sleep delay
                  |> Task.mapError (\_ -> "retry sleep error" :: Text)
                retryLoop (attempt + 1)
  retryLoop 0


-- | Process an event through stateless runners.
--
-- Only runners whose 'entityTypeName' matches the event's 'entityName' are
-- invoked (fixes issue #323). IO exceptions are retried with exponential
-- backoff via 'executeWithRetry' (fixes issue #571).
processStatelessEvent ::
  IntegrationDispatcher ->
  IntegrationLogContext ->
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
        newIntegrationRunnerExecution OutboundRunnerKind runnerOrdinal runner
  let processRunner execution = do
        let runner = execution.integrationExecutionRunner
        let runnerEntityType = runner.entityTypeName
        let runnerTask = executeWithRetry logContext dispatcher.config runnerEntityType streamId
              (runner.processEvent dispatcher.context dispatcher.eventStore event)
        let dispatchPayload payload =
              dispatchCommandWithSource dispatcher.commandEndpoints (Just sourceEventMetadata) payload
        runIntegrationRunner execution runnerTask dispatchPayload
  Task.finally
    (logIntegrationRunnerExecutions logContext timeoutReached runnerExecutions)
    (runnerExecutions |> Task.forEach processRunner)


-- | Process an event through lifecycle runners.
--
-- Only runners whose entity type matches the event's entity name are invoked.
-- IO exceptions are retried with exponential backoff.
processLifecycleEvent ::
  DispatcherConfig ->
  Array LifecycleRunnerWorkerState ->
  Map Text DispatchHandler ->
  IntegrationLogContext ->
  ConcurrentVar Bool ->
  Event Json.Value ->
  Task Text Unit
processLifecycleEvent config states endpoints logContext timeoutReached event = do
  let streamId = event.streamId
  let eventEntityName = event.entityName |> EntityName.toText
  let sourceEventMetadata = event.metadata
  runnerExecutions <-
    states
      |> Array.takeIf (\runnerState -> runnerState.lifecycleRunnerEntityTypeName == eventEntityName)
      |> Task.mapArray \runnerState ->
        newIntegrationRunnerExecution
          LifecycleRunnerKind
          runnerState.lifecycleRunnerOrdinal
          runnerState
  let processRunner execution = do
        let runnerState = execution.integrationExecutionRunner
        let runnerEntityType = runnerState.lifecycleRunnerEntityTypeName
        let runnerTask = executeWithRetry logContext config runnerEntityType streamId
              (runnerState.lifecycleRunnerState.processEvent event)
        let dispatchPayload payload =
              dispatchCommandWithSource endpoints (Just sourceEventMetadata) payload
        runIntegrationRunner execution runnerTask dispatchPayload
  Task.finally
    (logIntegrationRunnerExecutions logContext timeoutReached runnerExecutions)
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
        let logContext = integrationLogContext event
        timeoutReached <- ConcurrentVar.containing False
        withIntegrationEventScope logContext do
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
        let logContext = integrationLogContext event
        timeoutReached <- ConcurrentVar.containing False
        withIntegrationEventScope logContext do
          processLifecycleEvent config states endpoints logContext timeoutReached event
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
              Failed {} ->
                Log.warn "Command dispatch failed" |> Task.ignoreError
              Rejected {} ->
                Log.warn "Command dispatch rejected" |> Task.ignoreError
              _ ->
                Log.debug [fmt|Command dispatched successfully: #{cmdType}|] |> Task.ignoreError
            Task.yield unit
      handler sourceEventMetadata requestContext cmdBytes responseCallback
        |> Task.mapError (\err -> [fmt|Command dispatch failed for #{cmdType}: #{err}|])
    Nothing -> do
      Log.warn [fmt|No handler found for command: #{cmdType}|]
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
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                Task.when (idleTime > dispatcher.config.idleTimeoutMs) do
                    let capturedWorkerId = worker.workerId
                    let markSameWorkerDraining currentWorker = do
                          if currentWorker.workerId == capturedWorkerId
                            then markWorkerDrainingSTM currentWorker.queueStatus
                            else pure False
                    withWorkerCleanupHandoff
                      (dispatcher.lifecycleEntityWorkers
                        |> ConcurrentMap.removeIfM streamId markSameWorkerDraining
                      )
                      (\removedWorker -> do
                        Log.debug [fmt|Reaped idle lifecycle worker for stream #{toText streamId}|]
                          |> Task.ignoreError
                        stopLifecycleWorkerOrCancel dispatcher streamId removedWorker
                      )
                      (stopLifecycleWorkerOrCancel dispatcher streamId)

            dispatcher.entityWorkers
              |> ConcurrentMap.forEachChunked reaperChunkSize \streamId worker -> do
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                Task.when (idleTime > dispatcher.config.idleTimeoutMs) do
                    let capturedWorkerId = worker.workerId
                    let markSameWorkerDraining currentWorker = do
                          if currentWorker.workerId == capturedWorkerId
                            then markWorkerDrainingSTM currentWorker.queueStatus
                            else pure False
                    withWorkerCleanupHandoff
                      (dispatcher.entityWorkers
                        |> ConcurrentMap.removeIfM streamId markSameWorkerDraining
                      )
                      (\removedWorker -> do
                        Log.debug [fmt|Reaped idle worker for stream #{toText streamId}|]
                          |> Task.ignoreError
                        stopStatelessWorkerOrCancel dispatcher streamId removedWorker
                      )
                      (stopStatelessWorkerOrCancel dispatcher streamId)

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
