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
import AtomicVar (AtomicVar)
import AtomicVar qualified
import Basics
import Channel (Channel)
import Channel qualified
import Control.Exception qualified as GhcException
import ConcurrentMap (ConcurrentMap)
import ConcurrentMap qualified
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Log qualified
import Service.Response (CommandResponse (..))
import Control.Concurrent.Async qualified as GhcAsync
import Data.Either (Either (..))
import Data.Time.Clock.POSIX qualified as GhcPosix
import Float qualified
import GHC.Float (Double)
import GHC.Real qualified as GhcReal
import Int qualified
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
            dispatchResult <- Task.fromIO do
              GhcException.try @GhcException.SomeException
                (commands |> Task.forEach dispatchCommandPayload |> Task.runResult)
            case dispatchResult of
              Left exception -> case GhcException.fromException @GhcException.SomeAsyncException exception of
                Just asyncException -> Task.fromIO (GhcException.throwIO asyncException)
                Nothing -> do
                  completeRunner (IntegrationFailure IntegrationCallbackDispatchFailure)
                  Task.fromIO (GhcException.throwIO exception)
              Right (Ok _) -> completeRunner IntegrationProcessed
              Right (Err errorText) -> do
                completeRunner (IntegrationFailure IntegrationCallbackDispatchFailure)
                Task.throw errorText
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
    workerStates :: Array LifecycleRunnerWorkerState,
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
                else do
                  worker <- getOrCreateStatelessWorker dispatcher streamId
                  writeWorkerMessageWithTimeout dispatcher streamId (ProcessEvent event) worker.channel

              didQueueLifecycle <- if Array.isEmpty matchingLifecycleRunners
                then Task.yield True
                else do
                  lifecycleWorker <- getOrCreateLifecycleWorker dispatcher streamId eventEntityName
                  currentTime <- getCurrentTimeMs
                  _ <- lifecycleWorker.lastActivityTime |> ConcurrentVar.swap currentTime
                  writeWorkerMessageWithTimeout dispatcher streamId (ProcessEvent event) lifecycleWorker.channel

              Task.when (not didQueueStateless || not didQueueLifecycle) do
                let logContext = integrationLogContext event
                logIntegrationResult logContext IntegrationDropped


writeWorkerMessageWithTimeout ::
  IntegrationDispatcher ->
  StreamId ->
  WorkerMessage (Event Json.Value) ->
  Channel (WorkerMessage (Event Json.Value)) ->
  Task Text Bool
writeWorkerMessageWithTimeout dispatcher streamId message channel = do
  writeResult <-
    Channel.tryWriteWithTimeout dispatcher.config.channelWriteTimeoutMs message channel
  case writeResult of
    Ok _ -> Task.yield True
    Err _ -> do
      let messageLabel = workerMessageLabel message
      Log.warn [fmt|Dropped #{messageLabel} for stream #{toText streamId} (channel write timeout)|]
        |> Task.ignoreError
      Task.yield False


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
      Log.warn
        [fmt|Stop message timed out for stream #{toText streamId} (#{err}), force-cancelling worker|]
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
      Log.warn
        [fmt|Stop message timed out for lifecycle worker stream #{toText streamId} (#{err}), force-cancelling (cleanup may be skipped)|]
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
  Text -> -- ^ Entity type name for filtering which lifecycle runners to initialize
  Task Text LifecycleEntityWorker
getOrCreateLifecycleWorker dispatcher streamId entityTypeName = do
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
          candidateWorker <- spawnLifecycleWorker dispatcher streamId entityTypeName
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
      candidateWorker <- spawnLifecycleWorker dispatcher streamId entityTypeName
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

  workerTask <- AsyncTask.run safeWorkerLoop
  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Spawned stateless worker for stream #{toText streamId}|]
      |> Task.ignoreError
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
  Text -> -- ^ Entity type name — only lifecycle runners matching this type are initialized
  Task Text LifecycleEntityWorker
spawnLifecycleWorker dispatcher streamId entityTypeName = do
  -- Generate unique ID for this worker instance
  uniqueId <- Uuid.generate
  workerChannel <- Channel.newBounded dispatcher.config.workerChannelCapacity
  currentTime <- getCurrentTimeMs
  lastActivity <- ConcurrentVar.containing currentTime
  workerStatus <- AtomicVar.containing Active

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

  workerTask <- AsyncTask.run safeWorkerLoop
  Log.withScope [("component", "Dispatcher")] do
    Log.debug [fmt|Spawned lifecycle worker for stream #{toText streamId}|]
      |> Task.ignoreError
  Task.yield
    LifecycleEntityWorker
      { workerId = uniqueId,
        channel = workerChannel,
        workerTask = workerTask,
        lastActivityTime = lastActivity,
        workerStates = states,
        status = workerStatus
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
                -- Reaper race condition fix:
                -- 1. Mark worker as Draining (so dispatcher knows not to use it)
                -- 2. Atomically remove from map ONLY IF it's still the same worker
                --    (uses removeIf to prevent removing a newly-spawned replacement)
                -- 3. Send Stop (worker drains queue then exits)
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                Task.when (idleTime > dispatcher.config.idleTimeoutMs) do
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
                      Just removedWorker -> do
                        Log.debug [fmt|Reaped idle lifecycle worker for stream #{toText streamId}|]
                          |> Task.ignoreError
                        stopLifecycleWorkerOrCancel dispatcher streamId removedWorker
                      Nothing ->
                        -- Worker was replaced, don't send Stop to the new one
                        pass

            -- Reap idle STATELESS workers (same logic, simpler - no Draining status)
            dispatcher.entityWorkers
              |> ConcurrentMap.forEachChunked reaperChunkSize \streamId worker -> do
                lastActivity <- worker.lastActivityTime |> ConcurrentVar.peek
                let idleTime = currentTime - lastActivity
                Task.when (idleTime > dispatcher.config.idleTimeoutMs) do
                    -- Capture the worker ID we're trying to remove
                    let capturedWorkerId = worker.workerId
                    -- Atomically remove ONLY IF the worker ID matches
                    let isSameWorker currentWorker =
                          currentWorker.workerId == capturedWorkerId
                    removed <- dispatcher.entityWorkers
                      |> ConcurrentMap.removeIf streamId isSameWorker
                    -- Send Stop only if we actually removed the worker
                    case removed of
                      Just removedWorker -> do
                        Log.debug [fmt|Reaped idle worker for stream #{toText streamId}|]
                          |> Task.ignoreError
                        stopStatelessWorkerOrCancel dispatcher streamId removedWorker
                      Nothing ->
                        -- Worker was replaced, don't send Stop to the new one
                        pass

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
