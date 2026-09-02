module Service.Query.Subscriber (
  QuerySubscriber (..),
  Readiness (..),
  RebuildOptions (..),
  QueryRebuildError (..),
  new,
  newWithStore,
  newWithCheckpointStore,
  start,
  stop,
  rebuildAll,
  rebuildFrom,
  rebuildAllAsync,
  readinessOf,
  readinessOfQuery,
  rebuildOptionsDefault,
  queryHashFor,
  -- | Exported for testing the live-subscription checkpoint write path.
  processEventHandler,
) where

import Array (Array)
import Array qualified
import AsyncTask (RaceWinner (..))
import AsyncTask qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Data.Hashable qualified as Hashable
import DateTime qualified
import Json qualified
import Log qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event (Event (..))
import Service.Event.EntityName (EntityName)
import Service.Event.EventMetadata (EventMetadata (..))
import Service.Event.StreamPosition (StreamPosition (..))
import Service.EventStore (EventStore (..))
import Service.EventStore.Core (Error, Limit (..), ReadAllMessage (..), SubscriptionId)
import Service.Query.Registry (QueryRegistry, QueryUpdater (..))
import Service.Query.Registry qualified as Registry
import Service.QueryObjectStore.Core (QueryObjectStore (..))
import Service.QueryObjectStore.Postgres (CheckpointStore (..))
import Stream qualified
import Uuid qualified
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)


-- | The QuerySubscriber listens to events from the EventStore and
-- dispatches them to registered QueryUpdaters.
data QuerySubscriber = QuerySubscriber
  { eventStore :: EventStore Json.Value,
    registry :: QueryRegistry,
    lastProcessedPosition :: ConcurrentVar (Maybe StreamPosition),
    subscriptionId :: ConcurrentVar (Maybe SubscriptionId),
    -- | Per-query readiness state, keyed by queryName.
    queryReadiness :: ConcurrentVar (Map Text Readiness),
    -- | Optional persistent query object store for checkpoint resume.
    -- Nothing = in-memory only (tests); Just store = Postgres-backed (production).
    objectStore :: Maybe (QueryObjectStore Json.Value),
    -- | Optional checkpoint store for hash-aware checkpoint operations.
    --
    -- Carries resumeFromCheckpoint and deleteStaleHash helpers (ADR-0059 §Internal helpers).
    -- Nothing = skip hash checking (in-memory / test path).
    -- Just store = Postgres-backed checkpoint management.
    checkpointStore :: Maybe CheckpointStore
  }


-- | Create a new QuerySubscriber.
--
-- Pass Nothing for objectStore in tests (no checkpoint persistence).
-- Pass Just store in production to enable checkpoint resume via rebuildFrom.
new :: EventStore Json.Value -> QueryRegistry -> Task Text QuerySubscriber
new eventStore registry = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  queryReadiness <- ConcurrentVar.containing Map.empty
  Task.yield
    QuerySubscriber
      { eventStore,
        registry,
        lastProcessedPosition,
        subscriptionId,
        queryReadiness,
        objectStore = Nothing,
        checkpointStore = Nothing
      }


-- | Create a new QuerySubscriber with a persistent QueryObjectStore.
--
-- Use this when checkpoint persistence is needed (production wiring).
-- Enables rebuildFrom to resume from the last persisted position instead
-- of replaying from position 0 on every restart.
newWithStore
  :: EventStore Json.Value
  -> QueryRegistry
  -> QueryObjectStore Json.Value
  -> Task Text QuerySubscriber
newWithStore eventStore registry store = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  queryReadiness <- ConcurrentVar.containing Map.empty
  Task.yield
    QuerySubscriber
      { eventStore,
        registry,
        lastProcessedPosition,
        subscriptionId,
        queryReadiness,
        objectStore = Just store,
        checkpointStore = Nothing
      }


-- | Create a new QuerySubscriber with a CheckpointStore for hash-aware checkpoint ops.
--
-- Use this in tests that need to exercise the hash-mismatch path (H5) or in
-- production wiring where both a QueryObjectStore and a CheckpointStore are present.
-- The checkpointStore carries resumeFromCheckpoint and deleteStaleHash helpers.
newWithCheckpointStore
  :: EventStore Json.Value
  -> QueryRegistry
  -> CheckpointStore
  -> Task Text QuerySubscriber
newWithCheckpointStore eventStore registry cpStore = do
  lastProcessedPosition <- ConcurrentVar.containing Nothing
  subscriptionId <- ConcurrentVar.containing Nothing
  queryReadiness <- ConcurrentVar.containing Map.empty
  Task.yield
    QuerySubscriber
      { eventStore,
        registry,
        lastProcessedPosition,
        subscriptionId,
        queryReadiness,
        objectStore = Nothing,
        checkpointStore = Just cpStore
      }


-- | Rebuild all queries from the beginning of the event store.
-- Called on application startup before starting live subscription.
rebuildAll :: QuerySubscriber -> Task Text Unit
rebuildAll subscriber = do
  Log.withScope [("component", "QuerySubscriber")] do
    Log.info "Starting query rebuild from event store..."
      |> Task.ignoreError

  -- Read all events from the beginning (use large limit)
  messageStream <-
    subscriber.eventStore.readAllEventsForwardFrom (StreamPosition 0) (Limit 9223372036854775807)
      |> Task.mapError (toText :: Error -> Text)

  -- Process each message incrementally via Stream.consume
  messageStream
    |> Stream.consume
      ( \_ message -> do
          case message of
            AllEvent rawEvent -> do
              processEvent subscriber rawEvent
              -- Update last processed position
              case rawEvent.metadata.globalPosition of
                Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
                Nothing -> pass
            _ -> pass
          Task.yield unit
      )
      unit

  -- Log completion
  maybeLastPos <- ConcurrentVar.peek subscriber.lastProcessedPosition
  Log.withScope [("component", "QuerySubscriber")] do
    case maybeLastPos of
      Just pos ->
        Log.info [fmt|Query rebuild complete. Last position: #{pos}|]
          |> Task.ignoreError
      Nothing ->
        Log.info "Query rebuild complete. No events found."
          |> Task.ignoreError

  Task.yield unit


data ReplayPhase
  = ReplayInProgress
  | ReplayLive
  deriving (Eq, Show)


data ReplayState = ReplayState
  { phase :: ReplayPhase,
    highWaterPosition :: Maybe StreamPosition,
    pendingEvents :: Map StreamPosition (Event Json.Value),
    needsCatchUp :: Bool
  }


newtype ReplayCoordinator = ReplayCoordinator
  { replayState :: ConcurrentVar ReplayState
  }


-- | Maximum live overlap retained before positional recovery takes over.
replayInboxCapacity :: Int
replayInboxCapacity = 1000


-- | Allocate the internal replay/live ordering state.
newReplayCoordinator :: Task w ReplayCoordinator
newReplayCoordinator = do
  replayState <-
    ConcurrentVar.containing
      ReplayState
        { phase = ReplayInProgress,
          highWaterPosition = Nothing,
          pendingEvents = Map.empty,
          needsCatchUp = False
        }
  Task.yield ReplayCoordinator {replayState}


-- | Register live delivery first, then rebuild in the background.
-- Live events overlap the replay through a bounded positional inbox; the
-- coordinator drains that inbox before readiness can become Ready.
start :: QuerySubscriber -> Task Text Unit
start subscriber = do
  coordinator <- newReplayCoordinator
  setAllReadiness subscriber Rebuilding

  Log.withScope [("component", "QuerySubscriber")] do
    Log.info "Starting register-first query subscriber"
      |> Task.ignoreError

  subId <-
    subscriber.eventStore.subscribeToAllEvents
      (handleLiveEvent subscriber coordinator)
      |> Task.mapError (toText :: Error -> Text)
  subscriber.subscriptionId |> ConcurrentVar.modify (\_ -> Just subId)

  _rebuildTask <-
    AsyncTask.run (runCoordinatedRebuild subscriber coordinator rebuildOptionsDefault)
      |> Task.mapError (toText .> (\message -> [fmt|Could not start query replay: #{message}|]))
  Task.yield unit


-- | Stop the live subscription.
-- This unsubscribes from the event store, allowing graceful shutdown.
stop :: QuerySubscriber -> Task Text Unit
stop subscriber = do
  maybeSubId <- ConcurrentVar.peek subscriber.subscriptionId
  case maybeSubId of
    Just subId -> do
      subscriber.eventStore.unsubscribe subId
        |> Task.mapError (toText :: Error -> Text)
      subscriber.subscriptionId |> ConcurrentVar.modify (\_ -> Nothing)
    Nothing -> Task.yield unit


-- | Handler wrapper for subscription callback.
-- Processes the event and updates lastProcessedPosition, mirroring rebuildAll behavior.
processEventHandler :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEventHandler subscriber rawEvent = do
  processEvent subscriber rawEvent
  recordProcessedEvent subscriber rawEvent
  writeLiveCheckpoints subscriber rawEvent


-- | Rebuild handler that propagates updater failures into readiness.
processEventHandlerStrict :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEventHandlerStrict subscriber rawEvent = do
  processEventStrict subscriber rawEvent
  recordProcessedEvent subscriber rawEvent


-- | Advance the in-process cursor without multiplying replay checkpoint writes.
recordProcessedEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
recordProcessedEvent subscriber rawEvent = do
  case rawEvent.metadata.globalPosition of
    Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
    Nothing -> pass


-- | Preserve the existing live-subscription checkpoint contract.
writeLiveCheckpoints :: QuerySubscriber -> Event Json.Value -> Task Text Unit
writeLiveCheckpoints subscriber rawEvent =
  case (subscriber.checkpointStore, rawEvent.metadata.globalPosition) of
    (Just cpStore, Just (StreamPosition position)) -> do
      getAllQueryNames subscriber.registry
        |> Task.forEach
          ( \queryName ->
              cpStore.writeCheckpoint queryName (queryHashFor queryName) position
                |> Task.ignoreError
          )
    _ -> pass


-- | Readiness state of a query rebuild.
data Readiness
  = Rebuilding
  | Ready
  | Failed Text
  deriving (Eq, Show, Generic)


-- | Options controlling a per-query rebuild.
data RebuildOptions = RebuildOptions
  { chunkSize :: Int
    -- ^ Events per fetch (default: 1000).
  , timeout :: Int
    -- ^ Per-query rebuild timeout in seconds (default: 300).
  , logProgress :: Bool
    -- ^ Emit a log message after each chunk (default: True).
  , deleteStaleHashFirst :: Bool
    -- ^ Delete rows with mismatched query_hash before replaying (default: True).
  }
  deriving (Eq, Show)


-- | Errors produced during a query rebuild.
data QueryRebuildError
  = RebuildTimeout Text
    -- ^ Rebuild took longer than the configured timeout.
  | UpdaterException Text
    -- ^ QueryUpdater returned Err during replay.
  | HashMismatchReplay Text
    -- ^ Hash mismatch forced a replay, but the replay itself failed.
  | CheckpointFetchFailed Text
    -- ^ Could not read the resume position from the object store.
  | CheckpointWriteFailed Text
    -- ^ Could not persist the new checkpoint after a successful rebuild.
  | EventStoreFailed Text
    -- ^ EventStore.readFrom returned Err.
  deriving (Eq, Show, Generic)


-- | Default rebuild options.
rebuildOptionsDefault :: RebuildOptions
rebuildOptionsDefault = RebuildOptions
  { chunkSize = 1000
  , timeout = 300
  , logProgress = True
  , deleteStaleHashFirst = True
  }


-- | Resumable per-query rebuild from a given StreamPosition.
--
-- Reads events from the EventStore in chunks, applies them to registered
-- query updaters for the named query, and tracks progress.
--
-- Error variants:
--   CheckpointFetchFailed — when store.get returns Err (store unavailable)
--   EventStoreFailed      — when EventStore.readAllEventsForwardFrom returns Err
--   UpdaterException      — when any QueryUpdater.updateQuery returns Err
--   HashMismatchReplay    — reserved for hash-mismatch deletion + replay failure
--   RebuildTimeout        — when the rebuild exceeds options.timeout seconds
rebuildFrom
  :: QuerySubscriber
  -> Text
  -> StreamPosition
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildFrom subscriber queryName startPosition options = do
  -- Wrap the actual rebuild in a timeout race.
  let timeoutSec = options.timeout
  let timeoutMs = timeoutSec * 1000
  raceResult <-
    AsyncTask.race
      (rebuildFromInner subscriber queryName startPosition options)
      (AsyncTask.sleep timeoutMs |> Task.mapError (\(_ :: Text) -> RebuildTimeout "sleep failed"))
  case raceResult of
    LeftWon _ -> pass
    RightWon _ -> do
      -- Timeout: flip readiness to Failed
      let timeoutMsg = [fmt|Rebuild timeout (> #{timeoutSec}s): #{queryName}|]
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName (Failed timeoutMsg))
      Task.throw (RebuildTimeout [fmt|Query #{queryName} rebuild timed out after #{timeoutSec}s|])


-- | Inner rebuild implementation (no timeout wrapper).
--
-- Wires together:
--   1. CheckpointFetchFailed — if objectStore.get fails on startup check
--   2. Hash-mismatch detection — via checkpointStore.resumeFromCheckpoint
--   3. deleteStaleHash — clean up mismatched rows before replay
--   4. HashMismatchReplay — if deletion succeeded but replay itself failed (H5)
--   5. EventStoreFailed — if EventStore.readAllEventsForwardFrom fails (normal path)
--   6. UpdaterException — if any QueryUpdater.updateQuery returns Err
rebuildFromInner
  :: QuerySubscriber
  -> Text
  -> StreamPosition
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildFromInner subscriber queryName startPosition options = do
  -- Step 1: If an object store is configured, probe its availability.
  -- Uuid.nil is used as a no-op probe key: it is guaranteed never to be
  -- a real instance UUID (Uuid.generate uses v4 random), so the lookup
  -- either returns Nothing (store is up) or fails (store is unavailable).
  -- A store.get failure raises CheckpointFetchFailed.
  -- (A dedicated `healthCheck` trait method would be cleaner; this is
  -- the minimum-disruption path until the trait is extended.)
  case subscriber.objectStore of
    Just store -> do
      _ <- store.get Uuid.nil
        |> Task.mapError (\err -> CheckpointFetchFailed (toText (show err)))
      pass
    Nothing -> pass

  -- Step 2: Determine the effective start position via checkpoint + hash check.
  -- Returns (startPos, wentThroughHashMismatch) so the caller can raise the
  -- correct error if replay fails after a hash-mismatch deletion (H5).
  (effectiveStart, hashMismatchOccurred) <-
    resolveStartPosition subscriber queryName startPosition options

  -- Mark query as rebuilding.
  subscriber.queryReadiness
    |> ConcurrentVar.modify (Map.set queryName Rebuilding)

  -- Step 3: Read every page and route only to this query's updaters.
  let updaters = getUpdatersForQuery queryName subscriber.registry
  pageResult <-
    rebuildQueryPages
      subscriber
      queryName
      updaters
      options
      hashMismatchOccurred
      effectiveStart
      0
      |> Task.asResult

  case pageResult of
    Err replayError -> do
      let failure = sanitizedFailure replayError
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName (Failed failure))
      Task.throw replayError
    Ok finalPos -> do
      checkpointResult <- case subscriber.checkpointStore of
        Just cpStore ->
          cpStore.writeCheckpoint queryName (queryHashFor queryName) finalPos
            |> Task.mapError (toText .> CheckpointWriteFailed)
            |> Task.asResult
        Nothing -> Task.yield (Ok unit)
      case checkpointResult of
        Err checkpointError -> do
          let failure = sanitizedFailure checkpointError
          subscriber.queryReadiness
            |> ConcurrentVar.modify (Map.set queryName (Failed failure))
          Task.throw checkpointError
        Ok _ ->
          subscriber.queryReadiness
            |> ConcurrentVar.modify (Map.set queryName Ready)


rebuildQueryPages
  :: QuerySubscriber
  -> Text
  -> Array QueryUpdater
  -> RebuildOptions
  -> Bool
  -> StreamPosition
  -> Int64
  -> Task QueryRebuildError Int64
rebuildQueryPages subscriber queryName updaters options hashMismatchOccurred startPosition lastPosition = do
  streamResult <-
    subscriber.eventStore.readAllEventsForwardFrom
      startPosition
      (Limit (fromIntegral options.chunkSize))
      |> Task.asResult
  messageStream <- case streamResult of
    Err err ->
      if hashMismatchOccurred then
        Task.throw
          (HashMismatchReplay [fmt|Query #{queryName}: stale hash deleted, but replay failed|])
      else
        Task.throw (EventStoreFailed (toText err))
    Ok stream -> Task.yield stream
  pageResult <-
    processEventsForQuery updaters options messageStream
      |> Task.mapError UpdaterException
  let (pageLastPosition, pageCount) = pageResult
  let finalPosition = max lastPosition pageLastPosition
  if pageCount < options.chunkSize then
    Task.yield finalPosition
  else
    rebuildQueryPages
      subscriber
      queryName
      updaters
      options
      hashMismatchOccurred
      (StreamPosition (pageLastPosition + 1))
      finalPosition


-- | Resolve the effective start position, performing hash-mismatch cleanup if needed.
--
-- Returns (startPosition, wentThroughHashMismatch):
--   - wentThroughHashMismatch = True means deleteStaleHash was called successfully;
--     if the subsequent replay fails, the error must be HashMismatchReplay (H5).
--
-- When a checkpointStore is present:
--   - Call resumeFromCheckpoint queryName currentHash
--   - If rows match (same hash): resume from the stored MIN(position), no mismatch
--   - If no matching rows (first run or hash mismatch): call deleteStaleHash to
--     wipe outdated rows, then replay from position 0; mismatch = True
--
-- When no checkpointStore is present (in-memory / test path): use startPosition, no mismatch.
resolveStartPosition
  :: QuerySubscriber
  -> Text
  -> StreamPosition
  -> RebuildOptions
  -> Task QueryRebuildError (StreamPosition, Bool)
resolveStartPosition subscriber queryName startPosition _options =
  case subscriber.checkpointStore of
    Nothing -> Task.yield (startPosition, False)
    Just cpStore -> do
      -- ADR-0059 §H5: schema-evolution detection. The hash must match the
      -- compile-time value emitted by `deriveQuery` (Service.Query.TH), which
      -- is `Hashable.hash (queryName :: String)`. We reproduce that exact
      -- computation here at runtime so a checkpoint row written by an older
      -- compiled binary is detected as "matching" by a newer one (and vice
      -- versa) iff the query name is unchanged.
      let currentHash = queryHashFor queryName
      resumeResult <-
        cpStore.resumeFromCheckpoint queryName currentHash
          |> Task.mapError (\err -> CheckpointFetchFailed (toText (show err)))
      case resumeResult of
        Just minPos ->
          -- Reads are inclusive, so resume at the event after the committed checkpoint.
          Task.yield (StreamPosition (minPos + 1), False)
        Nothing -> do
          -- No matching rows: delete any stale rows (hash mismatch / first run).
          _ <-
            cpStore.deleteStaleHash queryName currentHash
              |> Task.mapError (\err -> CheckpointFetchFailed (toText (show err)))
          Task.yield (StreamPosition 0, True)


-- | Compute the schema-evolution hash for a query name.
--
-- Must produce the same value as the compile-time hash emitted by
-- `Service.Query.TH.deriveKnownHash` (and the equivalent in
-- `Service.CommandExecutor.TH`), which both use `Hashable.hash` on the
-- query name in its `String` form. We mirror that exact pipeline:
-- Text → Array Char → [Char] → Hashable.hash :: Int → Text.
queryHashFor :: Text -> Text
queryHashFor queryName =
  queryName
    |> Text.toArray
    |> Array.toLinkedList
    |> Hashable.hash
    |> show
    |> toText


-- | Process one page for a specific set of updaters.
-- Returns the maximum global position and number of events consumed.
processEventsForQuery
  :: Array QueryUpdater
  -> RebuildOptions
  -> Stream.Stream (ReadAllMessage Json.Value)
  -> Task Text (Int64, Int)
processEventsForQuery updaters _options messageStream =
  messageStream
    |> Stream.consume
        ( \(lastPos, count) message ->
            case message of
              AllEvent rawEvent -> do
                applyEvent updaters rawEvent
                case rawEvent.metadata.globalPosition of
                  Just (StreamPosition position) -> Task.yield (max lastPos position, count + 1)
                  Nothing -> Task.yield (lastPos, count + 1)
              _ -> Task.yield (lastPos, count)
        )
        (0, 0)


-- | Apply an event to a set of updaters.
-- Propagates the first updater error as Err Text so rebuildFrom can
-- record it as UpdaterException. Logs a WARN for observability.
applyEvent
  :: Array QueryUpdater
  -> Event Json.Value
  -> Task Text Unit
applyEvent updaters rawEvent =
  updaters
    |> Task.forEach \updater -> do
        let updaterName = updater.queryName
        result <- updater.updateQuery rawEvent |> Task.asResult
        case result of
          Ok _ -> pass
          Err err -> do
            let position = case rawEvent.metadata.globalPosition of
                  Just globalPosition -> toText globalPosition
                  Nothing -> "unknown"
            Log.withScope [("queryName", updaterName), ("position", position)] do
              Log.warn "Query updater failed"
                |> Task.ignoreError
            Task.throw err


-- | Get all QueryUpdaters across all entities that belong to the named query.
getUpdatersForQuery :: Text -> QueryRegistry -> Array QueryUpdater
getUpdatersForQuery queryName registry =
  Registry.getAllUpdaters registry
    |> Array.takeIf (\updater -> updater.queryName == queryName)


-- | Replace readiness with one state for every registered query.
setAllReadiness :: QuerySubscriber -> Readiness -> Task w Unit
setAllReadiness subscriber readiness = do
  let queryNames = getAllQueryNames subscriber.registry
  subscriber.queryReadiness
    |> ConcurrentVar.modify
      ( \_ ->
          queryNames
            |> Array.reduce (\queryName states -> states |> Map.set queryName readiness) Map.empty
      )


-- | Buffer overlap during replay and serialize delivery after it reaches head.
handleLiveEvent :: QuerySubscriber -> ReplayCoordinator -> Event Json.Value -> Task Text Unit
handleLiveEvent subscriber coordinator rawEvent = do
  outcome <-
    coordinator.replayState
      |> ConcurrentVar.modifyReturning (coordinateQueryLiveEvent rawEvent)
  deliverCoordinatedLiveEvent subscriber outcome


coordinateQueryLiveEvent
  :: Event Json.Value
  -> ReplayState
  -> Task w (ReplayState, Result Text (Maybe (Event Json.Value)))
coordinateQueryLiveEvent rawEvent state =
  case rawEvent.metadata.globalPosition of
    Nothing -> Task.yield (state, Err "Live event had no global position")
    Just position ->
      if queryReplayPositionProcessed state position then
        Task.yield (state, Ok Nothing)
      else
        coordinateQueryReplayPhase rawEvent position state


coordinateQueryReplayPhase
  :: Event Json.Value
  -> StreamPosition
  -> ReplayState
  -> Task w (ReplayState, Result Text (Maybe (Event Json.Value)))
coordinateQueryReplayPhase rawEvent position state =
  case state.phase of
    ReplayInProgress -> bufferQueryReplayOverlap rawEvent position state
    ReplayLive ->
      Task.yield
        ( state {highWaterPosition = Just position}
        , Ok (Just rawEvent)
        )


bufferQueryReplayOverlap
  :: Event Json.Value
  -> StreamPosition
  -> ReplayState
  -> Task w (ReplayState, Result Text (Maybe (Event Json.Value)))
bufferQueryReplayOverlap rawEvent position state =
  if state.needsCatchUp || Map.length state.pendingEvents >= replayInboxCapacity then
    Task.yield (state {needsCatchUp = True}, Ok Nothing)
  else
    Task.yield
      ( state {pendingEvents = state.pendingEvents |> Map.set position rawEvent}
      , Ok Nothing
      )


queryReplayPositionProcessed :: ReplayState -> StreamPosition -> Bool
queryReplayPositionProcessed state position =
  case state.highWaterPosition of
    Just highWater -> position <= highWater
    Nothing -> False


deliverCoordinatedLiveEvent
  :: QuerySubscriber
  -> Result Text (Maybe (Event Json.Value))
  -> Task Text Unit
deliverCoordinatedLiveEvent subscriber outcome =
  case outcome of
    Ok (Just eventToProcess) -> processEventHandler subscriber eventToProcess
    Ok Nothing -> Task.yield unit
    Err err -> Task.throw err


processReplayEvent
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> Event Json.Value
  -> Task Text Unit
processReplayEvent subscriber maybeCoordinator rawEvent =
  case maybeCoordinator of
    Nothing -> processEventHandlerStrict subscriber rawEvent
    Just coordinator -> processCoordinatedReplayEvent subscriber coordinator rawEvent


processCoordinatedReplayEvent
  :: QuerySubscriber
  -> ReplayCoordinator
  -> Event Json.Value
  -> Task Text Unit
processCoordinatedReplayEvent subscriber coordinator rawEvent = do
  state <- ConcurrentVar.peek coordinator.replayState
  case rawEvent.metadata.globalPosition of
    Nothing -> Task.throw "Replay event had no global position"
    Just position ->
      if queryReplayPositionProcessed state position then
        Task.yield unit
      else do
        -- Updaters run outside the coordinator so live events can enter the inbox.
        processEventHandlerStrict subscriber rawEvent
        advanceQueryReplayHighWater coordinator position


advanceQueryReplayHighWater :: ReplayCoordinator -> StreamPosition -> Task w Unit
advanceQueryReplayHighWater coordinator position =
  coordinator.replayState
    |> ConcurrentVar.modify
      ( \current ->
          case current.highWaterPosition of
            Just highWater -> current {highWaterPosition = Just (max highWater position)}
            Nothing -> current {highWaterPosition = Just position}
      )


data ReplayStats = ReplayStats
  { replayedCount :: Int,
    replayedThrough :: Maybe StreamPosition
  }


-- | Inclusive read position immediately after the last successful event.
nextReplayPosition :: QuerySubscriber -> Task w StreamPosition
nextReplayPosition subscriber = do
  lastPosition <- ConcurrentVar.peek subscriber.lastProcessedPosition
  case lastPosition of
    Just (StreamPosition position) -> Task.yield (StreamPosition (position + 1))
    Nothing -> Task.yield (StreamPosition 0)


-- | Read the last matching global position with one bounded reverse lookup.
readReplayHead
  :: QuerySubscriber
  -> Array EntityName
  -> Task QueryRebuildError (Maybe StreamPosition)
readReplayHead subscriber entityNames = do
  messageStream <-
    subscriber.eventStore.readAllEventsBackwardFromFiltered
      (StreamPosition 9223372036854775807)
      (Limit 1)
      entityNames
      |> Task.mapError (toText .> EventStoreFailed)
  messageStream
    |> Stream.consume
      ( \current message ->
          case (current, message) of
            (Just position, _) -> Task.yield (Just position)
            (Nothing, AllEvent rawEvent) -> Task.yield rawEvent.metadata.globalPosition
            _ -> Task.yield Nothing
      )
      Nothing
    |> Task.mapError (toText .> EventStoreFailed)


runReplayPages
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> RebuildOptions
  -> StreamPosition
  -> ReplayStats
  -> ConcurrentVar Int
  -> Maybe StreamPosition
  -> DateTime.DateTime
  -> Task QueryRebuildError ReplayStats
runReplayPages subscriber maybeCoordinator options startPosition stats replayedCountRef replayHead startedAt = do
  let entityNames = Registry.registeredEntityNames subscriber.registry
  if Array.isEmpty entityNames then
    Task.yield stats
  else do
    pageResult <- consumeReplayPage subscriber maybeCoordinator options startPosition stats replayedCountRef replayHead entityNames
    nextStats <- recordReplayProgress options startPosition replayHead startedAt replayedCountRef pageResult
    continueReplayPages subscriber maybeCoordinator options replayedCountRef replayHead startedAt pageResult nextStats


consumeReplayPage
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> RebuildOptions
  -> StreamPosition
  -> ReplayStats
  -> ConcurrentVar Int
  -> Maybe StreamPosition
  -> Array EntityName
  -> Task QueryRebuildError (Maybe StreamPosition, Int)
consumeReplayPage subscriber maybeCoordinator options startPosition stats replayedCountRef replayHead entityNames = do
  messageStream <-
    subscriber.eventStore.readAllEventsForwardFromFiltered
      startPosition
      (Limit (fromIntegral options.chunkSize))
      entityNames
      |> Task.mapError (toText .> EventStoreFailed)
  messageStream
    |> Stream.consume
      (consumeReplayMessage subscriber maybeCoordinator replayedCountRef replayHead)
      (stats.replayedThrough, 0)
    |> Task.mapError UpdaterException


consumeReplayMessage
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> ConcurrentVar Int
  -> Maybe StreamPosition
  -> (Maybe StreamPosition, Int)
  -> ReadAllMessage Json.Value
  -> Task Text (Maybe StreamPosition, Int)
consumeReplayMessage subscriber maybeCoordinator replayedCountRef replayHead (lastPosition, count) message =
  case message of
    AllEvent rawEvent ->
      if eventIsWithinReplayHead replayHead rawEvent then do
        processReplayEvent subscriber maybeCoordinator rawEvent
        replayedCountRef |> ConcurrentVar.modify (\replayedCount -> replayedCount + 1)
        Task.yield (rawEvent.metadata.globalPosition, count + 1)
      else
        Task.yield (lastPosition, count)
    _ -> Task.yield (lastPosition, count)


eventIsWithinReplayHead :: Maybe StreamPosition -> Event Json.Value -> Bool
eventIsWithinReplayHead replayHead rawEvent =
  case replayHead of
    Nothing -> False
    Just headPosition ->
      case rawEvent.metadata.globalPosition of
        Just eventPosition -> eventPosition <= headPosition
        Nothing -> True


recordReplayProgress
  :: RebuildOptions
  -> StreamPosition
  -> Maybe StreamPosition
  -> DateTime.DateTime
  -> ConcurrentVar Int
  -> (Maybe StreamPosition, Int)
  -> Task w ReplayStats
recordReplayProgress options startPosition replayHead startedAt replayedCountRef (lastPosition, _pageCount) = do
  totalCount <- ConcurrentVar.peek replayedCountRef
  finishedAt <- DateTime.now
  let durationSeconds = DateTime.toEpochSeconds finishedAt - DateTime.toEpochSeconds startedAt
  let lagFromHead = replayLagFromHead startPosition replayHead lastPosition
  Task.when options.logProgress do
    Log.info
      [fmt|Query replay progress events_replayed=#{totalCount} lag_from_head=#{lagFromHead} duration_seconds=#{durationSeconds}|]
      |> Task.ignoreError
  Task.yield ReplayStats {replayedCount = totalCount, replayedThrough = lastPosition}


replayLagFromHead :: StreamPosition -> Maybe StreamPosition -> Maybe StreamPosition -> Int64
replayLagFromHead startPosition replayHead lastPosition =
  case (replayHead, lastPosition) of
    (Just (StreamPosition headPosition), Just (StreamPosition processedPosition)) ->
      max 0 (headPosition - processedPosition)
    (Just (StreamPosition headPosition), Nothing) -> do
      let (StreamPosition requestedPosition) = startPosition
      max 0 (headPosition - requestedPosition + 1)
    _ -> 0


continueReplayPages
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> RebuildOptions
  -> ConcurrentVar Int
  -> Maybe StreamPosition
  -> DateTime.DateTime
  -> (Maybe StreamPosition, Int)
  -> ReplayStats
  -> Task QueryRebuildError ReplayStats
continueReplayPages subscriber maybeCoordinator options replayedCountRef replayHead startedAt (_lastPosition, pageCount) nextStats =
  if pageCount < options.chunkSize then
    Task.yield nextStats
  else
    case nextStats.replayedThrough of
      Nothing -> Task.yield nextStats
      Just (StreamPosition position) ->
        runReplayPages
          subscriber
          maybeCoordinator
          options
          (StreamPosition (position + 1))
          nextStats
          replayedCountRef
          replayHead
          startedAt


-- | Persist the final replay position for each query when configured.
writeFinalCheckpoints :: QuerySubscriber -> ReplayStats -> Task QueryRebuildError Unit
writeFinalCheckpoints subscriber stats = do
  lastProcessed <- ConcurrentVar.peek subscriber.lastProcessedPosition
  let checkpointPosition = case stats.replayedThrough of
        Just position -> Just position
        Nothing -> lastProcessed
  case (subscriber.checkpointStore, checkpointPosition) of
    (Just checkpointStore, Just (StreamPosition position)) -> do
      getAllQueryNames subscriber.registry
        |> Task.forEach
          ( \queryName ->
              checkpointStore.writeCheckpoint queryName (queryHashFor queryName) position
                |> Task.mapError (toText .> CheckpointWriteFailed)
          )
    _ -> Task.yield unit


-- | Fixed bounded retry schedule in milliseconds.
retryDelay :: Int -> Maybe Int
retryDelay retryIndex =
  case retryIndex of
    0 -> Just 100
    1 -> Just 500
    2 -> Just 2000
    _ -> Nothing


-- | Failures safe to retry without re-running a deterministic updater error.
isTransientReplayError :: QueryRebuildError -> Bool
isTransientReplayError replayError =
  case replayError of
    EventStoreFailed _ -> True
    CheckpointFetchFailed _ -> True
    CheckpointWriteFailed _ -> True
    _ -> False


runReplayWithRetries
  :: QuerySubscriber
  -> Maybe ReplayCoordinator
  -> RebuildOptions
  -> Int
  -> ConcurrentVar Int
  -> DateTime.DateTime
  -> Task QueryRebuildError ReplayStats
runReplayWithRetries subscriber maybeCoordinator options retryIndex replayedCountRef startedAt = do
  startPosition <- nextReplayPosition subscriber
  let entityNames = Registry.registeredEntityNames subscriber.registry
  result <-
    readReplayHead subscriber entityNames
      |> Task.andThen
        ( \replayHead ->
            runReplayPages
              subscriber
              maybeCoordinator
              options
              startPosition
              ReplayStats {replayedCount = 0, replayedThrough = Nothing}
              replayedCountRef
              replayHead
              startedAt
        )
      |> Task.andThen (\stats -> writeFinalCheckpoints subscriber stats |> Task.map (\_ -> stats))
      |> Task.asResult
  case result of
    Ok stats -> Task.yield stats
    Err replayError ->
      case (isTransientReplayError replayError, retryDelay retryIndex) of
        (True, Just delayMs) -> do
          Log.warn [fmt|Transient query replay failure; retrying after #{delayMs}ms|]
            |> Task.ignoreError
          AsyncTask.sleep delayMs
            |> Task.mapError (\_ -> replayError)
          runReplayWithRetries subscriber maybeCoordinator options (retryIndex + 1) replayedCountRef startedAt
        _ -> Task.throw replayError


-- | Atomically begin one overflow-recovery pass.
clearCatchUpFlag :: ReplayCoordinator -> Task w (Bool, Maybe StreamPosition)
clearCatchUpFlag coordinator =
  coordinator.replayState
    |> ConcurrentVar.modifyReturning \state ->
        Task.yield
          ( state {needsCatchUp = False}
          , (state.needsCatchUp, state.highWaterPosition)
          )


finishReplayCoordinator
  :: QuerySubscriber
  -> ReplayCoordinator
  -> RebuildOptions
  -> ConcurrentVar Int
  -> Task QueryRebuildError Unit
finishReplayCoordinator subscriber coordinator options replayedCountRef = do
  (catchUpRequired, highWater) <- clearCatchUpFlag coordinator
  if catchUpRequired then do
    recoverQueryReplayOverflow subscriber coordinator options replayedCountRef highWater
    finishReplayCoordinator subscriber coordinator options replayedCountRef
  else do
    entries <- takePendingReplayEvents coordinator
    drainPendingReplayEvents subscriber coordinator entries
    Task.unless (Array.isEmpty entries) do
      finishReplayCoordinator subscriber coordinator options replayedCountRef


recoverQueryReplayOverflow
  :: QuerySubscriber
  -> ReplayCoordinator
  -> RebuildOptions
  -> ConcurrentVar Int
  -> Maybe StreamPosition
  -> Task QueryRebuildError Unit
recoverQueryReplayOverflow subscriber coordinator options replayedCountRef highWater = do
  let catchUpStart = case highWater of
        Just (StreamPosition position) -> StreamPosition (position + 1)
        Nothing -> StreamPosition 0
  startedAt <- DateTime.now
  let entityNames = Registry.registeredEntityNames subscriber.registry
  replayHead <- readReplayHead subscriber entityNames
  runReplayPages
    subscriber
    (Just coordinator)
    options
    catchUpStart
    ReplayStats {replayedCount = 0, replayedThrough = highWater}
    replayedCountRef
    replayHead
    startedAt
    |> discard


takePendingReplayEvents :: ReplayCoordinator -> Task w (Array (StreamPosition, Event Json.Value))
takePendingReplayEvents coordinator =
  coordinator.replayState
    |> ConcurrentVar.modifyReturning \state -> do
        let pendingEntries = state.pendingEvents |> Map.entries
        if Array.isEmpty pendingEntries then
          Task.yield
            ( state
                { phase = ReplayLive
                , pendingEvents = Map.empty
                , needsCatchUp = False
                }
            , pendingEntries
            )
        else
          Task.yield (state {pendingEvents = Map.empty}, pendingEntries)


drainPendingReplayEvents
  :: QuerySubscriber
  -> ReplayCoordinator
  -> Array (StreamPosition, Event Json.Value)
  -> Task QueryRebuildError Unit
drainPendingReplayEvents subscriber coordinator entries =
  entries
    |> Task.forEach
      ( \(_, pendingEvent) ->
          processReplayEvent subscriber (Just coordinator) pendingEvent
            |> Task.mapError UpdaterException
      )


-- | Choose bounded structured context for aggregate replay failure logs.
replayFailureContext :: QuerySubscriber -> Task w (Text, Text)
replayFailureContext subscriber = do
  lastPosition <- ConcurrentVar.peek subscriber.lastProcessedPosition
  let queryName = "all"
  let position = case lastPosition of
        Just processedPosition -> toText processedPosition
        Nothing -> "0"
  Task.yield (queryName, position)


-- | Operator/client-safe readiness reason with database internals removed.
sanitizedFailure :: QueryRebuildError -> Text
sanitizedFailure replayError =
  case replayError of
    UpdaterException _ -> "Query updater failed"
    RebuildTimeout _ -> "Query replay timed out"
    HashMismatchReplay _ -> "Query replay failed after schema change"
    CheckpointFetchFailed _ -> "Query checkpoint read failed"
    CheckpointWriteFailed _ -> "Query checkpoint write failed"
    EventStoreFailed _ -> "Event store replay failed"


runCoordinatedRebuild
  :: QuerySubscriber
  -> ReplayCoordinator
  -> RebuildOptions
  -> Task QueryRebuildError Unit
runCoordinatedRebuild subscriber coordinator options = do
  startedAt <- DateTime.now
  replayedCountRef <- ConcurrentVar.containing (0 :: Int)
  replayResult <-
    runReplayWithRetries subscriber (Just coordinator) options 0 replayedCountRef startedAt
      |> Task.asResult
  requireCoordinatedReplaySuccess subscriber "Query replay failed" replayResult
  drainResult <-
    finishReplayCoordinator subscriber coordinator options replayedCountRef
      |> Task.asResult
  requireCoordinatedReplaySuccess subscriber "Query replay overlap drain failed" drainResult
  completeCoordinatedReplay subscriber replayedCountRef startedAt


requireCoordinatedReplaySuccess
  :: QuerySubscriber
  -> Text
  -> Result QueryRebuildError value
  -> Task QueryRebuildError Unit
requireCoordinatedReplaySuccess subscriber message result =
  case result of
    Ok _ -> Task.yield unit
    Err replayError -> recordCoordinatedReplayFailure subscriber message replayError


recordCoordinatedReplayFailure
  :: QuerySubscriber
  -> Text
  -> QueryRebuildError
  -> Task QueryRebuildError Unit
recordCoordinatedReplayFailure subscriber message replayError = do
  let failure = sanitizedFailure replayError
  setAllReadiness subscriber (Failed failure)
  (queryName, position) <- replayFailureContext subscriber
  Log.withScope [("queryName", queryName), ("position", position)] do
    Log.warn [fmt|#{message}: #{failure}|] |> Task.ignoreError
  Task.throw replayError


completeCoordinatedReplay
  :: QuerySubscriber
  -> ConcurrentVar Int
  -> DateTime.DateTime
  -> Task w Unit
completeCoordinatedReplay subscriber replayedCountRef startedAt = do
  setAllReadiness subscriber Ready
  completedCount <- ConcurrentVar.peek replayedCountRef
  finishedAt <- DateTime.now
  let durationSeconds = DateTime.toEpochSeconds finishedAt - DateTime.toEpochSeconds startedAt
  Log.info
    [fmt|Query replay complete events_replayed=#{completedCount} lag_from_head=0 duration_seconds=#{durationSeconds}|]
    |> Task.ignoreError


-- | Rebuild all registered queries in one entity-filtered, paged pass.
-- A failure is recorded in readiness and does not escape; callers inspect
-- readinessOf/readinessOfQuery for the operational result.
rebuildAllAsync
  :: QuerySubscriber
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildAllAsync subscriber options = do
  setAllReadiness subscriber Rebuilding
  startedAt <- DateTime.now
  replayedCountRef <- ConcurrentVar.containing (0 :: Int)
  result <- runReplayWithRetries subscriber Nothing options 0 replayedCountRef startedAt |> Task.asResult
  case result of
    Ok _ -> setAllReadiness subscriber Ready
    Err replayError -> do
      let failure = sanitizedFailure replayError
      setAllReadiness subscriber (Failed failure)
      Log.warn [fmt|Query replay failed: #{failure}|] |> Task.ignoreError


-- | Fetch the aggregate readiness state of all registered queries.
--
-- Returns:
-- - Ready    if all registered queries are Ready (or none are registered)
-- - Rebuilding if any query is Rebuilding
-- - Failed   if any query has Failed (and none are Rebuilding)
readinessOf
  :: QuerySubscriber
  -> Task Text Readiness
readinessOf subscriber = do
  readinessMap <- ConcurrentVar.peek subscriber.queryReadiness
  let states = readinessMap |> Map.values
  Task.yield (aggregateReadiness states)


-- | Fetch the readiness state for a specific named query.
--
-- Returns Nothing if the query is not registered.
readinessOfQuery
  :: QuerySubscriber
  -> Text
  -> Task Text (Maybe Readiness)
readinessOfQuery subscriber queryName = do
  readinessMap <- ConcurrentVar.peek subscriber.queryReadiness
  Task.yield (readinessMap |> Map.get queryName)


-- | Aggregate per-query readiness values into a single Readiness.
aggregateReadiness :: Array Readiness -> Readiness
aggregateReadiness states =
  states
    |> Array.reduce
        (\state acc ->
          case acc of
            Failed reason -> Failed reason
            Rebuilding ->
              case state of
                Failed reason -> Failed reason
                _ -> Rebuilding
            Ready ->
              case state of
                Rebuilding -> Rebuilding
                Failed reason -> Failed reason
                Ready -> Ready)
        Ready


-- | Get all unique query names from the registry.
getAllQueryNames :: QueryRegistry -> Array Text
getAllQueryNames registry =
  Registry.getAllUpdaters registry
    |> Array.map (\updater -> updater.queryName)
    |> deduplicateTexts


-- | Remove duplicate Text values from an Array, preserving last-occurrence order.
deduplicateTexts :: Array Text -> Array Text
deduplicateTexts arr =
  arr
    |> Array.reduce
        (\name acc ->
          if Array.contains name acc
            then acc
            else Array.push name acc)
        Array.empty


-- | Process a live/legacy event without letting one updater terminate the
-- subscription. Rebuild uses processEventStrict so readiness can expose failure.
processEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEvent subscriber rawEvent = do
  let updaters = Registry.getUpdatersForEntity rawEvent.entityName subscriber.registry
  updaters
    |> Task.forEach \updater -> do
      result <- updater.updateQuery rawEvent |> Task.asResult
      case result of
        Ok _ -> Task.yield unit
        Err _ -> do
          let updaterName = updater.queryName
          let position = case rawEvent.metadata.globalPosition of
                Just globalPosition -> toText globalPosition
                Nothing -> "unknown"
          Log.withScope [("component", "QuerySubscriber"), ("queryName", updaterName), ("position", position)] do
            Log.warn "Query updater failed"
              |> Task.ignoreError


-- | Apply every matching updater and propagate the first failure.
processEventStrict :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEventStrict subscriber rawEvent = do
  let entityName = rawEvent.entityName
  let updaters = Registry.getUpdatersForEntity entityName subscriber.registry

  Log.withScope [("component", "QuerySubscriber")] do
    case rawEvent.metadata.globalPosition of
      Just position ->
        Log.debug [fmt|Processing event at position #{toText position}|] |> Task.ignoreError
      Nothing ->
        Log.debug "Processing event (no position)" |> Task.ignoreError

  applyEvent updaters rawEvent
