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
) where

import Array (Array)
import Array qualified
import AsyncTask (RaceWinner (..))
import AsyncTask qualified
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Data.Hashable qualified as Hashable
import Json qualified
import Log qualified
import Map (Map)
import Map qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Event (Event (..))
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


-- | Start live subscription for query updates.
-- Should be called after rebuildAll completes.
start :: QuerySubscriber -> Task Text Unit
start subscriber = do
  maybeStartPosition <- ConcurrentVar.peek subscriber.lastProcessedPosition

  let startPosition = case maybeStartPosition of
        Just (StreamPosition pos) -> StreamPosition (pos + 1)
        Nothing -> StreamPosition 0

  Log.withScope [("component", "QuerySubscriber")] do
    Log.info [fmt|Starting query subscriber from position #{startPosition}|]
      |> Task.ignoreError

  subId <-
    subscriber.eventStore.subscribeToAllEventsFromPosition
      startPosition
      (processEventHandler subscriber)
      |> Task.mapError (toText :: Error -> Text)

  subscriber.subscriptionId |> ConcurrentVar.modify (\_ -> Just subId)

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
  -- Update last processed position (same logic as rebuildAll)
  case rawEvent.metadata.globalPosition of
    Just pos -> subscriber.lastProcessedPosition |> ConcurrentVar.modify (\_ -> Just pos)
    Nothing -> pass


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

  -- Step 3: Read events from the event store in chunks.
  let chunkLimit = Limit (fromIntegral options.chunkSize)
  streamResult <-
    subscriber.eventStore.readAllEventsForwardFrom effectiveStart chunkLimit
      |> Task.asResult

  messageStream <- case streamResult of
    Err err ->
      if hashMismatchOccurred
        then
          -- Hash mismatch deletion succeeded but replay read failed → H5
          Task.throw (HashMismatchReplay [fmt|Query #{queryName}: stale hash deleted, but replay failed: #{toText err}|])
        else
          Task.throw (EventStoreFailed (toText err))
    Ok stream -> Task.yield stream

  -- Process events, routing only to updaters for this queryName.
  let updaters = getUpdatersForQuery queryName subscriber.registry
  result <-
    processEventsForQuery updaters options messageStream
      |> Task.asResult

  case result of
    Err errText -> do
      let err = UpdaterException errText
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName (Failed errText))
      Task.throw err
    Ok _ ->
      -- Mark query as ready.
      subscriber.queryReadiness
        |> ConcurrentVar.modify (Map.set queryName Ready)


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
          -- Rows found with matching hash: resume from the persisted position.
          Task.yield (StreamPosition minPos, False)
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


-- | Process events from a stream for a specific set of updaters.
-- Returns Err Text when any updater fails (propagated as UpdaterException).
processEventsForQuery
  :: Array QueryUpdater
  -> RebuildOptions
  -> Stream.Stream (ReadAllMessage Json.Value)
  -> Task Text Unit
processEventsForQuery updaters _options messageStream =
  messageStream
    |> Stream.consume
        (\_ message ->
          case message of
            AllEvent rawEvent -> applyEvent updaters rawEvent
            _ -> pass)
        unit


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
            Log.warn [fmt|Query updater #{updaterName} failed: #{err}|]
              |> Task.ignoreError
            Task.throw err


-- | Get all QueryUpdaters across all entities that belong to the named query.
getUpdatersForQuery :: Text -> QueryRegistry -> Array QueryUpdater
getUpdatersForQuery queryName registry =
  Registry.getAllUpdaters registry
    |> Array.takeIf (\updater -> updater.queryName == queryName)


-- | Spawn async rebuild for all registered queries, updating readiness states.
--
-- Returns Ok Unit when all queries have completed (either Ready or Failed).
-- Per-query failures are recorded in readiness state, not propagated as errors.
rebuildAllAsync
  :: QuerySubscriber
  -> RebuildOptions
  -> Task QueryRebuildError Unit
rebuildAllAsync subscriber options = do
  let queryNames = getAllQueryNames subscriber.registry
  queryNames
    |> Task.forEach (\queryName -> do
        result <-
          rebuildFrom subscriber queryName (StreamPosition 0) options
            |> Task.asResult
        case result of
          Ok _ -> pass
          Err err ->
            Log.withScope [("component", "QuerySubscriber"), ("queryName", queryName)] do
              Log.warn [fmt|Query rebuild failed: #{toText (show err)}|]
                |> Task.ignoreError)


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


-- | Process a single raw event through all relevant query updaters.
processEvent :: QuerySubscriber -> Event Json.Value -> Task Text Unit
processEvent subscriber rawEvent = do
  let entityName = rawEvent.entityName
  let updaters = Registry.getUpdatersForEntity entityName subscriber.registry

  Log.withScope [("component", "QuerySubscriber")] do
    case rawEvent.metadata.globalPosition of
      Just pos ->
        Log.debug [fmt|Processing event at position #{toText pos}|] |> Task.ignoreError
      Nothing ->
        Log.debug "Processing event (no position)" |> Task.ignoreError

  -- Run all updaters for this entity type
  updaters
    |> Task.forEach \updater -> do
      let updaterName = updater.queryName
      result <- updater.updateQuery rawEvent |> Task.asResult
      case result of
        Ok _ -> pass
        Err errText ->
          Log.withScope [("component", "QuerySubscriber"), ("queryName", updaterName)] do
            Log.warn [fmt|Query updater failed: #{errText}|]
              |> Task.ignoreError
