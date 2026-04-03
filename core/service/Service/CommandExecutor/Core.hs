{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandExecutor.Core (
  CommandExecutor (..),
  ExecutionResult (..),
  execute,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import Decider (CommandResult (..), DecisionContext (..), runDecision)
import Float qualified
import GHC.TypeLits qualified as GHC
import Int qualified
import Json qualified
import Log qualified
import Maybe (Maybe (..))
import Maybe qualified
import Record qualified
import Result (Result (..))
import Auth.Claims (UserClaims (..))
import Service.Auth (RequestContext (..))
import Service.Command (Event (..))
import Service.Command.Core (Command (..), Entity (..), EntityOf, EventOf, KnownMultiTenant (..), NameOf, SBool (..))
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher, FetchedEntity (..))
import Service.EntityFetcher.Core qualified
import Service.Event (EntityName, InsertionPayload (..), InsertionType (..))
import Service.Event qualified as Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.StreamId (StreamId, ToStreamId (..))
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
import Uuid (Uuid)
import Uuid qualified


-- | Result of executing a command through the CommandExecutor.
--
-- This captures the full outcome including retry information for debugging.
data ExecutionResult
  = CommandAccepted
      { streamId :: StreamId,
        eventsAppended :: Int,
        retriesAttempted :: Int
      }
  | CommandRejected
      { reason :: Text
      }
  | CommandFailed
      { error :: Text,
        retriesAttempted :: Int
      }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON ExecutionResult


-- | Configuration for command execution.
data CommandExecutor event = CommandExecutor
  { eventStore :: EventStore event,
    maxRetries :: Int,
    retryDelayMs :: Int
  }
  deriving (Generic)


-- | Wait with exponential backoff and jitter for retries.
-- Starts at 10ms, doubles each retry, capped at 1 second.
-- Adds random jitter between 50% and 100% of calculated delay.
awaitWithJitter :: Int -> Task _ Unit
awaitWithJitter retryCount = do
  -- Calculate exponential backoff: 10ms * 2^retryCount
  let retryCountFloat = Int.toFloat retryCount
  let baseDelayFloat = 10.0 * (2.0 ^ retryCountFloat)
  let baseDelayMs = Float.toInt baseDelayFloat
  let cappedDelay = min baseDelayMs 1000 -- Cap at 1 second

  -- Add jitter: randomize between 50% and 100% of the delay
  jitteredDelay <- Int.getRandomBetween (cappedDelay // 2) cappedDelay

  -- Wait for the calculated duration
  AsyncTask.sleep jitteredDelay


-- | Extract constructor names from a list of events for logging.
--
-- Uses 'Show' to get the textual representation and takes the first word,
-- which is the constructor name (e.g., @"CartCreated"@ from @"CartCreated {..}"@).
extractEventNames :: forall event. (Show event) => Array event -> Text
extractEventNames events =
  events
    |> Array.map (\event -> toText event |> Text.words |> Array.first |> Maybe.withDefault "Unknown")
    |> Text.joinWith ", "


-- | Execute a command through the event-sourcing pipeline.
--
-- This function:
-- 1. Extracts the entity ID from the command
-- 2. Fetches the current entity state from the event store
-- 3. Runs the decision logic to produce events or rejection (with RequestContext)
-- 4. Persists accepted events to the event store
-- 5. Handles concurrency conflicts with exponential backoff retry
execute ::
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity,
    commandEntity ~ EntityOf commandEvent,
    Event commandEvent,
    Entity commandEntity,
    ToStreamId (EntityIdType commandEntity),
    Eq (EntityIdType commandEntity),
    Show (EntityIdType commandEntity),
    Show commandEvent,
    GHC.KnownSymbol (NameOf command),
    KnownMultiTenant (IsMultiTenant command)
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  RequestContext ->
  command ->
  Task Text ExecutionResult
execute eventStore entityFetcher entityName requestContext command = do
  case sbool @(IsMultiTenant command) of
    SFalse -> do
      -- Single-tenant path: extract entity ID directly (unchanged behavior)
      let maybeEntityId = (getEntityIdImpl @command) command
      executeInner eventStore entityFetcher entityName requestContext command maybeEntityId Nothing

    STrue -> do
      -- Multi-tenant path: extract and validate tenant UUID from JWT
      case requestContext.user of
        Nothing ->
          Task.yield CommandRejected {reason = "Unauthorized"}
        Just claims ->
          case claims.tenantId of
            Nothing -> do
              Log.debug "Multi-tenant command rejected: no tenantId in token" |> Task.ignoreError
              Task.yield CommandRejected {reason = "Forbidden"}
            Just tenantIdText ->
              case Uuid.fromText tenantIdText of
                Nothing -> do
                  Log.debug "Multi-tenant command rejected: invalid tenantId format" |> Task.ignoreError
                  Task.yield CommandRejected {reason = "Forbidden"}
                Just tenantUuid -> do
                  let maybeEntityId = (getEntityIdImpl @command) tenantUuid command
                  executeInner eventStore entityFetcher entityName requestContext command maybeEntityId (Just tenantUuid)


-- | Internal execution logic shared between single-tenant and multi-tenant paths.
executeInner ::
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity,
    commandEntity ~ EntityOf commandEvent,
    Event commandEvent,
    Entity commandEntity,
    ToStreamId (EntityIdType commandEntity),
    Eq (EntityIdType commandEntity),
    Show (EntityIdType commandEntity),
    Show commandEvent,
    GHC.KnownSymbol (NameOf command),
    KnownMultiTenant (IsMultiTenant command)
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  RequestContext ->
  command ->
  Maybe (EntityIdType commandEntity) ->
  Maybe Uuid ->
  Task Text ExecutionResult
executeInner eventStore entityFetcher entityName requestContext command maybeEntityId maybeTenantUuid = do
  -- Helper to apply tenant scoping to a stream ID
  let scopeStreamId streamId = case maybeTenantUuid of
        Nothing -> streamId
        Just tenantUuid -> StreamId.withTenant tenantUuid streamId

  -- Helper function to fetch entity with error handling
  let fetchEntity streamId = do
        result <-
          entityFetcher.fetch entityName streamId
            |> Task.asResult

        case result of
          Ok (EntityFound fetchedEntity) -> do
            Task.yield (Just fetchedEntity.state, Just streamId)
          Ok EntityNotFound -> do
            Task.yield (Nothing, Just streamId)
          Err error -> do
            Task.throw (toText error)

  -- Resolve the entity state based on whether we have an entity ID
  (maybeEntity, maybeStreamId) <- case maybeEntityId of
    Just entityId -> do
      let streamId = toStreamId entityId |> scopeStreamId
      fetchEntity streamId
    Nothing -> do
      Task.yield (Nothing, Nothing)

  let maxRetries = 10

  let commandNameText = GHC.symbolVal (Record.Proxy @(NameOf command)) |> Text.fromLinkedList

  let retryLoop retryCount currentEntity currentStreamId = do
        let streamIdText = case currentStreamId of
              Just sid -> StreamId.toText sid
              Nothing -> "new-stream"
        Log.withScope [("component", "CommandExecutor"), ("streamId", streamIdText), ("entityName", EntityName.toText entityName), ("commandName", commandNameText)] do
          case retryCount of
            0 -> Log.info "Executing command" |> Task.ignoreError
            _ -> Log.debug [fmt|Retrying command (attempt #{retryCount})|] |> Task.ignoreError
          let decisionContext =
                DecisionContext
                  { genUuid = Uuid.generate
                  }

          -- Execute the decision logic, dispatching tenant UUID if multi-tenant
          let decision = case sbool @(IsMultiTenant command) of
                SFalse -> (decideImpl @command) command currentEntity requestContext
                STrue -> case maybeTenantUuid of
                  Just tenantUuid -> (decideImpl @command) tenantUuid command currentEntity requestContext
                  Nothing -> panic "unreachable: multi-tenant command without tenant UUID"
          commandResult <- runDecision decisionContext decision

          case commandResult of
            RejectCommand reason -> do
              Task.yield
                CommandRejected
                  { reason = reason
                  }
            AcceptCommand insertionType events -> do
              let validationError = case insertionType of
                    StreamCreation ->
                      case currentEntity of
                        Just _ -> Just "Cannot create entity: entity already exists"
                        Nothing -> Nothing
                    ExistingStream ->
                      case currentEntity of
                        Nothing -> Just "Cannot update entity: entity does not exist"
                        Just _ -> Nothing
                    _ -> Nothing
              case validationError of
                Just errorMsg ->
                  Task.yield
                    CommandRejected
                      { reason = errorMsg
                      }
                Nothing -> do
                  finalStreamId <- case currentStreamId of
                    Just sid -> Task.yield sid
                    Nothing -> do
                      let eventEntityIds = events |> Array.map (\e -> getEventEntityIdImpl e)
                      case Array.first eventEntityIds of
                        Nothing -> do
                          Task.throw "No events to extract entity ID from"
                        Just firstId -> do
                          let matchingCount = eventEntityIds |> Array.takeIf (\id -> id == firstId) |> Array.length
                          let totalCount = Array.length eventEntityIds
                          if matchingCount == totalCount
                            then do
                              let streamId = toStreamId firstId |> scopeStreamId
                              Task.yield streamId
                            else do
                              let idsText = eventEntityIds |> Array.map toText |> Text.joinWith ", "
                              Task.throw [fmt|Events have different entity IDs: #{idsText}|]

                  payload <-
                    Event.payloadFromEvents entityName finalStreamId events

                  let finalInsertionType = case insertionType of
                        InsertAfter pos -> InsertAfter pos
                        _ -> AnyStreamState

                  let payloadWithType = payload {insertionType = finalInsertionType}

                  insertResult <-
                    eventStore.insert payloadWithType
                      |> Task.asResult

                  case insertResult of
                    Ok _success -> do
                      let eventsCount = Array.length events
                      let eventNames = extractEventNames events
                      Log.withScope [("eventsEmitted", eventNames)] do
                        Log.info [fmt|Command executed successfully, #{toText eventsCount} event(s) inserted|] |> Task.ignoreError
                      Task.yield
                        CommandAccepted
                          { streamId = finalStreamId,
                            eventsAppended = eventsCount,
                            retriesAttempted = retryCount
                          }
                    Err (EventStore.InsertionError Event.ConsistencyCheckFailed) -> do
                      Log.warn [fmt|Concurrency conflict on attempt #{toText (retryCount + 1)}|] |> Task.ignoreError
                      if retryCount < maxRetries
                        then do
                          awaitWithJitter retryCount

                          refetchResult <-
                            entityFetcher.fetch entityName finalStreamId
                              |> Task.asResult

                          case refetchResult of
                            Ok (EntityFound freshFetchedEntity) -> do
                              retryLoop (retryCount + 1) (Just freshFetchedEntity.state) (Just finalStreamId)
                            Ok EntityNotFound -> do
                              retryLoop (retryCount + 1) Nothing (Just finalStreamId)
                            Err refetchError -> do
                              Log.warn [fmt|Failed to refetch entity for retry: #{toText refetchError}|] |> Task.ignoreError
                              Task.throw "Failed to refetch entity for retry"
                        else do
                          Task.yield
                            CommandFailed
                              { error = "Max retries exceeded due to concurrent modifications",
                                retriesAttempted = retryCount
                              }
                    Err eventStoreError -> do
                      let errorText = [fmt|Event store insert failed for stream #{toText finalStreamId}: #{toText eventStoreError}|]
                      Log.critical errorText |> Task.ignoreError
                      Task.yield
                        CommandFailed
                          { error = errorText,
                            retriesAttempted = retryCount
                          }

  retryLoop 0 maybeEntity maybeStreamId
