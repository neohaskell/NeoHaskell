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
import Control.Exception qualified as GhcException
import Data.IORef qualified as GhcIORef
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
import Service.AccessControl (AccessError)
import Service.AccessControl (AccessError (..))
import Service.Command (Event (..))
import Service.Command.Core (Command (..), Entity (..), EntityOf, EventOf, KnownMultiTenant (..), NameOf, SBool (..))
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher, FetchedEntity (..))
import Service.EntityFetcher.Core qualified
import Service.Event (EntityName, InsertionPayload (..), InsertionType (..))
import Service.Event qualified as Event
import Service.Event.EntityName qualified as EntityName
import Service.Event.EventMetadata (EventMetadata)
import Service.Event.EventMetadata qualified as EventMetadata
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
  | CommandUnauthorized
      { authError :: AccessError
      }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON ExecutionResult


data CommandExecutionFailure = CommandExecutionFailure
  { failureMessage :: Text,
    failureStage :: Text,
    failureClass :: Text
  }


data CommandLogOutcome
  = AcceptedCommandLog StreamId Int
  | RejectedCommandLog (Maybe StreamId)
  | UnauthorizedCommandLog AccessError
  | AppendFailedCommandLog StreamId Text
  | RetriesExhaustedCommandLog StreamId
  | ExecutionFailedCommandLog CommandExecutionFailure


data CommandAttempt = CommandAttempt
  { attemptResult :: ExecutionResult,
    commandLogOutcome :: CommandLogOutcome,
    appendedEventMetadata :: Array EventMetadata
  }


data CommandExecutionOutcome
  = CommandExecutionSucceeded CommandAttempt
  | CommandExecutionFailed CommandExecutionFailure


data CommandLogSummary = CommandLogSummary
  { resultLabel :: Text,
    resultLevel :: Log.Level,
    eventCount :: Int,
    resultStreamId :: Maybe StreamId,
    resultFailureStage :: Maybe Text,
    resultErrorClass :: Maybe Text
  }


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


-- | Execute a command through the event-sourcing pipeline.
--
-- This function:
-- 1. Extracts the entity ID from the command
-- 2. Fetches the current entity state from the event store
-- 3. Runs the decision logic to produce events or rejection (with RequestContext)
-- 4. Persists accepted events to the event store
-- 5. Handles concurrency conflicts with exponential backoff retry
positionInsertions ::
  InsertionType ->
  Array (Event.Insertion event) ->
  Array (Event.Insertion event)
positionInsertions insertionType insertions = do
  case insertionType of
    InsertAfter (Event.StreamPosition previousPosition) ->
      insertions
        |> Array.indexedMap (\index insertion -> do
            let localPosition = previousPosition + 1 + Int.toInt64 index
            Event.Insertion
              { id = insertion.id,
                event = insertion.event,
                metadata =
                  insertion.metadata
                    { EventMetadata.localPosition = Just (Event.StreamPosition localPosition)
                    }
              }
          )
    _ -> insertions


-- | Bind an existing-stream decision to the revision observed by the fetcher.
insertionTypeForEntity :: InsertionType -> Maybe (FetchedEntity entity) -> InsertionType
insertionTypeForEntity insertionType maybeFetchedEntity = case insertionType of
  ExistingStream -> existingInsertionType maybeFetchedEntity
  _ -> insertionType


-- | Preserve the legacy precondition when a fetcher cannot report a revision.
existingInsertionType :: Maybe (FetchedEntity entity) -> InsertionType
existingInsertionType maybeFetchedEntity =
  maybeFetchedEntity
    |> Maybe.andThen (\fetchedEntity -> fetchedEntity.lastPosition)
    |> Maybe.map InsertAfter
    |> Maybe.withDefault ExistingStream


normalizeRequestContext :: RequestContext -> Task Text RequestContext
normalizeRequestContext requestContext =
  if requestContext.requestId == Uuid.nil
    then do
      requestId <- Uuid.generate
      Task.yield requestContext {requestId}
    else Task.yield requestContext


eventMetadataFor :: Maybe EventMetadata -> RequestContext -> EventMetadata -> EventMetadata
eventMetadataFor sourceEventMetadata requestContext freshMetadata = case sourceEventMetadata of
  Nothing -> do
    let relatedUserSub = requestContext.user |> Maybe.map (\claims -> claims.sub)
    freshMetadata
      { EventMetadata.relatedUserSub = relatedUserSub,
        EventMetadata.correlationId = Just (Uuid.toText requestContext.requestId),
        EventMetadata.causationId = Nothing
      }
  Just sourceMetadata -> do
    let sourceEventId = Uuid.toText sourceMetadata.eventId
    let correlationId = sourceMetadata.correlationId |> Maybe.withDefault sourceEventId
    freshMetadata
      { EventMetadata.relatedUserSub = sourceMetadata.relatedUserSub,
        EventMetadata.correlationId = Just correlationId,
        EventMetadata.causationId = Just sourceEventId
      }


attributeInsertionMetadata :: Maybe EventMetadata -> RequestContext -> Event.Insertion event -> Event.Insertion event
attributeInsertionMetadata sourceEventMetadata requestContext insertion =
  Event.Insertion
    { id = insertion.id,
      event = insertion.event,
      metadata = eventMetadataFor sourceEventMetadata requestContext insertion.metadata
    }


optionalTextField :: Text -> Maybe Text -> Array (Text, Json.Value)
optionalTextField fieldName maybeValue = case maybeValue of
  Just value -> Array.wrap (fieldName Json..= value)
  Nothing -> Array.empty


commandTraceFields :: RequestContext -> Maybe EventMetadata -> Array (Text, Json.Value)
commandTraceFields requestContext sourceEventMetadata = do
  let requestIdText =
        if requestContext.requestId == Uuid.nil
          then Nothing
          else Just (Uuid.toText requestContext.requestId)
  let sourceEventId = sourceEventMetadata |> Maybe.map (\metadata -> Uuid.toText metadata.eventId)
  let correlationId = case sourceEventMetadata of
        Just metadata -> Just (metadata.correlationId |> Maybe.withDefault (Uuid.toText metadata.eventId))
        Nothing -> requestIdText
  let relatedUserSub = case sourceEventMetadata of
        Just metadata -> metadata.relatedUserSub
        Nothing -> requestContext.user |> Maybe.map (\claims -> claims.sub)
  optionalTextField "requestId" requestIdText
    |> Array.append (optionalTextField "correlationId" correlationId)
    |> Array.append (optionalTextField "causationId" sourceEventId)
    |> Array.append (optionalTextField "relatedUserSub" relatedUserSub)


eventTraceFields :: Text -> EventMetadata -> Array (Text, Json.Value)
eventTraceFields requestIdText metadata =
  ["requestId" Json..= requestIdText]
    |> Array.append (optionalTextField "correlationId" metadata.correlationId)
    |> Array.append (optionalTextField "causationId" metadata.causationId)
    |> Array.append (optionalTextField "relatedUserSub" metadata.relatedUserSub)


accessErrorClass :: AccessError -> Text
accessErrorClass accessError = case accessError of
  Unauthenticated -> "Unauthenticated"
  Forbidden -> "Forbidden"
  InsufficientPermissions _ -> "InsufficientPermissions"


insertionFailureClass :: Event.InsertionFailure -> Text
insertionFailureClass insertionFailure = case insertionFailure of
  Event.ConsistencyCheckFailed -> "ConsistencyCheckFailed"
  Event.InsertionFailed _ -> "InsertionFailed"
  Event.PayloadTooLarge -> "PayloadTooLarge"
  Event.EmptyPayload -> "EmptyPayload"


eventStoreErrorClass :: EventStore.Error -> Text
eventStoreErrorClass eventStoreError = case eventStoreError of
  EventStore.StreamNotFound _ _ -> "StreamNotFound"
  EventStore.EventNotFound _ _ -> "EventNotFound"
  EventStore.StorageFailure _ -> "StorageFailure"
  EventStore.SubscriptionNotFound _ -> "SubscriptionNotFound"
  EventStore.SubscriptionError _ _ -> "SubscriptionError"
  EventStore.TruncationError _ _ _ _ -> "TruncationError"
  EventStore.InsertionError insertionFailure -> insertionFailureClass insertionFailure
  EventStore.ReadingAllError _ -> "ReadingAllError"


commandLogSummary :: CommandLogOutcome -> CommandLogSummary
commandLogSummary outcome = case outcome of
  AcceptedCommandLog streamId count ->
    CommandLogSummary
      { resultLabel = "accepted",
        resultLevel = Log.Info,
        eventCount = count,
        resultStreamId = Just streamId,
        resultFailureStage = Nothing,
        resultErrorClass = Nothing
      }
  RejectedCommandLog maybeStreamId ->
    CommandLogSummary
      { resultLabel = "rejected",
        resultLevel = Log.Warn,
        eventCount = 0,
        resultStreamId = maybeStreamId,
        resultFailureStage = Just "decision",
        resultErrorClass = Just "CommandRejected"
      }
  UnauthorizedCommandLog accessError ->
    CommandLogSummary
      { resultLabel = "unauthorized",
        resultLevel = Log.Warn,
        eventCount = 0,
        resultStreamId = Nothing,
        resultFailureStage = Just "authorization",
        resultErrorClass = Just (accessErrorClass accessError)
      }
  AppendFailedCommandLog streamId safeErrorClass ->
    CommandLogSummary
      { resultLabel = "append_failed",
        resultLevel = Log.Error,
        eventCount = 0,
        resultStreamId = Just streamId,
        resultFailureStage = Just "append",
        resultErrorClass = Just safeErrorClass
      }
  RetriesExhaustedCommandLog streamId ->
    CommandLogSummary
      { resultLabel = "retries_exhausted",
        resultLevel = Log.Error,
        eventCount = 0,
        resultStreamId = Just streamId,
        resultFailureStage = Just "append",
        resultErrorClass = Just "ConsistencyCheckFailed"
      }
  ExecutionFailedCommandLog failure ->
    CommandLogSummary
      { resultLabel = "execution_failed",
        resultLevel = Log.Error,
        eventCount = 0,
        resultStreamId = Nothing,
        resultFailureStage = Just failure.failureStage,
        resultErrorClass = Just failure.failureClass
      }


emitCommandResult :: Text -> Text -> RequestContext -> Maybe EventMetadata -> CommandLogOutcome -> Task _ Unit
emitCommandResult commandNameText entityNameText requestContext sourceEventMetadata outcome = do
  let summary = commandLogSummary outcome
  let fields =
        [ "kind" Json..= ("command_result" :: Text),
          "result" Json..= summary.resultLabel,
          "component" Json..= ("CommandExecutor" :: Text),
          "commandName" Json..= commandNameText,
          "entityName" Json..= entityNameText,
          "eventsAppended" Json..= summary.eventCount
        ]
          |> Array.append (commandTraceFields requestContext sourceEventMetadata)
          |> Array.append (optionalTextField "streamId" (summary.resultStreamId |> Maybe.map StreamId.toText))
          |> Array.append (optionalTextField "failureStage" summary.resultFailureStage)
          |> Array.append (optionalTextField "errorClass" summary.resultErrorClass)
  Log.recordIsolated summary.resultLevel fields "Command result"
    |> Task.ignoreError


emitEventAppended :: Text -> Text -> Text -> Text -> EventMetadata -> Task _ Unit
emitEventAppended commandNameText entityNameText streamIdText requestIdText metadata = do
  let fields =
        [ "kind" Json..= ("event_appended" :: Text),
          "eventId" Json..= Uuid.toText metadata.eventId,
          "component" Json..= ("CommandExecutor" :: Text),
          "commandName" Json..= commandNameText,
          "entityName" Json..= entityNameText,
          "streamId" Json..= streamIdText
        ]
          |> Array.append (eventTraceFields requestIdText metadata)
  Log.recordIsolated Log.Info fields "Event appended"
    |> Task.ignoreError


emitCommandRetry :: RequestContext -> Maybe EventMetadata -> Int -> Text -> Task _ Unit
emitCommandRetry requestContext sourceEventMetadata attempt retryResult = do
  let fields =
        [ "kind" Json..= ("retry" :: Text),
          "result" Json..= retryResult,
          "attempt" Json..= attempt,
          "errorClass" Json..= ("ConsistencyCheckFailed" :: Text)
        ]
          |> Array.append (commandTraceFields requestContext sourceEventMetadata)
  Log.recordIsolated Log.Warn fields "Command append conflict"
    |> Task.ignoreError


-- | Execute a command through fetch, decision, append, and bounded conflict retry.
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
  Maybe EventMetadata ->
  RequestContext ->
  command ->
  Task Text ExecutionResult
execute eventStore entityFetcher entityName sourceEventMetadata requestContext command = do
  let commandNameText = GHC.symbolVal (Record.Proxy @(NameOf command)) |> Text.fromLinkedList
  let entityNameText = EntityName.toText entityName
  commandResultReported <- Task.fromIO (GhcIORef.newIORef False)
  currentRequestContext <- Task.fromIO (GhcIORef.newIORef requestContext)
  let reportCommandResult context outcome =
        Task.fromIO do
          GhcException.mask_ do
            (emitCommandResult commandNameText entityNameText context sourceEventMetadata outcome :: Task Text Unit)
              |> Task.runOrPanic
            GhcIORef.writeIORef commandResultReported True
  let reportInterruptedCommand = do
        wasReported <- GhcIORef.readIORef commandResultReported |> Task.fromIO
        if wasReported
          then pass
          else do
            context <- GhcIORef.readIORef currentRequestContext |> Task.fromIO
            emitCommandResult
              commandNameText
              entityNameText
              context
              sourceEventMetadata
              ( ExecutionFailedCommandLog
                  (CommandExecutionFailure
                    { failureMessage = "Command execution was interrupted",
                      failureStage = "execution",
                      failureClass = "ExecutionInterrupted"
                    })
              )
  Task.finally reportInterruptedCommand do
    normalizedContextResult <- normalizeRequestContext requestContext |> Task.asResultSafe
    case normalizedContextResult of
      Err failureMessage -> do
        reportCommandResult
          requestContext
          ( ExecutionFailedCommandLog
              (CommandExecutionFailure
                { failureMessage,
                  failureStage = "request_context",
                  failureClass = "RequestContextFailure"
                })
          )
        Task.throw failureMessage
      Ok normalizedContext -> do
        GhcIORef.writeIORef currentRequestContext normalizedContext |> Task.fromIO
        let executionTask =
              executeNormalized eventStore entityFetcher entityName sourceEventMetadata normalizedContext command
                |> Task.map CommandExecutionSucceeded
                |> Task.recover (\failure -> Task.yield (CommandExecutionFailed failure))
                |> Task.mapError (\_ -> "Command execution failed before reporting its outcome" :: Text)
        executionResult <- executionTask |> Task.asResultSafe
        case executionResult of
          Err failureMessage -> do
            reportCommandResult
              normalizedContext
              ( ExecutionFailedCommandLog
                  (CommandExecutionFailure
                    { failureMessage,
                      failureStage = "execution",
                      failureClass = "SynchronousException"
                    })
              )
            Task.throw failureMessage
          Ok (CommandExecutionFailed failure) -> do
            reportCommandResult normalizedContext (ExecutionFailedCommandLog failure)
            Task.throw failure.failureMessage
          Ok (CommandExecutionSucceeded attempt) -> do
            case attempt.commandLogOutcome of
              AcceptedCommandLog streamId _ -> do
                let requestIdText = Uuid.toText normalizedContext.requestId
                Task.fromIO do
                  GhcException.mask_ do
                    (reportCommandResult normalizedContext attempt.commandLogOutcome :: Task Text Unit)
                      |> Task.runOrPanic
                    ( attempt.appendedEventMetadata
                        |> Task.forEach (\metadata -> emitEventAppended commandNameText entityNameText (StreamId.toText streamId) requestIdText metadata)
                        :: Task Text Unit
                      )
                      |> Task.runOrPanic
              _ -> reportCommandResult normalizedContext attempt.commandLogOutcome
            Task.yield attempt.attemptResult


executeNormalized ::
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
  Maybe EventMetadata ->
  RequestContext ->
  command ->
  Task CommandExecutionFailure CommandAttempt
executeNormalized eventStore entityFetcher entityName sourceEventMetadata normalizedContext command = do
  -- Authorization check: consult canExecuteImpl before any I/O or entity fetch.
  -- This is a pure check — no event-store calls are made on rejection.
  --
  -- Trusted-mode requests (transports that ran without JWT auth configured)
  -- bypass the gate entirely so the no-auth deployment story is preserved:
  -- commands without an explicit 'canAccess' override stay reachable.
  let gateResult = case normalizedContext.trustedBypass of
        True  -> Nothing
        False -> canExecuteImpl @command (normalizedContext.user)
  case gateResult of
    Just authErr -> do
      Task.yield
        CommandAttempt
          { attemptResult = CommandUnauthorized {authError = authErr},
            commandLogOutcome = UnauthorizedCommandLog authErr,
            appendedEventMetadata = Array.empty
          }
    Nothing ->
      case sbool @(IsMultiTenant command) of
        SFalse -> do
          -- Single-tenant path: extract entity ID directly (unchanged behavior)
          let maybeEntityId = (getEntityIdImpl @command) command
          executeInner eventStore entityFetcher entityName sourceEventMetadata normalizedContext command maybeEntityId Nothing

        STrue -> do
          -- Multi-tenant path: extract and validate tenant UUID from JWT.
          -- Auth-shaped failures (no user, no tenantId, bad tenantId) ride the
          -- CommandUnauthorized channel so transports map them to 401/403,
          -- matching the per-command auth gate above.
          case normalizedContext.user of
            Nothing -> do
              let authError = Unauthenticated
              Task.yield
                CommandAttempt
                  { attemptResult = CommandUnauthorized {authError},
                    commandLogOutcome = UnauthorizedCommandLog authError,
                    appendedEventMetadata = Array.empty
                  }
            Just claims ->
              case claims.tenantId of
                Nothing -> do
                  let authError = Forbidden
                  Task.yield
                    CommandAttempt
                      { attemptResult = CommandUnauthorized {authError},
                        commandLogOutcome = UnauthorizedCommandLog authError,
                        appendedEventMetadata = Array.empty
                      }
                Just tenantIdText ->
                  case Uuid.fromText tenantIdText of
                    Nothing -> do
                      let authError = Forbidden
                      Task.yield
                        CommandAttempt
                          { attemptResult = CommandUnauthorized {authError},
                            commandLogOutcome = UnauthorizedCommandLog authError,
                            appendedEventMetadata = Array.empty
                          }
                    Just tenantUuid -> do
                      let maybeEntityId = (getEntityIdImpl @command) tenantUuid command
                      executeInner eventStore entityFetcher entityName sourceEventMetadata normalizedContext command maybeEntityId (Just tenantUuid)


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
  Maybe EventMetadata ->
  RequestContext ->
  command ->
  Maybe (EntityIdType commandEntity) ->
  Maybe Uuid ->
  Task CommandExecutionFailure CommandAttempt
executeInner eventStore entityFetcher entityName sourceEventMetadata requestContext command maybeEntityId maybeTenantUuid = do
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
            Task.yield (Just fetchedEntity, Just streamId)
          Ok EntityNotFound -> do
            Task.yield (Nothing, Just streamId)
          Err error -> do
            Task.throw
              CommandExecutionFailure
                { failureMessage = toText error,
                  failureStage = "fetch",
                  failureClass = "EntityFetchError"
                }

  -- Resolve the entity state based on whether we have an entity ID
  (maybeFetchedEntity, maybeStreamId) <- case maybeEntityId of
    Just entityId -> do
      let streamId = toStreamId entityId |> scopeStreamId
      fetchEntity streamId
    Nothing -> do
      Task.yield (Nothing, Nothing)

  let maxRetries = 10

  let commandNameText = GHC.symbolVal (Record.Proxy @(NameOf command)) |> Text.fromLinkedList

  let retryLoop retryCount maybeCurrentFetchedEntity currentStreamId = do
        let currentEntity = maybeCurrentFetchedEntity |> Maybe.map (\fetchedEntity -> fetchedEntity.state)
        let streamIdText = case currentStreamId of
              Just sid -> StreamId.toText sid
              Nothing -> "new-stream"
        Log.withScope [("component", "CommandExecutor"), ("streamId", streamIdText), ("entityName", EntityName.toText entityName), ("commandName", commandNameText)] do
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
          commandResult <-
            runDecision decisionContext decision
              |> Task.mapError
                (\error ->
                  CommandExecutionFailure
                    { failureMessage = error,
                      failureStage = "decision",
                      failureClass = "DecisionExecutionError"
                    }
                )

          case commandResult of
            RejectCommand reason -> do
              Task.yield
                CommandAttempt
                  { attemptResult = CommandRejected {reason},
                    commandLogOutcome = RejectedCommandLog currentStreamId,
                    appendedEventMetadata = Array.empty
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
                    CommandAttempt
                      { attemptResult = CommandRejected {reason = errorMsg},
                        commandLogOutcome = RejectedCommandLog currentStreamId,
                        appendedEventMetadata = Array.empty
                      }
                Nothing -> do
                  finalStreamId <- case currentStreamId of
                    Just sid -> Task.yield sid
                    Nothing -> do
                      let eventEntityIds = events |> Array.map (\e -> getEventEntityIdImpl e)
                      case Array.first eventEntityIds of
                        Nothing -> do
                          Task.throw
                            CommandExecutionFailure
                              { failureMessage = "No events to extract entity ID from",
                                failureStage = "execution",
                                failureClass = "InvalidCommandEvents"
                              }
                        Just firstId -> do
                          let matchingCount = eventEntityIds |> Array.takeIf (\id -> id == firstId) |> Array.length
                          let totalCount = Array.length eventEntityIds
                          if matchingCount == totalCount
                            then do
                              let streamId = toStreamId firstId |> scopeStreamId
                              Task.yield streamId
                            else do
                              let idsText = eventEntityIds |> Array.map toText |> Text.joinWith ", "
                              Task.throw
                                CommandExecutionFailure
                                  { failureMessage = [fmt|Events have different entity IDs: #{idsText}|],
                                    failureStage = "execution",
                                    failureClass = "InvalidCommandEvents"
                                  }

                  payload <-
                    Event.payloadFromEvents entityName finalStreamId events

                  let finalInsertionType = insertionTypeForEntity insertionType maybeCurrentFetchedEntity
                  let attributedInsertions =
                        payload.insertions
                          |> Array.map (\insertion -> attributeInsertionMetadata sourceEventMetadata requestContext insertion)

                  let payloadWithType =
                        payload
                          { insertionType = finalInsertionType,
                            insertions = positionInsertions finalInsertionType attributedInsertions
                          }

                  insertResult <-
                    eventStore.insert payloadWithType
                      |> Task.asResult

                  case insertResult of
                    Ok _success -> do
                      let eventsCount = Array.length events
                      Task.yield
                        CommandAttempt
                          { attemptResult =
                              CommandAccepted
                                { streamId = finalStreamId,
                                  eventsAppended = eventsCount,
                                  retriesAttempted = retryCount
                                },
                            commandLogOutcome = AcceptedCommandLog finalStreamId eventsCount,
                            appendedEventMetadata = payloadWithType.insertions |> Array.map (\insertion -> insertion.metadata)
                          }
                    Err (EventStore.InsertionError Event.ConsistencyCheckFailed) -> do
                      let retryResult = if retryCount < maxRetries then "scheduled" else "exhausted"
                      emitCommandRetry
                        requestContext
                        sourceEventMetadata
                        (retryCount + 1)
                        retryResult
                      if retryCount < maxRetries
                        then do
                          awaitWithJitter retryCount

                          refetchResult <-
                            entityFetcher.fetch entityName finalStreamId
                              |> Task.asResult

                          case refetchResult of
                            Ok (EntityFound freshFetchedEntity) -> do
                              retryLoop (retryCount + 1) (Just freshFetchedEntity) (Just finalStreamId)
                            Ok EntityNotFound -> do
                              retryLoop (retryCount + 1) Nothing (Just finalStreamId)
                            Err _ -> do
                              Task.throw
                                CommandExecutionFailure
                                  { failureMessage = "Failed to refetch entity for retry",
                                    failureStage = "retry_refetch",
                                    failureClass = "EntityFetchError"
                                  }
                        else do
                          Task.yield
                            CommandAttempt
                              { attemptResult =
                                  CommandFailed
                                    { error = "Max retries exceeded due to concurrent modifications",
                                      retriesAttempted = retryCount
                                    },
                                commandLogOutcome = RetriesExhaustedCommandLog finalStreamId,
                                appendedEventMetadata = Array.empty
                              }
                    Err eventStoreError -> do
                      let errorText = [fmt|Event store insert failed for stream #{toText finalStreamId}: #{toText eventStoreError}|]
                      Task.yield
                        CommandAttempt
                          { attemptResult =
                              CommandFailed
                                { error = errorText,
                                  retriesAttempted = retryCount
                                },
                            commandLogOutcome = AppendFailedCommandLog finalStreamId (eventStoreErrorClass eventStoreError),
                            appendedEventMetadata = Array.empty
                          }

  retryLoop 0 maybeFetchedEntity maybeStreamId
