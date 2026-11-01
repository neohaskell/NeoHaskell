{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandExecutor.Core (
  ExecutionDependencies (..),
  CommandSource (..),
  CommandExecutionFailure (..),
  ExecutionResult (..),
  execute,
) where

import Array (Array)
import Array qualified
import AsyncTask qualified
import Basics
import Decider (CommandResult (..), Decision, DecisionContext (..))
import Decider qualified
import Erasable.Internal (NewKey)
import Float qualified
import GHC.TypeLits qualified as GHC
import Int qualified
import Log qualified
import Maybe (Maybe (..))
import Maybe qualified
import Record qualified
import Result (Result (..))
import Service.Auth (RequestContext (..))
import Service.AccessControl (AccessError)
import Service.Command (Event (..))
import Service.Command.Core (Command (..), Entity (..), EntityOf, EventOf, KnownMultiTenant (..), NameOf, SBool (..))
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher, FetchedEntity (..))
import Service.EntityFetcher.Core qualified
import Service.Event (EntityName, InsertionPayload (..), InsertionType (..))
import Service.Event qualified as Event
import Service.Event.EntityName qualified as EntityName
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure (..))
import Service.CommandExecutor.CommandOutcome (CommandOutcome (..))
import Service.CommandExecutor.ExecutionResult (ExecutionResult (..))
import Service.CommandExecutor.Reporting qualified as Reporting
import Service.CommandExecutor.Source (CommandSource (..))
import Service.CommandExecutor.Source qualified as Source
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId (ToStreamId (..))
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


data ExecutionDependencies entity event = ExecutionDependencies
  { eventStore :: EventStore event,
    entityFetcher :: EntityFetcher entity event,
    entityName :: EntityName
  }


data ExecutionContext entity event = ExecutionContext
  { services :: ExecutionDependencies entity event,
    source :: CommandSource,
    commandName :: Text
  }


data AuthorizedCommand entityId entity event
  = DirectAuthority (Maybe entityId) (Maybe entity -> Decision event)
  | TenantAuthority Uuid (Maybe entityId) (Maybe entity -> Decision event)


data AttemptState entity = AttemptState
  { attemptRetries :: Int,
    attemptFetched :: Maybe (FetchedEntity entity),
    attemptStream :: Maybe StreamId.StreamId
  }


data AcceptedDecision event = AcceptedDecision InsertionType (Array event) (Array NewKey)


data InsertionRejection
  = EntityAlreadyExists
  | EntityDoesNotExist


insertionRejectionText :: InsertionRejection -> Text
insertionRejectionText rejection = case rejection of
  EntityAlreadyExists -> "Cannot create entity: entity already exists"
  EntityDoesNotExist -> "Cannot update entity: entity does not exist"


validateInsertion :: InsertionType -> Maybe entity -> Maybe InsertionRejection
validateInsertion insertionType currentEntity = case insertionType of
  StreamCreation -> case currentEntity of
    Just _ -> Just EntityAlreadyExists
    Nothing -> Nothing
  ExistingStream -> case currentEntity of
    Nothing -> Just EntityDoesNotExist
    Just _ -> Nothing
  _ -> Nothing


authorizedEntityId :: AuthorizedCommand entityId entity event -> Maybe entityId
authorizedEntityId authorized = case authorized of
  DirectAuthority entityId _ -> entityId
  TenantAuthority _ entityId _ -> entityId


scopeStream :: AuthorizedCommand entityId entity event -> StreamId.StreamId -> StreamId.StreamId
scopeStream authorized streamId = case authorized of
  DirectAuthority _ _ -> streamId
  TenantAuthority tenantUuid _ _ -> StreamId.withTenant tenantUuid streamId


decideFor :: AuthorizedCommand entityId entity event -> Maybe entity -> Decision event
decideFor authorized currentEntity = case authorized of
  DirectAuthority _ decision -> decision currentEntity
  TenantAuthority _ _ decision -> decision currentEntity


resolveAuthorizedCommand ::
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity,
    KnownMultiTenant (IsMultiTenant command)
  ) =>
  CommandSource ->
  command ->
  Result AccessError (AuthorizedCommand (EntityIdType commandEntity) commandEntity commandEvent)
resolveAuthorizedCommand source command = do
  let requestContext = Source.sourceContext source
  let gateResult = if requestContext.trustedBypass
        then Nothing
        else canExecuteImpl @command requestContext.user
  case gateResult of
    Just authError -> Err authError
    Nothing ->
      case sbool @(IsMultiTenant command) of
        SFalse ->
          Ok
            (DirectAuthority
              (getEntityIdImpl @command command)
              (\currentEntity -> decideImpl @command command currentEntity requestContext)
            )
        STrue ->
          case Source.tenantFromRequest source of
            Err authError -> Err authError
            Ok tenantUuid ->
              Ok
                (TenantAuthority
                  tenantUuid
                  (getEntityIdImpl @command tenantUuid command)
                  (\currentEntity -> decideImpl @command tenantUuid command currentEntity requestContext)
                )


awaitWithJitter :: Int -> Task _ Unit
awaitWithJitter retryCount = do
  let retryCountFloat = Int.toFloat retryCount
  let baseDelayFloat = 10.0 * (2.0 ^ retryCountFloat)
  let baseDelayMs = Float.toInt baseDelayFloat
  let cappedDelay = min baseDelayMs 1000

  jitteredDelay <- Int.getRandomBetween (cappedDelay // 2) cappedDelay

  AsyncTask.sleep jitteredDelay


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


insertionTypeForEntity :: InsertionType -> Maybe (FetchedEntity entity) -> InsertionType
insertionTypeForEntity insertionType maybeFetchedEntity = case insertionType of
  ExistingStream ->
    maybeFetchedEntity
      |> Maybe.andThen (\fetchedEntity -> fetchedEntity.lastPosition)
      |> Maybe.map InsertAfter
      |> Maybe.withDefault ExistingStream
  _ -> insertionType


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
  ExecutionDependencies commandEntity commandEvent ->
  CommandSource ->
  command ->
  Task CommandExecutionFailure ExecutionResult
execute dependencies source command = do
  let commandNameText = GHC.symbolVal (Record.Proxy @(NameOf command)) |> Text.fromLinkedList
  let entityNameText = EntityName.toText dependencies.entityName
  let reporting = Reporting.ReportingConfig {commandName = commandNameText, entityName = entityNameText, source}
  let context = ExecutionContext {services = dependencies, source, commandName = commandNameText}
  Reporting.run reporting \normalizedSource ->
    executeNormalized (context {source = normalizedSource}) command


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
  ExecutionContext commandEntity commandEvent ->
  command ->
  Task CommandExecutionFailure CommandOutcome
executeNormalized context command =
  case resolveAuthorizedCommand context.source command of
    Err authError -> Task.yield (Unauthorized authError)
    Ok authorized -> executeInner context authorized


executeInner ::
  forall commandEntity commandEvent.
  ( Event commandEvent,
    Entity commandEntity,
    commandEntity ~ EntityOf commandEvent,
    ToStreamId (EntityIdType commandEntity),
    Eq (EntityIdType commandEntity),
    Show (EntityIdType commandEntity),
    Show commandEvent
  ) =>
  ExecutionContext commandEntity commandEvent ->
  AuthorizedCommand (EntityIdType commandEntity) commandEntity commandEvent ->
  Task CommandExecutionFailure CommandOutcome
executeInner context authorized = do
  initialState <- initialAttemptState context authorized
  runAttempt context authorized initialState


fetchAt ::
  ExecutionContext entity event ->
  StreamId.StreamId ->
  Task CommandExecutionFailure (Maybe (FetchedEntity entity))
fetchAt context streamId = do
  let dependencies = context.services
  result <- dependencies.entityFetcher.fetch dependencies.entityName streamId |> Task.asResult
  case result of
    Ok (EntityFound fetchedEntity) -> Task.yield (Just fetchedEntity)
    Ok EntityNotFound -> Task.yield Nothing
    Err error -> Task.throw (EntityFetchFailed (toText error))


initialAttemptState ::
  ToStreamId entityId =>
  ExecutionContext entity event ->
  AuthorizedCommand entityId entity event ->
  Task CommandExecutionFailure (AttemptState entity)
initialAttemptState context authorized = case authorizedEntityId authorized of
  Nothing -> Task.yield AttemptState {attemptRetries = 0, attemptFetched = Nothing, attemptStream = Nothing}
  Just entityId -> do
    let streamId = toStreamId entityId |> scopeStream authorized
    fetched <- fetchAt context streamId
    Task.yield AttemptState {attemptRetries = 0, attemptFetched = fetched, attemptStream = Just streamId}


runAttempt ::
  forall entity event.
  ( Event event,
    Entity entity,
    entity ~ EntityOf event,
    ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Show (EntityIdType entity),
    Show event
  ) =>
  ExecutionContext entity event ->
  AuthorizedCommand (EntityIdType entity) entity event ->
  AttemptState entity ->
  Task CommandExecutionFailure CommandOutcome
runAttempt context authorized state = do
  let currentEntity = state.attemptFetched |> Maybe.map (\fetched -> fetched.state)
  let streamIdText = case state.attemptStream of
        Just streamId -> StreamId.toText streamId
        Nothing -> "new-stream"
  Log.withScope
    [ ("component", "CommandExecutor"),
      ("streamId", streamIdText),
      ("entityName", EntityName.toText context.services.entityName),
      ("commandName", context.commandName)
    ] do
    let decisionContext = DecisionContext {genUuid = Uuid.generate}
    (decision, newKeys) <-
      Decider.runDecisionWithKeys decisionContext
        (\reference -> context.services.eventStore.readErasableKey reference |> Task.mapError toText)
        (decideFor authorized currentEntity)
        |> Task.mapError DecisionExecutionFailed
    case decision of
      RejectCommand reason -> Task.yield (Rejected state.attemptStream reason)
      AcceptCommand insertionType events ->
        appendAcceptedEvents context authorized state (AcceptedDecision insertionType events newKeys)


appendAcceptedEvents ::
  forall entity event.
  ( Event event,
    Entity entity,
    entity ~ EntityOf event,
    ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Show (EntityIdType entity),
    Show event
  ) =>
  ExecutionContext entity event ->
  AuthorizedCommand (EntityIdType entity) entity event ->
  AttemptState entity ->
  AcceptedDecision event ->
  Task CommandExecutionFailure CommandOutcome
appendAcceptedEvents context authorized state (AcceptedDecision insertionType events newKeys) = do
  let currentEntity = state.attemptFetched |> Maybe.map (\fetched -> fetched.state)
  case validateInsertion insertionType currentEntity of
    Just rejection -> Task.yield (Rejected state.attemptStream (insertionRejectionText rejection))
    Nothing -> do
      finalStreamId <- chooseStream authorized state events
      if Array.isEmpty events
        then Task.yield (Accepted finalStreamId state.attemptRetries Array.empty)
        else appendEventsToStore context authorized state (AcceptedDecision insertionType events newKeys) finalStreamId


appendEventsToStore ::
  forall entity event.
  ( Event event,
    Entity entity,
    entity ~ EntityOf event,
    ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Show (EntityIdType entity),
    Show event
  ) =>
  ExecutionContext entity event ->
  AuthorizedCommand (EntityIdType entity) entity event ->
  AttemptState entity ->
  AcceptedDecision event ->
  StreamId.StreamId ->
  Task CommandExecutionFailure CommandOutcome
appendEventsToStore context authorized state (AcceptedDecision insertionType events newKeys) finalStreamId = do
  let dependencies = context.services
  payload <- Event.payloadFromEvents dependencies.entityName finalStreamId events
  let finalInsertionType = insertionTypeForEntity insertionType state.attemptFetched
  let attributedInsertions = payload.insertions |> Array.map (Source.attributeInsertionMetadata context.source)
  let positionedInsertions = positionInsertions finalInsertionType attributedInsertions
  let payloadWithType = payload {insertionType = finalInsertionType, insertions = positionedInsertions}
  insertResult <-
    (if Array.isEmpty newKeys
      then dependencies.eventStore.insert payloadWithType
      else dependencies.eventStore.insertWithErasableKeys newKeys payloadWithType)
      |> Task.asResult
  case insertResult of
    Ok _ -> do
      let metadata = positionedInsertions |> Array.map (\insertion -> insertion.metadata)
      Task.yield (Accepted finalStreamId state.attemptRetries metadata)
    Err (EventStore.InsertionError Event.ConsistencyCheckFailed) ->
      handleConflict context authorized state finalStreamId
    Err eventStoreError -> Task.yield (AppendFailed finalStreamId state.attemptRetries eventStoreError)


chooseStream ::
  forall entity event.
  ( Event event,
    entity ~ EntityOf event,
    ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Show (EntityIdType entity)
  ) =>
  AuthorizedCommand (EntityIdType entity) entity event ->
  AttemptState entity ->
  Array event ->
  Task CommandExecutionFailure StreamId.StreamId
chooseStream authorized state events = case state.attemptStream of
  Just streamId -> Task.yield streamId
  Nothing -> do
    let entityIds = events |> Array.map getEventEntityIdImpl
    case Array.first entityIds of
      Nothing -> Task.throw (InvalidCommandEvents "No events to extract entity ID from")
      Just firstId -> do
        let matchingCount = entityIds |> Array.takeIf (\entityId -> entityId == firstId) |> Array.length
        if matchingCount == Array.length entityIds
          then Task.yield (toStreamId firstId |> scopeStream authorized)
          else do
            let idsText = entityIds |> Array.map toText |> Text.joinWith ", "
            Task.throw (InvalidCommandEvents [fmt|Events have different entity IDs: #{idsText}|])


handleConflict ::
  forall entity event.
  ( Event event,
    Entity entity,
    entity ~ EntityOf event,
    ToStreamId (EntityIdType entity),
    Eq (EntityIdType entity),
    Show (EntityIdType entity),
    Show event
  ) =>
  ExecutionContext entity event ->
  AuthorizedCommand (EntityIdType entity) entity event ->
  AttemptState entity ->
  StreamId.StreamId ->
  Task CommandExecutionFailure CommandOutcome
handleConflict context authorized state streamId = do
  let maxRetries = 10
  let nextRetry = state.attemptRetries + 1
  let progress =
        if state.attemptRetries < maxRetries
          then Reporting.RetryScheduled nextRetry
          else Reporting.RetryLimitReached nextRetry
  Reporting.retry context.source progress
  if state.attemptRetries < maxRetries
    then do
      awaitWithJitter state.attemptRetries
      refetchResult <- context.services.entityFetcher.fetch context.services.entityName streamId |> Task.asResult
      case refetchResult of
        Ok (EntityFound fetched) ->
          runAttempt context authorized state {attemptRetries = nextRetry, attemptFetched = Just fetched, attemptStream = Just streamId}
        Ok EntityNotFound ->
          runAttempt context authorized state {attemptRetries = nextRetry, attemptFetched = Nothing, attemptStream = Just streamId}
        Err _ -> Task.throw (RetryRefetchFailed "Failed to refetch entity for retry")
    else Task.yield (RetriesExhausted streamId state.attemptRetries)
