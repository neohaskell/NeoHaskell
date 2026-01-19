{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandExecutor.Core (
  CommandExecutor (..),
  ExecutionResult (..),
  execute,
) where

import Array qualified
import AsyncTask qualified
import Basics
import Decider (CommandResult (..), DecisionContext (..), runDecision)
import Float qualified
import Int qualified
import Json qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Auth (RequestContext)
import Service.Command (Event (..))
import Service.Command.Core (Command (..), Entity (..), EntityOf, EventOf)
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher, FetchedEntity (..))
import Service.EntityFetcher.Core qualified
import Service.Event (EntityName, InsertionPayload (..), InsertionType (..))
import Service.Event qualified as Event
import Service.Event.StreamId (StreamId, ToStreamId (..))
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified
import ToText (toText)
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
    IsMultiTenant command ~ 'False
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  RequestContext ->
  command ->
  Task Text ExecutionResult
execute eventStore entityFetcher entityName requestContext command = do
  -- Extract the entity ID from the command
  let maybeEntityId = (getEntityIdImpl @command) command

  -- Helper function to fetch entity with error handling
  let fetchEntity streamId = do
        result <-
          entityFetcher.fetch entityName streamId
            |> Task.asResult

        case result of
          Ok (EntityFound fetchedEntity) -> do
            -- Extract just the state from FetchedEntity
            Task.yield (Just fetchedEntity.state, Just streamId)
          Ok EntityNotFound -> do
            -- No events for this stream yet, entity doesn't exist
            Task.yield (Nothing, Just streamId)
          Err error -> do
            -- Actual error occurred (network, permissions, corruption)
            -- Re-throw the error to propagate it properly
            Task.throw (toText error)

  -- Resolve the entity state based on whether we have an entity ID
  (maybeEntity, maybeStreamId) <- case maybeEntityId of
    Just entityId -> do
      let streamId = toStreamId entityId
      fetchEntity streamId
    Nothing -> do
      -- No entity ID means we're creating a new entity
      Task.yield (Nothing, Nothing)

  let maxRetries = 10

  let retryLoop retryCount currentEntity currentStreamId = do
        -- TODO: Extract decision context into service context
        let decisionContext =
              DecisionContext
                { genUuid = Uuid.generate
                }

        -- Execute the decision logic with RequestContext
        let decision = (decideImpl @command) command currentEntity requestContext
        commandResult <- runDecision decisionContext decision

        case commandResult of
          RejectCommand reason -> do
            Task.yield
              CommandRejected
                { reason = reason
                }
          AcceptCommand insertionType events -> do
            -- Validate insertion type constraints against current entity state
            let validationError = case insertionType of
                  StreamCreation ->
                    case currentEntity of
                      Just _ -> Just "Cannot create entity: entity already exists"
                      Nothing -> Nothing
                  ExistingStream ->
                    case currentEntity of
                      Nothing -> Just "Cannot update entity: entity does not exist"
                      Just _ -> Nothing
                  _ -> Nothing -- InsertAfter and AnyStreamState don't have these constraints
            case validationError of
              Just errorMsg ->
                Task.yield
                  CommandRejected
                    { reason = errorMsg
                    }
              Nothing -> do
                -- Determine the stream ID
                finalStreamId <- case currentStreamId of
                  Just sid -> Task.yield sid
                  Nothing -> do
                    let eventEntityIds = events |> Array.map (\e -> getEventEntityIdImpl e)
                    -- Check if all entity IDs are the same
                    case Array.first eventEntityIds of
                      Nothing -> do
                        Task.throw "No events to extract entity ID from"
                      Just firstId -> do
                        let matchingCount = eventEntityIds |> Array.takeIf (\id -> id == firstId) |> Array.length
                        let totalCount = Array.length eventEntityIds
                        if matchingCount == totalCount
                          then do
                            let streamId = toStreamId firstId
                            Task.yield streamId
                          else do
                            let idsText = eventEntityIds |> Array.map toText |> Text.joinWith ", "
                            Task.throw [fmt|Events have different entity IDs: #{idsText}|]

                -- Build insertion payload
                payload <-
                  Event.payloadFromEvents entityName finalStreamId events

                -- Map insertion types to EventStore types:
                -- We use AnyStreamState for most cases since we don't track exact positions
                -- Only InsertAfter carries position information
                let finalInsertionType = case insertionType of
                      InsertAfter pos -> InsertAfter pos
                      _ -> AnyStreamState

                let payloadWithType = payload {insertionType = finalInsertionType}

                -- Persist to event store
                insertResult <-
                  eventStore.insert payloadWithType
                    |> Task.asResult

                case insertResult of
                  Ok _success -> do
                    let eventsCount = Array.length events
                    Task.yield
                      CommandAccepted
                        { streamId = finalStreamId,
                          eventsAppended = eventsCount,
                          retriesAttempted = retryCount
                        }
                  Err (EventStore.InsertionError Event.ConsistencyCheckFailed) -> do
                    -- Concurrency conflict, retry if we haven't exceeded max retries
                    if retryCount < maxRetries
                      then do
                        -- Wait with exponential backoff and jitter
                        awaitWithJitter retryCount

                        -- Re-fetch the entity with latest state
                        refetchResult <-
                          entityFetcher.fetch entityName finalStreamId
                            |> Task.asResult

                        case refetchResult of
                          Ok (EntityFound freshFetchedEntity) -> do
                            -- Retry with fresh state (extract state from FetchedEntity)
                            retryLoop (retryCount + 1) (Just freshFetchedEntity.state) (Just finalStreamId)
                          Ok EntityNotFound -> do
                            -- Entity doesn't exist, retry with Nothing
                            retryLoop (retryCount + 1) Nothing (Just finalStreamId)
                          Err _error -> do
                            -- Actual fetch error occurred, propagate it
                            Task.throw "Failed to refetch entity for retry"
                      else do
                        Task.yield
                          CommandFailed
                            { error = "Max retries exceeded due to concurrent modifications",
                              retriesAttempted = retryCount
                            }
                  Err _eventStoreError -> do
                    let errorText = "Event store error during insertion"
                    Task.yield
                      CommandFailed
                        { error = errorText,
                          retriesAttempted = retryCount
                        }

  -- Start the retry loop
  retryLoop 0 maybeEntity maybeStreamId
