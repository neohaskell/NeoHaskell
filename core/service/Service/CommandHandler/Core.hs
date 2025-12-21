{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
) where

import Array qualified
import AsyncTask qualified
import Basics
import Float qualified
import Int qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Command.Core (Command (..), CommandResult (..), EntityOf, EventOf)
import Service.Command.Core qualified as Command
import Service.EntityFetcher.Core (EntityFetchResult (..), EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
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


-- | Result of executing a command through the CommandHandler
data CommandHandlerResult
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


data CommandHandler event = CommandHandler
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


execute ::
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity,
    HasField "entityId" commandEvent (EntityIdType command),
    ToStreamId (EntityIdType command),
    Eq (EntityIdType command),
    Show (EntityIdType command),
    IsMultiTenant command ~ 'False
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  command ->
  Task Text CommandHandlerResult
execute eventStore entityFetcher entityName command = do
  -- Extract the entity ID from the command
  let maybeEntityId = (getEntityIdImpl @command) command

  -- Helper function to fetch entity with error handling
  let fetchEntity streamId = do
        result <-
          entityFetcher.fetch entityName streamId
            |> Task.asResult

        case result of
          Ok (EntityFound entity) -> do
            Task.yield (Just entity, Just streamId)
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
              Command.DecisionContext
                { genUuid = Uuid.generate
                }

        -- Execute the decision logic
        let decision = (decideImpl @command) command currentEntity
        commandResult <- Command.runDecision decisionContext decision

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
                    let eventEntityIds = events |> Array.map (\e -> e.entityId)
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
                          Ok (EntityFound freshEntity) -> do
                            -- Retry with fresh state
                            retryLoop (retryCount + 1) (Just freshEntity) (Just finalStreamId)
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