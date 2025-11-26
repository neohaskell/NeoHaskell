{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
  deriveCommand,
) where

import Array qualified
import Core
import Service.Command.Core qualified as Command
import Service.CommandHandler.TH (deriveCommand)
import Service.EntityFetcher.Core (EntityFetcher)
import Service.EntityFetcher.Core qualified as EntityFetcher
import Service.Event (EntityName, InsertionPayload (..))
import Service.Event qualified as Event
import Service.Event.StreamId qualified as StreamId
import Service.EventStore.Core (EventStore)
import Service.EventStore.Core qualified as EventStore
import Task qualified
import ToText qualified
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


execute ::
  forall command commandEntity commandEvent.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity,
    ToStreamId (EntityIdType command),
    IsMultiTenant command ~ 'False
  ) =>
  EventStore commandEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  command ->
  Task Text CommandHandlerResult
execute eventStore entityFetcher entityName command = do
  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ PHASE 1: ENTITY RESOLUTION                                              │
  -- └─────────────────────────────────────────────────────────────────────────┘

  -- Extract the entity ID from the command
  let maybeEntityId = (getEntityIdImpl @command) command

  -- Helper function to fetch entity with error handling
  let fetchEntity streamId = do
        result <-
          entityFetcher.fetch entityName streamId
            |> Task.asResult

        case result of
          Ok entity -> do
            Task.yield (Just entity, Just streamId)
          Err _fetchError -> do
            -- If fetch fails (e.g., stream not found), treat as new entity
            Task.yield (Nothing, Just streamId)

  -- Resolve the entity state based on whether we have an entity ID
  (maybeEntity, maybeStreamId) <- case maybeEntityId of
    Just entityId -> do
      let streamId = toStreamId entityId
      fetchEntity streamId
    Nothing -> do
      -- No entity ID means we're creating a new entity
      Task.yield (Nothing, Nothing)

  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ PHASE 2: DECISION & PERSISTENCE (with retry loop)                       │
  -- └─────────────────────────────────────────────────────────────────────────┘

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
            -- Determine the stream ID
            finalStreamId <- case currentStreamId of
              Just sid -> Task.yield sid
              Nothing -> do
                -- FIXME: HOW TO MAKE THIS WORK??
                -- Must extract from first event (for new entities)
                -- This requires the event to have a field we can use as stream ID
                -- For now, we'll generate a new UUID and convert to StreamId
                uuid <- Uuid.generate
                let streamId = StreamId.fromText (ToText.toText uuid)
                Task.yield streamId

            -- Build insertion payload
            payload <-
              Event.payloadFromEvents entityName finalStreamId events

            -- Map insertion types appropriately:
            -- ExistingStream and StreamCreation are checked by Decision.accept* functions,
            -- but for EventStore we use AnyStreamState since we don't track exact positions
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
                    -- Re-fetch the entity with latest state using the helper
                    refetchResult <-
                      entityFetcher.fetch entityName finalStreamId
                        |> Task.asResult

                    case refetchResult of
                      Ok freshEntity -> do
                        -- Retry with fresh state
                        retryLoop (retryCount + 1) (Just freshEntity) (Just finalStreamId)
                      Err _refetchError -> do
                        -- Stream disappeared or other error, retry with Nothing
                        retryLoop (retryCount + 1) Nothing (Just finalStreamId)
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