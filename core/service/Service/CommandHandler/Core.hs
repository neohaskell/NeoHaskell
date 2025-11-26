{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
  deriveCommand,
) where

import Core
import Service.CommandHandler.TH (deriveCommand)
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event (EntityName)
import Service.EventStore.Core (EventStore)
import Task qualified


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
  forall command commandEntity commandEvent appEvent error.
  ( Command command,
    commandEntity ~ EntityOf command,
    commandEvent ~ EventOf commandEntity
  ) =>
  EventStore appEvent ->
  EntityFetcher commandEntity commandEvent ->
  EntityName ->
  command ->
  Task error CommandHandlerResult
execute _eventStore _entityFetcher _entityName _command = do
  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ IMPLEMENTATION PLAN: Command Handler Execution Algorithm                │
  -- └─────────────────────────────────────────────────────────────────────────┘
  --
  -- This function implements the core command execution flow with optimistic
  -- concurrency control and automatic retry logic for handling concurrent
  -- modifications.
  --
  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ PHASE 1: ENTITY RESOLUTION                                              │
  -- └─────────────────────────────────────────────────────────────────────────┘
  --
  -- 1. Call command.getEntityIdImpl to extract entity ID
  --    - Returns Maybe EntityIdType (Text or custom type)
  --    - Nothing means creating a new entity
  --    - Just entityId means operating on existing entity
  --
  -- 2. If entityId exists (Some):
  --    a. Convert EntityIdType to StreamId using ToStreamId trait
  --    b. Call entityFetcher.fetch with entityName and streamId
  --    c. On success: store entity state and stream version
  --    d. On EntityFetcher.EventStoreError (EventStore.StreamNotFound):
  --       - Set entity = Nothing (stream doesn't exist yet)
  --       - Set currentStreamVersion = Nothing
  --    e. On other errors: propagate error
  --
  -- 3. If entityId is Nothing:
  --    - Set entity = Nothing (new entity creation)
  --    - Set currentStreamVersion = Nothing
  --
  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ PHASE 2: DECISION & PERSISTENCE (with retry loop)                       │
  -- └─────────────────────────────────────────────────────────────────────────┘
  --
  -- 4. Setup retry loop:
  --    - maxRetries = 10
  --    - retryCount = 0
  --
  -- 5. Decision phase:
  --    a. Create DecisionContext with Uuid.generate as genUuid
  --    b. Call command.decideImpl with current entity state
  --       - Takes: command, Maybe entity
  --       - Returns: Decision commandEvent
  --    c. Call runDecision with context and decision
  --       - Returns: Task Text (CommandResult commandEvent)
  --    d. Handle CommandResult:
  --       - RejectCommand reason -> return CommandRejected
  --       - AcceptCommand insertionType events -> continue to step 6
  --
  -- 6. Extract or generate stream ID:
  --    - If we had maybeEntityId from step 1: use that
  --    - Otherwise: extract from first event in events array
  --      (events must have at least one element for new entities)
  --
  -- 7. Build InsertionPayload:
  --    a. Convert events to insertions using eventToInsertion helper
  --       - Generates UUID for each event
  --       - Creates EventMetadata for each event
  --    b. Build InsertionPayload:
  --       - entityName: from parameter
  --       - streamId: from step 6
  --       - insertionType: from CommandResult
  --       - insertions: from step 7a
  --
  -- 8. Persist events:
  --    a. Call eventStore.insert with payload
  --    b. On Success (InsertionSuccess):
  --       - Return CommandAccepted with streamId, event count, retry count
  --    c. On Error (InsertionError ConsistencyCheckFailed):
  --       - If retryCount < maxRetries:
  --         * Increment retryCount
  --         * Re-fetch entity (go back to step 2 with same streamId)
  --         * Continue loop (go back to step 5)
  --       - Else: Return CommandFailed "Max retries exceeded"
  --    d. On other EventStore errors:
  --       - Return CommandFailed with error message
  --
  -- ┌─────────────────────────────────────────────────────────────────────────┐
  -- │ KEY INVARIANTS                                                           │
  -- └─────────────────────────────────────────────────────────────────────────┘
  --
  -- - The retry loop ensures optimistic concurrency:
  --   When concurrent commands modify the same stream, one succeeds and others
  --   retry with the updated state
  --
  -- - The insertionType from Decision determines consistency guarantees:
  --   * StreamCreation: fails if stream exists
  --   * ExistingStream: fails if stream doesn't exist
  --   * InsertAfter pos: fails if stream position doesn't match
  --   * AnyStreamState: always succeeds (least safe)
  --
  -- - Entity fetcher returns current state by replaying all events
  --   This ensures decisions are made on the latest state after retries
  --
  Task.yield (panic "Service.CommandHandler.execute not yet implemented")