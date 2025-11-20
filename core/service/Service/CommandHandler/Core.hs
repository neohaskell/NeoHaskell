{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
) where

import Core
import Service.Command (Command (..), EventOf)
import Service.EntityFetcher.Core (EntityFetcher)
import Service.Event (EntityName)
import Service.Event.StreamId (StreamId)
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


-- | CommandHandler handles command execution with retry logic and event persistence
data CommandHandler event = CommandHandler
  { eventStore :: EventStore event,
    maxRetries :: Int,
    retryDelayMs :: Int
  }
  deriving (Generic)


-- | Execute a command against an entity stream
--
--   This function:
--   1. Fetches the current entity state using the EntityFetcher
--   2. Calls the command's decide function with the entity state
--   3. If accepted, inserts the events into the EventStore
--   4. Handles optimistic concurrency by retrying on consistency failures
--   5. Returns CommandHandlerResult indicating success, rejection, or failure
execute ::
  forall command entity event error.
  ( Command command,
    entity ~ EntityOf command,
    event ~ EventOf entity
  ) =>
  EventStore event ->
  EntityFetcher entity event ->
  EntityName ->
  command ->
  Task error CommandHandlerResult
execute _eventStore _entityFetcher _entityName _command = do
  Task.yield (panic "Service.CommandHandler.execute not yet implemented")
