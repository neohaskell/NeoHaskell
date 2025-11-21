{-# LANGUAGE AllowAmbiguousTypes #-}

module Service.CommandHandler.Core (
  CommandHandler (..),
  CommandHandlerResult (..),
  execute,
) where

import Core
import Service.Command (EventOf)
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
  Task.yield (panic "Service.CommandHandler.execute not yet implemented")
