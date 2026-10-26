-- | Hold the executor's accepted and failed outcomes. Convert an outcome to
-- 'ExecutionResult' when returning a command result to its caller.
module Service.CommandExecutor.CommandOutcome (
  CommandOutcome (..),
  executionResult,
) where

import Array (Array)
import Array qualified
import Basics
import Maybe (Maybe)
import Service.AccessControl (AccessError)
import Service.CommandExecutor.ExecutionResult (ExecutionResult (..))
import Service.Event.EventMetadata (EventMetadata)
import Service.Event.StreamId (StreamId)
import Service.EventStore.Core qualified as EventStore
import Text (Text)
import ToText (toText)


data CommandOutcome
  = Accepted StreamId Int (Array EventMetadata)
  | Rejected (Maybe StreamId) Text
  | Unauthorized AccessError
  | AppendFailed StreamId Int EventStore.Error
  | RetriesExhausted StreamId Int
  deriving (Eq, Show)


executionResult :: CommandOutcome -> ExecutionResult
executionResult outcome = case outcome of
  Accepted streamId retryCount appendedMetadata ->
    CommandAccepted
      { streamId,
        eventsAppended = Array.length appendedMetadata,
        retriesAttempted = retryCount
      }
  Rejected _ reason -> CommandRejected {reason}
  Unauthorized authError -> CommandUnauthorized {authError}
  AppendFailed streamId retryCount eventStoreError -> do
    let error = [fmt|Event store insert failed for stream #{toText streamId}: #{toText eventStoreError}|]
    CommandFailed {error, retriesAttempted = retryCount}
  RetriesExhausted _ retryCount ->
    CommandFailed
      { error = "Max retries exceeded due to concurrent modifications",
        retriesAttempted = retryCount
      }
