module Service.Response (
  CommandResponse (..),
  fromExecutionResult,
) where

import Basics
import Json qualified
import Service.CommandExecutor.Core (ExecutionResult (..))
import Service.Event.StreamId qualified as StreamId
import Text (Text)


-- | Domain-level response for command execution.
-- This is what gets serialized to API clients.
-- Internal details like retry counts and event counts are not exposed.
data CommandResponse
  = Accepted
      { entityId :: Text
      }
  | Rejected
      { reason :: Text
      }
  | Failed
      { error :: Text
      }
  deriving (Eq, Show, Ord, Generic)


instance Json.ToJSON CommandResponse


instance Json.FromJSON CommandResponse


-- | Convert internal ExecutionResult to client-facing CommandResponse
fromExecutionResult :: ExecutionResult -> CommandResponse
fromExecutionResult result = case result of
  CommandAccepted {streamId} ->
    Accepted
      { entityId = StreamId.toText streamId
      }
  CommandRejected {reason} ->
    Rejected
      { reason = reason
      }
  CommandFailed {error} ->
    Failed
      { error = error
      }
