-- | Return a command's accepted, rejected, failed, or unauthorized result.
-- Service adapters serialize this result for command callers.
module Service.CommandExecutor.ExecutionResult (ExecutionResult (..)) where

import Basics
import Json qualified
import Service.AccessControl (AccessError)
import Service.Event.StreamId (StreamId)
import Text (Text)


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
