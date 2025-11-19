module Test.Service.CommandHandler.Core (
  -- Re-export command test helpers
  module Test.Service.Command.Core,
  -- CommandHandler result types
  CommandHandlerResult (..),
) where

import Core
import Service.Event.StreamId (StreamId)
import Test.Service.Command.Core


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
