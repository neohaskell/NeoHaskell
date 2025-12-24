{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | DEPRECATED: Use Service.CommandExecutor.Core instead.
--
-- This module is kept for backward compatibility during the transition.
module Service.CommandHandler.Core (
  CommandHandler,
  CommandHandlerResult,
  pattern CommandAccepted,
  pattern CommandRejected,
  pattern CommandFailed,
  execute,
) where

import Service.CommandExecutor.Core (
  CommandExecutor,
  ExecutionResult,
  execute,
 )
import Basics (Int)
import Service.CommandExecutor.Core qualified as Executor
import Service.Event.StreamId (StreamId)
import Text (Text)


-- | DEPRECATED: Use 'CommandExecutor' from Service.CommandExecutor.Core
type CommandHandler = CommandExecutor


-- | DEPRECATED: Use 'ExecutionResult' from Service.CommandExecutor.Core
type CommandHandlerResult = ExecutionResult


-- | Pattern synonym for backward compatibility
pattern CommandAccepted :: StreamId -> Int -> Int -> CommandHandlerResult
pattern CommandAccepted {streamId, eventsAppended, retriesAttempted} =
  Executor.CommandAccepted streamId eventsAppended retriesAttempted


-- | Pattern synonym for backward compatibility
pattern CommandRejected :: Text -> CommandHandlerResult
pattern CommandRejected {reason} = Executor.CommandRejected reason


-- | Pattern synonym for backward compatibility
pattern CommandFailed :: Text -> Int -> CommandHandlerResult
pattern CommandFailed {error, retriesAttempted} = Executor.CommandFailed error retriesAttempted


{-# COMPLETE CommandAccepted, CommandRejected, CommandFailed #-}
