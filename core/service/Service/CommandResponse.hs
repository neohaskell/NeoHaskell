-- | DEPRECATED: Use Service.Response instead.
--
-- This module is kept for backward compatibility during the transition.
module Service.CommandResponse (
  CommandResponse (..),
  fromHandlerResult,
) where

import Service.CommandExecutor.Core (ExecutionResult)
import Service.Response (CommandResponse (..), fromExecutionResult)


-- | DEPRECATED: Use 'fromExecutionResult' from Service.Response
fromHandlerResult :: ExecutionResult -> CommandResponse
fromHandlerResult = fromExecutionResult
