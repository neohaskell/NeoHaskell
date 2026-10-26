-- | Represent failures at the executor's internal stages. Construct a
-- stage-specific failure and use 'message' at the service response boundary.
module Service.CommandExecutor.CommandExecutionFailure (
  CommandExecutionFailure (..),
  message,
) where

import Basics
import Text (Text)


data CommandExecutionFailure
  = RequestContextFailed Text
  | EntityFetchFailed Text
  | RetryRefetchFailed Text
  | DecisionExecutionFailed Text
  | InvalidCommandEvents Text
  | SynchronousExecutionFailed Text
  deriving (Eq, Show)


message :: CommandExecutionFailure -> Text
message failure = case failure of
  RequestContextFailed detail -> detail
  EntityFetchFailed detail -> detail
  RetryRefetchFailed detail -> detail
  DecisionExecutionFailed detail -> detail
  InvalidCommandEvents detail -> detail
  SynchronousExecutionFailed detail -> detail
