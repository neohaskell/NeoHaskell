-- | Name the stage that failed for structured command logs. Use 'fromFailure'
-- for executor failures and 'text' when writing the log field.
module Service.CommandExecutor.CommandFailureStage (
  CommandFailureStage (..),
  fromFailure,
  text,
) where

import Basics
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure (..))
import Text (Text)


data CommandFailureStage
  = RequestContextStage
  | FetchStage
  | RetryRefetchStage
  | DecisionStage
  | AuthorizationStage
  | AppendStage
  | ExecutionStage
  deriving (Eq, Show)


fromFailure :: CommandExecutionFailure -> CommandFailureStage
fromFailure failure = case failure of
  RequestContextFailed _ -> RequestContextStage
  EntityFetchFailed _ -> FetchStage
  RetryRefetchFailed _ -> RetryRefetchStage
  DecisionExecutionFailed _ -> DecisionStage
  InvalidCommandEvents _ -> ExecutionStage
  SynchronousExecutionFailed _ -> ExecutionStage


text :: CommandFailureStage -> Text
text stage = case stage of
  RequestContextStage -> "request_context"
  FetchStage -> "fetch"
  RetryRefetchStage -> "retry_refetch"
  DecisionStage -> "decision"
  AuthorizationStage -> "authorization"
  AppendStage -> "append"
  ExecutionStage -> "execution"
