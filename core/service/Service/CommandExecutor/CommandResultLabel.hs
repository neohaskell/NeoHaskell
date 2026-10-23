module Service.CommandExecutor.CommandResultLabel (
  CommandResultLabel (..),
  text,
) where

import Basics
import Text (Text)


data CommandResultLabel
  = AcceptedResult
  | RejectedResult
  | UnauthorizedResult
  | AppendFailedResult
  | RetriesExhaustedResult
  | ExecutionFailedResult
  deriving (Eq, Show)


text :: CommandResultLabel -> Text
text result = case result of
  AcceptedResult -> "accepted"
  RejectedResult -> "rejected"
  UnauthorizedResult -> "unauthorized"
  AppendFailedResult -> "append_failed"
  RetriesExhaustedResult -> "retries_exhausted"
  ExecutionFailedResult -> "execution_failed"
