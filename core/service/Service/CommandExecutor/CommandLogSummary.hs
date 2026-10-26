-- | Summarize one command outcome for structured reporting. Build a summary
-- from the outcome or failure, then read its log fields.
module Service.CommandExecutor.CommandLogSummary (
  CommandLogSummary,
  fromOutcome,
  fromFailure,
  interrupted,
  resultLabel,
  level,
  eventCount,
  streamId,
  failureStage,
  errorClass,
) where

import Array qualified
import Basics
import Log qualified
import Maybe (Maybe (..))
import Service.AccessControl (AccessError)
import Service.CommandExecutor.CommandErrorClass (CommandErrorClass (..))
import Service.CommandExecutor.CommandErrorClass qualified as CommandErrorClass
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure)
import Service.CommandExecutor.CommandFailureStage (CommandFailureStage (..))
import Service.CommandExecutor.CommandFailureStage qualified as CommandFailureStage
import Service.CommandExecutor.CommandOutcome (CommandOutcome (..))
import Service.CommandExecutor.CommandResultLabel (CommandResultLabel (..))
import Service.Event.StreamId (StreamId)
import Service.EventStore.Core qualified as EventStore


data CommandLogSummary
  = AcceptedSummary StreamId Int
  | RejectedSummary (Maybe StreamId)
  | UnauthorizedSummary AccessError
  | AppendFailedSummary StreamId EventStore.Error
  | RetriesExhaustedSummary StreamId
  | ExecutionFailedSummary CommandExecutionFailure
  | InterruptedSummary


fromOutcome :: CommandOutcome -> CommandLogSummary
fromOutcome outcome = case outcome of
  Accepted acceptedStreamId _ appendedMetadata ->
    AcceptedSummary acceptedStreamId (Array.length appendedMetadata)
  Rejected maybeStreamId _ -> RejectedSummary maybeStreamId
  Unauthorized accessError -> UnauthorizedSummary accessError
  AppendFailed failedStreamId _ eventStoreError -> AppendFailedSummary failedStreamId eventStoreError
  RetriesExhausted exhaustedStreamId _ -> RetriesExhaustedSummary exhaustedStreamId


fromFailure :: CommandExecutionFailure -> CommandLogSummary
fromFailure failure = ExecutionFailedSummary failure


interrupted :: CommandLogSummary
interrupted = InterruptedSummary


resultLabel :: CommandLogSummary -> CommandResultLabel
resultLabel summary = case summary of
  AcceptedSummary _ _ -> AcceptedResult
  RejectedSummary _ -> RejectedResult
  UnauthorizedSummary _ -> UnauthorizedResult
  AppendFailedSummary _ _ -> AppendFailedResult
  RetriesExhaustedSummary _ -> RetriesExhaustedResult
  ExecutionFailedSummary _ -> ExecutionFailedResult
  InterruptedSummary -> ExecutionFailedResult


level :: CommandLogSummary -> Log.Level
level summary = case summary of
  AcceptedSummary _ _ -> Log.Info
  RejectedSummary _ -> Log.Warn
  UnauthorizedSummary _ -> Log.Warn
  AppendFailedSummary _ _ -> Log.Error
  RetriesExhaustedSummary _ -> Log.Error
  ExecutionFailedSummary _ -> Log.Error
  InterruptedSummary -> Log.Error


eventCount :: CommandLogSummary -> Int
eventCount summary = case summary of
  AcceptedSummary _ count -> count
  RejectedSummary _ -> 0
  UnauthorizedSummary _ -> 0
  AppendFailedSummary _ _ -> 0
  RetriesExhaustedSummary _ -> 0
  ExecutionFailedSummary _ -> 0
  InterruptedSummary -> 0


streamId :: CommandLogSummary -> Maybe StreamId
streamId summary = case summary of
  AcceptedSummary acceptedStreamId _ -> Just acceptedStreamId
  RejectedSummary maybeStreamId -> maybeStreamId
  UnauthorizedSummary _ -> Nothing
  AppendFailedSummary failedStreamId _ -> Just failedStreamId
  RetriesExhaustedSummary exhaustedStreamId -> Just exhaustedStreamId
  ExecutionFailedSummary _ -> Nothing
  InterruptedSummary -> Nothing


failureStage :: CommandLogSummary -> Maybe CommandFailureStage
failureStage summary = case summary of
  AcceptedSummary _ _ -> Nothing
  RejectedSummary _ -> Just DecisionStage
  UnauthorizedSummary _ -> Just AuthorizationStage
  AppendFailedSummary _ _ -> Just AppendStage
  RetriesExhaustedSummary _ -> Just AppendStage
  ExecutionFailedSummary failure -> Just (CommandFailureStage.fromFailure failure)
  InterruptedSummary -> Just ExecutionStage


errorClass :: CommandLogSummary -> Maybe CommandErrorClass
errorClass summary = case summary of
  AcceptedSummary _ _ -> Nothing
  RejectedSummary _ -> Just CommandRejectedClass
  UnauthorizedSummary accessError -> Just (CommandErrorClass.fromAccessError accessError)
  AppendFailedSummary _ eventStoreError -> Just (CommandErrorClass.fromEventStoreError eventStoreError)
  RetriesExhaustedSummary _ -> Just ConsistencyCheckFailedClass
  ExecutionFailedSummary failure -> Just (CommandErrorClass.fromFailure failure)
  InterruptedSummary -> Just ExecutionInterruptedClass
