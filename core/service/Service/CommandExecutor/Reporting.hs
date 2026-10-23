{-# LANGUAGE ScopedTypeVariables #-}

module Service.CommandExecutor.Reporting (
  ReportingConfig (..),
  run,
  RetryProgress (..),
  retry,
) where

import Array (Array)
import Array qualified
import Basics
import Json qualified
import Log qualified
import Maybe (Maybe (..))
import Maybe qualified
import Result (Result (..))
import Service.Auth (RequestContext (..))
import Service.CommandExecutor.CommandErrorClass qualified as CommandErrorClass
import Service.CommandExecutor.CommandExecutionFailure (CommandExecutionFailure (..))
import Service.CommandExecutor.CommandFailureStage qualified as CommandFailureStage
import Service.CommandExecutor.CommandLogSummary (CommandLogSummary)
import Service.CommandExecutor.CommandLogSummary qualified as CommandLogSummary
import Service.CommandExecutor.CommandOutcome (CommandOutcome (..))
import Service.CommandExecutor.CommandOutcome qualified as CommandOutcome
import Service.CommandExecutor.CommandResultLabel qualified as CommandResultLabel
import Service.CommandExecutor.ExecutionResult (ExecutionResult)
import Service.CommandExecutor.Source (CommandSource)
import Service.CommandExecutor.Source qualified as Source
import Service.CommandResultReceipt.Internal qualified as CommandResultReceipt
import Service.Event.EventMetadata (EventMetadata)
import Service.Event.EventMetadata qualified as EventMetadata
import Service.Event.StreamId qualified as StreamId
import Task (Task)
import Task qualified
import Text (Text)
import Uuid qualified
import Var (Var)
import Var qualified


data RetryProgress
  = RetryScheduled Int
  | RetryLimitReached Int


data ReportingConfig = ReportingConfig
  { commandName :: Text,
    entityName :: Text,
    source :: CommandSource
  }


data ReportingScope = ReportingScope
  { reportingConfig :: ReportingConfig,
    completed :: Var Bool,
    activeSource :: Var CommandSource
  }


run ::
  ReportingConfig ->
  (CommandSource -> Task CommandExecutionFailure CommandOutcome) ->
  Task CommandExecutionFailure ExecutionResult
run reportingConfig execute = Task.mask \restore -> do
  completed <- Var.new False
  activeSource <- Var.new reportingConfig.source
  let scope = ReportingScope {reportingConfig, completed, activeSource}
  Task.finally (reportInterruption scope) (restore (runExecution scope reportingConfig.source execute))


runExecution ::
  ReportingScope ->
  CommandSource ->
  (CommandSource -> Task CommandExecutionFailure CommandOutcome) ->
  Task CommandExecutionFailure ExecutionResult
runExecution scope source execute = do
  normalizedResult <- captureResult (\_ -> Source.normalize source)
  case normalizedResult of
    Err message -> failExecution scope source (RequestContextFailed message)
    Ok (Err failure) -> failExecution scope source failure
    Ok (Ok normalizedSource) -> do
      Var.set normalizedSource scope.activeSource
      runNormalized scope normalizedSource execute


runNormalized ::
  ReportingScope ->
  CommandSource ->
  (CommandSource -> Task CommandExecutionFailure CommandOutcome) ->
  Task CommandExecutionFailure ExecutionResult
runNormalized scope source execute = do
  executionResult <- captureResult (\_ -> execute source)
  case executionResult of
    Err message -> failExecution scope source (SynchronousExecutionFailed message)
    Ok (Err failure) -> failExecution scope source failure
    Ok (Ok outcome) -> finishOutcome scope source outcome


finishOutcome :: ReportingScope -> CommandSource -> CommandOutcome -> Task CommandExecutionFailure ExecutionResult
finishOutcome scope source outcome = do
  result <- captureResult (\_ -> Task.evaluate (CommandOutcome.executionResult outcome))
  case result of
    Err message -> failExecution scope source (SynchronousExecutionFailed message)
    Ok (Err failure) -> failExecution scope source failure
    Ok (Ok publicResult) -> do
      reportOutcome scope source outcome
      Task.yield publicResult


captureResult ::
  forall value err.
  (Unit -> Task CommandExecutionFailure value) ->
  Task err (Result Text (Result CommandExecutionFailure value))
captureResult makeAction = do
  let guarded :: Task CommandExecutionFailure (Result CommandExecutionFailure value)
      guarded = do
        evaluatedAction <- Task.evaluate (makeAction unit)
        Task.asResult evaluatedAction
  Task.asResultSafe guarded


failExecution ::
  ReportingScope ->
  CommandSource ->
  CommandExecutionFailure ->
  Task CommandExecutionFailure value
failExecution scope source failure = do
  Task.mask \_ -> publishResult scope source (CommandLogSummary.fromFailure failure)
  Task.throw failure


reportOutcome :: ReportingScope -> CommandSource -> CommandOutcome -> Task err Unit
reportOutcome scope source outcome = Task.mask \_ -> do
  publishResult scope source (CommandLogSummary.fromOutcome outcome)
  case outcome of
    Accepted streamId _ metadata -> reportAppendedEvents scope source streamId metadata
    _ -> Task.yield unit


reportInterruption :: ReportingScope -> Task err Unit
reportInterruption scope = do
  wasCompleted <- Var.get scope.completed
  Task.unless wasCompleted do
    source <- Var.get scope.activeSource
    Task.mask \_ -> publishResult scope source CommandLogSummary.interrupted


publishResult :: ReportingScope -> CommandSource -> CommandLogSummary -> Task err Unit
publishResult scope source summary = do
  let report = emitCommandResult scope source summary :: Task Text Unit
  let requestId = (Source.sourceContext source).requestId
  let deliver = do
        maybeReported <- CommandResultReceipt.reportIfRegistered requestId report
        case maybeReported of
          Nothing -> report
          Just _ -> Task.yield unit
  _ <- deliver |> Task.asResultSafe
  Var.set True scope.completed


reportAppendedEvents ::
  ReportingScope ->
  CommandSource ->
  StreamId.StreamId ->
  Array EventMetadata ->
  Task err Unit
reportAppendedEvents scope source streamId metadata = do
  let context = Source.sourceContext source
  let requestIdText = Uuid.toText context.requestId
  let streamIdText = StreamId.toText streamId
  let report = metadata |> Task.forEach (emitEventAppended scope streamIdText requestIdText)
  _ <- (report :: Task Text Unit) |> Task.asResultSafe
  Task.yield unit


optionalTextField :: Text -> Maybe Text -> Array (Text, Json.Value)
optionalTextField fieldName maybeValue = case maybeValue of
  Just value -> Array.wrap (fieldName Json..= value)
  Nothing -> Array.empty


eventTraceFields :: Text -> EventMetadata -> Array (Text, Json.Value)
eventTraceFields requestIdText metadata =
  ["requestId" Json..= requestIdText]
    |> Array.append (optionalTextField "correlationId" metadata.correlationId)
    |> Array.append (optionalTextField "causationId" metadata.causationId)
    |> Array.append (optionalTextField "relatedUserSub" metadata.relatedUserSub)


emitCommandResult :: ReportingScope -> CommandSource -> CommandLogSummary -> Task err Unit
emitCommandResult scope source summary = do
  let fields =
        [ "kind" Json..= ("command_result" :: Text),
          "result" Json..= CommandResultLabel.text (CommandLogSummary.resultLabel summary),
          "component" Json..= ("CommandExecutor" :: Text),
          "commandName" Json..= scope.reportingConfig.commandName,
          "entityName" Json..= scope.reportingConfig.entityName,
          "eventsAppended" Json..= CommandLogSummary.eventCount summary
        ]
          |> Array.append (Source.commandTraceFields source)
          |> Array.append (optionalTextField "streamId" (CommandLogSummary.streamId summary |> Maybe.map StreamId.toText))
          |> Array.append (optionalTextField "failureStage" (CommandLogSummary.failureStage summary |> Maybe.map CommandFailureStage.text))
          |> Array.append (optionalTextField "errorClass" (CommandLogSummary.errorClass summary |> Maybe.map CommandErrorClass.text))
  Log.recordIsolated (CommandLogSummary.level summary) fields "Command result"
    |> Task.ignoreError


emitEventAppended :: ReportingScope -> Text -> Text -> EventMetadata -> Task err Unit
emitEventAppended scope streamIdText requestIdText metadata = do
  let fields =
        [ "kind" Json..= ("event_appended" :: Text),
          "eventId" Json..= Uuid.toText metadata.eventId,
          "component" Json..= ("CommandExecutor" :: Text),
          "commandName" Json..= scope.reportingConfig.commandName,
          "entityName" Json..= scope.reportingConfig.entityName,
          "streamId" Json..= streamIdText
        ]
          |> Array.append (eventTraceFields requestIdText metadata)
  Log.recordIsolated Log.Info fields "Event appended"
    |> Task.ignoreError


retry :: CommandSource -> RetryProgress -> Task err Unit
retry source progress = do
  let (attempt, retryResult) = case progress of
        RetryScheduled number -> (number, "scheduled" :: Text)
        RetryLimitReached number -> (number, "exhausted" :: Text)
  let fields =
        [ "kind" Json..= ("retry" :: Text),
          "result" Json..= retryResult,
          "attempt" Json..= attempt,
          "errorClass" Json..= CommandErrorClass.text CommandErrorClass.ConsistencyCheckFailedClass
        ]
          |> Array.append (Source.commandTraceFields source)
  Log.recordIsolated Log.Warn fields "Command append conflict"
    |> Task.ignoreError
