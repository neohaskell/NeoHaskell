{-# LANGUAGE Strict #-}

-- | Owns one runner invocation and the outcome retained for interruption logs.
module Service.Integration.Dispatcher.RunnerExecution (
  RunnerExecution (..),
  new,
  logAll,
  run,
) where

import Array (Array)
import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Integration qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Service.Integration.Dispatcher.EventLogContext (EventLogContext)
import Service.Integration.Dispatcher.EventLogContext qualified as EventLogContext
import Service.Integration.Dispatcher.IntegrationOutcome (RunnerFailure (..), IntegrationOutcome (Processed))
import Service.Integration.Dispatcher.IntegrationOutcome qualified as IntegrationOutcome
import Service.Integration.Dispatcher.IntegrationResult (IntegrationResult (..), RunnerKind)
import Service.Integration.Dispatcher.RunnerLogState (RunnerLogState (..))
import Service.Integration.Dispatcher.RunnerLogState qualified as RunnerLogState
import Task (Task)
import Task qualified
import Text (Text)


data RunnerExecution runner = RunnerExecution
  { kind :: RunnerKind,
    ordinal :: Int,
    runner :: runner,
    logState :: ConcurrentVar RunnerLogState
  }


new :: RunnerKind -> Int -> runner -> Task err (RunnerExecution runner)
new kind ordinal runner = do
  logState <- ConcurrentVar.containing RunnerLogState.initial
  Task.yield RunnerExecution {kind, ordinal, runner, logState}


logAll :: EventLogContext -> ConcurrentVar Bool -> Array (RunnerExecution runner) -> Task err Unit
logAll context timeoutReached executions =
  executions |> Task.forEach (logOne context timeoutReached)


logOne :: EventLogContext -> ConcurrentVar Bool -> RunnerExecution runner -> Task err Unit
logOne context timeoutReached execution = do
  state <- execution.logState |> ConcurrentVar.peek
  case state of
    Pending ->
      EventLogContext.logResult context
        RunnerResult
          { runnerKind = execution.kind,
            runnerOrdinal = execution.ordinal,
            runnerOutcome = IntegrationOutcome.Failed NotStarted,
            commandsEmitted = 0
          }
    Processing commandCount -> do
      didTimeout <- timeoutReached |> ConcurrentVar.peek
      let failure = if didTimeout then TimedOut else Cancelled
      EventLogContext.logResult context
        RunnerResult
          { runnerKind = execution.kind,
            runnerOrdinal = execution.ordinal,
            runnerOutcome = IntegrationOutcome.Failed failure,
            commandsEmitted = commandCount
          }
    Completed outcome commandCount ->
      EventLogContext.logResult context
        RunnerResult
          { runnerKind = execution.kind,
            runnerOrdinal = execution.ordinal,
            runnerOutcome = outcome,
            commandsEmitted = commandCount
          }


run ::
  RunnerExecution runner ->
  Task Text (Maybe (Array Integration.CommandPayload)) ->
  (Integration.CommandPayload -> Task Text Unit) ->
  Task Text Unit
run execution runnerTask dispatchPayload = do
  let state = execution.logState
  let complete outcome = state |> ConcurrentVar.modify (RunnerLogState.complete outcome)
  state |> ConcurrentVar.modify RunnerLogState.start
  maybeCommandsResult <- runnerTask |> Task.asResultSafe
  case maybeCommandsResult of
    Err _ -> complete (IntegrationOutcome.Failed ProcessingFailure)
    Ok Nothing -> complete (IntegrationOutcome.Failed ProcessingFailure)
    Ok (Just commands) -> do
      let dispatchCommandPayload payload = do
            dispatchPayload payload
            state |> ConcurrentVar.modify RunnerLogState.incrementCommands
      let reportDispatchFailure failure = case failure of
            Task.Interrupted -> Task.yield unit
            Task.TypedFailure _ -> complete (IntegrationOutcome.Failed CallbackDispatchFailure)
            Task.RuntimeFailure -> complete (IntegrationOutcome.Failed CallbackDispatchFailure)
      let dispatchCommands =
            Task.evaluate (commands |> Task.forEach dispatchCommandPayload)
              |> Task.andThen identity
      dispatchCommands |> Task.onFailure reportDispatchFailure
      complete Processed
