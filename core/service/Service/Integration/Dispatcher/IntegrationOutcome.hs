{-# LANGUAGE Strict #-}

-- | Owns the closed result of one integration runner and its failure labels.
module Service.Integration.Dispatcher.IntegrationOutcome (
  IntegrationOutcome (..),
  RunnerFailure (..),
  resultLabel,
  failureFields,
  message,
) where

import Array (Array)
import Array qualified
import Json qualified
import Text (Text)


data IntegrationOutcome
  = Processed
  | Failed RunnerFailure


data RunnerFailure
  = ProcessingFailure
  | CallbackDispatchFailure
  | TimedOut
  | Cancelled
  | NotStarted


resultLabel :: IntegrationOutcome -> Text
resultLabel outcome = case outcome of
  Processed -> "processed"
  Failed failure -> case failure of
    ProcessingFailure -> "failed"
    CallbackDispatchFailure -> "failed"
    TimedOut -> "timed_out"
    Cancelled -> "cancelled"
    NotStarted -> "cancelled"


failureFields :: IntegrationOutcome -> Array (Text, Json.Value)
failureFields outcome = case outcome of
  Processed -> Array.empty
  Failed failure ->
    Array.fromLinkedList
      [ ("failureStage" Json..= failureStage failure),
        ("errorClass" Json..= errorClass failure)
      ]


message :: IntegrationOutcome -> Text
message outcome = case outcome of
  Processed -> "Integration runner processed event"
  Failed failure -> case failure of
    ProcessingFailure -> "Integration runner failed to process event"
    CallbackDispatchFailure -> "Integration runner callback dispatch failed"
    TimedOut -> "Integration runner timed out"
    Cancelled -> "Integration runner was cancelled"
    NotStarted -> "Integration runner was not started"


failureStage :: RunnerFailure -> Text
failureStage failure = case failure of
  ProcessingFailure -> "runner_processing"
  CallbackDispatchFailure -> "callback_dispatch"
  TimedOut -> "timeout"
  Cancelled -> "cancellation"
  NotStarted -> "cancellation"


errorClass :: RunnerFailure -> Text
errorClass failure = case failure of
  ProcessingFailure -> "RunnerProcessingFailure"
  CallbackDispatchFailure -> "CallbackDispatchFailure"
  TimedOut -> "Timeout"
  Cancelled -> "ExecutionInterrupted"
  NotStarted -> "RunnerNotStarted"
