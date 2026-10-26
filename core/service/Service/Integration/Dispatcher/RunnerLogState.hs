
-- | Tracks one runner's progress so interruption emits its last known outcome.
--
-- Runner execution advances this state from 'initial' through 'complete'.
module Service.Integration.Dispatcher.RunnerLogState (
  RunnerLogState (..),
  initial,
  start,
  complete,
  incrementCommands,
) where

import Basics
import Service.Integration.Dispatcher.IntegrationOutcome (IntegrationOutcome)


data RunnerLogState
  = Pending
  | Processing Int
  | Completed IntegrationOutcome Int


initial :: RunnerLogState
initial = Pending


start :: RunnerLogState -> RunnerLogState
start state = case state of
  Pending -> Processing 0
  existing -> existing


complete :: IntegrationOutcome -> RunnerLogState -> RunnerLogState
complete outcome state = case state of
  Pending -> Completed outcome 0
  Processing commandCount -> Completed outcome commandCount
  Completed _ commandCount -> Completed outcome commandCount


incrementCommands :: RunnerLogState -> RunnerLogState
incrementCommands state = case state of
  Processing commandCount -> Processing (commandCount + 1)
  completed -> completed
