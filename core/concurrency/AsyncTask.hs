{-# OPTIONS_GHC -Wno-unused-imports #-}

module AsyncTask (
  AsyncTask,
  Outcome (..),
  RaceWinner (..),
  run,
  runWithCancellationEnabled,
  waitFor,
  waitCatch,
  waitOutcome,
  isWaitingForStateChange,
  isWaitingForExceptionDelivery,
  requestThreadTermination,
  cancel,
  sleep,
  process,
  race,
  runConcurrently,
  runAllIgnoringErrors,
) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync
import Control.Exception (SomeAsyncException)
import Control.Exception qualified as GhcException
import GHC.Conc qualified as GhcConc
import GHC.IO qualified as GHC
import Maybe (Maybe (..))
import Prelude qualified
import Result (Result)
import Result qualified
import Text (Text)
import Text qualified
import Task (Task)
import Task qualified


newtype AsyncTask err result = AsyncTask (GhcAsync.Async (Result err result))


data Outcome err result
  = Completed result
  | Failed err
  | Interrupted
  | Crashed Text


run :: (Show err) => Task err result -> Task err (AsyncTask err result)
run task =
  Task.runResult task
    |> GhcAsync.async
    |> Task.fromIO
    |> Task.map AsyncTask


-- | Start a child that can receive cancellation while its parent protects an ownership handoff.
runWithCancellationEnabled :: (Show err) => Task err result -> Task err (AsyncTask err result)
runWithCancellationEnabled task =
  GhcAsync.asyncWithUnmask (\unmask -> unmask (Task.runResult task))
    |> Task.fromIO
    |> Task.map AsyncTask


waitFor :: (Show err) => AsyncTask err result -> Task err result
waitFor (AsyncTask self) =
  GhcAsync.wait self
    |> Task.fromIOResult


-- | Wait for an async task to complete without re-throwing its exceptions.
--
-- Unlike 'waitFor' which re-throws the task's exception into the calling
-- thread, 'waitCatch' returns the outcome as a 'Result'. This is essential
-- when waiting on tasks that were intentionally cancelled (e.g., during
-- shutdown), where re-throwing 'AsyncCancelled' would be incorrect.
waitCatch :: (Show err) => AsyncTask err result -> Task err2 (Result Text result)
waitCatch (AsyncTask self) = do
  outcome <- GhcAsync.waitCatch self |> Task.fromIO
  case Result.fromEither outcome of
    Result.Err someException ->
      Task.yield (Result.Err (show someException |> Text.fromLinkedList))
    Result.Ok taskResult ->
      case taskResult of
        Result.Ok value -> Task.yield (Result.Ok value)
        Result.Err err -> Task.yield (Result.Err (show err |> Text.fromLinkedList))


-- | Wait for a child and keep typed failures separate from cancellation and
-- runtime exceptions. A crash carries diagnostic text, not a typed task error.
waitOutcome :: AsyncTask err result -> Task otherErr (Outcome err result)
waitOutcome (AsyncTask self) = do
  outcome <- GhcAsync.waitCatch self |> Task.fromIO
  case Result.fromEither outcome of
    Result.Err someException ->
      case GhcException.fromException @SomeAsyncException someException of
        Just _ -> Task.yield Interrupted
        Nothing -> Task.yield (Crashed (show someException |> Text.fromLinkedList))
    Result.Ok (Result.Ok value) -> Task.yield (Completed value)
    Result.Ok (Result.Err err) -> Task.yield (Failed err)


-- | Observe whether a child is currently waiting for shared state to change.
-- This is a momentary status snapshot, useful for coordinating concurrency tests.
isWaitingForStateChange :: AsyncTask err result -> Task otherErr Bool
isWaitingForStateChange (AsyncTask child) = do
  status <- GhcConc.threadStatus (GhcAsync.asyncThreadId child) |> Task.fromIO
  case status of
    GhcConc.ThreadBlocked GhcConc.BlockedOnSTM -> Task.yield True
    _ -> Task.yield False


-- | Observe whether this task is blocked while delivering an exception.
-- This is a momentary status snapshot for coordinating cancellation tests.
isWaitingForExceptionDelivery :: AsyncTask err result -> Task otherErr Bool
isWaitingForExceptionDelivery (AsyncTask child) = do
  status <- GhcConc.threadStatus (GhcAsync.asyncThreadId child) |> Task.fromIO
  case status of
    GhcConc.ThreadBlocked GhcConc.BlockedOnException -> Task.yield True
    _ -> Task.yield False


-- | Send 'ThreadKilled' and return after delivery, without waiting for the
-- child to finish cleanup. Use 'waitOutcome' to observe its eventual exit.
requestThreadTermination :: AsyncTask err result -> Task otherErr Unit
requestThreadTermination (AsyncTask child) =
  Ghc.throwTo (GhcAsync.asyncThreadId child) GhcException.ThreadKilled
    |> Task.fromIO


-- | Cancel a running async task.
--
-- This forcefully terminates the task. Use with caution as it may leave
-- resources in an inconsistent state if the task doesn't handle cancellation.
cancel :: AsyncTask err result -> Task Text Unit
cancel (AsyncTask self) =
  GhcAsync.cancel self
    |> Task.fromIO


process :: forall err a b. (Show err) => Task err a -> (AsyncTask err a -> Task err b) -> Task err b
process task processor =
  do
    let internalProcessor :: GhcAsync.Async (Result err a) -> GHC.IO (Result err b)
        internalProcessor t = AsyncTask t |> processor |> Task.runResult
    internalProcessor
    |> GhcAsync.withAsync (Task.runResult task)
    |> Task.fromIOResult


-- | The winner of a 'race': 'LeftWon' means the left task finished first,
--   'RightWon' means the right task finished first.
data RaceWinner a b
  = LeftWon a
  | RightWon b
  deriving (Prelude.Show, Eq)


-- | Run two Tasks concurrently; return the result of whichever finishes first.
--   The loser is cancelled. Errors in the winner propagate; errors in the
--   loser are ignored. Mirrors the semantics of
--   'Control.Concurrent.Async.race' but stays inside the Task error channel.
race :: forall err a b. (Show err) => Task err a -> Task err b -> Task err (RaceWinner a b)
race leftTask rightTask = Task.fromIOResult do
  outcome <- GhcAsync.race (Task.runResult leftTask) (Task.runResult rightTask)
  case Result.fromEither outcome of
    Result.Err (Result.Ok value) -> pure (Result.Ok (LeftWon value))
    Result.Err (Result.Err error) -> pure (Result.Err error)
    Result.Ok (Result.Ok value) -> pure (Result.Ok (RightWon value))
    Result.Ok (Result.Err error) -> pure (Result.Err error)


sleep :: Int -> Task _ Unit
sleep milliseconds =
  (milliseconds * 1000)
    |> Ghc.threadDelay
    |> Task.fromIO


runConcurrently :: (Show err) => (Task err a, Task err b) -> Task err (a, b)
runConcurrently (task1, task2) = Task.fromIOResult do
  res <- GhcAsync.concurrently (Task.runResult task1) (Task.runResult task2)
  case res of
    (Result.Ok a, Result.Ok b) -> pure (Result.Ok (a, b))
    (Result.Err err, _) -> pure (Result.Err err)
    (_, Result.Err err) -> pure (Result.Err err)


-- | Execute multiple tasks in parallel, like Promise.all() in JavaScript/TypeScript.
--
-- This function takes an array of tasks and runs them all at the same time (concurrently),
-- rather than waiting for each one to finish before starting the next (sequentially).
--
-- Think of it like this:
-- - Sequential (one after another): Total time = task1 time + task2 time + task3 time
-- - Concurrent (all at once): Total time ≈ longest task time
--
-- Note: If any task fails, this function continues executing the other tasks and ignores
-- the failure. This is useful for scenarios like dispatching events to multiple listeners
-- where one listener failing shouldn't prevent others from receiving the event.
runAllIgnoringErrors :: (Show err) => Array (Task err a) -> Task _ Unit
runAllIgnoringErrors tasks = do
  let executeTask task = do
        Task.runResult task
  tasks
    |> Array.toLinkedList
    |> GhcAsync.mapConcurrently_ executeTask
    |> Task.fromIO
