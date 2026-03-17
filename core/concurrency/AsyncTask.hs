{-# OPTIONS_GHC -Wno-unused-imports #-}

module AsyncTask (
  AsyncTask,
  run,
  waitFor,
  waitCatch,
  cancel,
  sleep,
  process,
  runConcurrently,
  runAllIgnoringErrors,
) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync
import GHC.IO qualified as GHC
import Data.Either qualified as GhcEither
import Result (Result)
import Result qualified
import Text (Text)
import Text qualified
import Task (Task)
import Task qualified


newtype AsyncTask err result = AsyncTask (GhcAsync.Async (Result err result))


run :: (Show err) => Task err result -> Task err (AsyncTask err result)
run task =
  Task.runResult task
    |> GhcAsync.async
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
  case outcome of
    GhcEither.Left someException ->
      Task.yield (Result.Err (show someException |> Text.fromLinkedList))
    GhcEither.Right taskResult ->
      case taskResult of
        Result.Ok value -> Task.yield (Result.Ok value)
        Result.Err err -> Task.yield (Result.Err (show err |> Text.fromLinkedList))


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
