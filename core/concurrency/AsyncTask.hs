module AsyncTask (
  AsyncTask,
  run,
  waitFor,
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
import Result (Result)
import Result qualified
import Task (Task)
import Task qualified
import ToText (Show)


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
