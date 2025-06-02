module AsyncTask (AsyncTask, run, waitFor, sleep, process, waitAnyCancel, withRecovery, cancel, runConcurrentlyAndDiscard) where

import Array (Array)
import Array qualified
import Basics
import Control.Concurrent qualified as Ghc
import Control.Concurrent.Async qualified as GhcAsync
import Data.Either qualified as Either
import Result (Result)
import Result qualified
import Task (Task)
import Task qualified
import ToText (Show)


type AsyncTask result = GhcAsync.Async result


run :: (Show err) => Task err result -> Task err (AsyncTask result)
run task =
  Task.runOrPanic task
    |> GhcAsync.async
    |> Task.fromIO


waitFor :: (Show err) => AsyncTask result -> Task err result
waitFor self =
  GhcAsync.wait self
    |> Task.fromIO


process :: (Show err) => Task err a -> (AsyncTask a -> Task err b) -> Task err b
process task process =
  (\asyncTask -> process asyncTask |> Task.runOrPanic)
    |> GhcAsync.withAsync (Task.runOrPanic task)
    |> Task.fromIO


sleep :: Int -> Task Never Unit
sleep milliseconds =
  (milliseconds * 1000)
    |> Ghc.threadDelay
    |> Task.fromIO


withRecovery :: (Show err) => Task err error -> Task err result -> Task err (Result error result)
withRecovery errorTask resultTask = do
  let errorIO = Task.runOrPanic errorTask
  let resultIO = Task.runOrPanic resultTask
  result <- GhcAsync.race errorIO resultIO |> Task.fromIO
  case result of
    Either.Left a -> pure (Result.Err a)
    Either.Right a -> pure (Result.Ok a)


waitAnyCancel :: (Show err) => Array (AsyncTask a) -> Task err (AsyncTask a, a)
waitAnyCancel arr = do
  let asyncList =
        Array.toLinkedList arr
  (async, result) <- GhcAsync.waitAnyCancel asyncList |> Task.fromIO
  pure (async, result)


cancel :: (Show err) => AsyncTask a -> Task err Unit
cancel asyncTask =
  asyncTask
    |> GhcAsync.cancel
    |> Task.fromIO


runConcurrentlyAndDiscard :: (Show err) => (Task err a, Task err b) -> Task err Unit
runConcurrentlyAndDiscard (async1, async2) =
  GhcAsync.concurrently_ (Task.runOrPanic async1) (Task.runOrPanic async2)
    |> Task.fromIO
