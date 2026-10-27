module Lock (
  Lock,
  new,
  acquire,
  release,
  with,
  withTimeout,
) where

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Task (Task)
import Task qualified
import Maybe (Maybe (..))
import Result (Result (..))
import Text (Text)
import System.Timeout qualified as GhcTimeout


newtype Lock = Lock
  { lock :: ConcurrentVar Unit
  }


new :: Task _ Lock
new = do
  lock <- ConcurrentVar.containing unit
  Task.yield Lock {lock}


acquire :: Lock -> Task _ Unit
acquire self =
  ConcurrentVar.get self.lock


release :: Lock -> Task _ Unit
release self = do
  ConcurrentVar.set unit self.lock


with :: Lock -> Task _ value -> Task _ value
with self task = Task.withCancellationDeferred \restore -> do
  acquire self
  Task.finally (release self) (restore task)


-- | Execute a task while holding the lock, with a timeout in seconds.
-- Returns 'Err "lock timeout"' if the lock cannot be acquired within the timeout.
-- Returns 'Ok result' if the task completes successfully.
--
-- SECURITY: Use this instead of 'with' for refresh operations to prevent deadlock.
withTimeout :: forall value. Int -> Lock -> Task _ value -> Task _ (Result Text value)
withTimeout timeoutSeconds self task = do
  if timeoutSeconds <= 0
    then Task.yield (Err "lock timeout: non-positive timeout")
    else Task.withCancellationDeferred \restore -> do
      let microseconds = timeoutSeconds * 1000000
      maybeUnit <-
        GhcTimeout.timeout microseconds (acquire self |> Task.runNoErrors)
          |> Task.fromIO
          |> restore
      case maybeUnit of
        Nothing -> Task.yield (Err "lock timeout")
        Just _ -> do
          taskResult <-
            Task.finally (release self) (restore (Task.asResult task))
          case taskResult of
            Ok value -> Task.yield (Ok value)
            Err err -> Task.throw err
