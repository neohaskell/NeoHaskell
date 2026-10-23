{-# LANGUAGE Strict #-}

-- | Owns admission and draining for one integration worker queue.
module Service.Integration.Dispatcher.WorkerQueueControl (
  WorkerQueueControl,
  new,
  isAccepting,
  withAdmission,
  markDraining,
  shouldReplace,
  claimStop,
  waitForWriters,
  finishStop,
) where

import AtomicVar (AtomicVar)
import AtomicVar qualified
import Basics
import Maybe (Maybe (..))
import Task (Task)
import Task qualified


data QueueStatus
  = Accepting Int
  | Draining Int
  | Stopping Int
  | Stopped


newtype WorkerQueueControl = WorkerQueueControl (AtomicVar QueueStatus)


new :: Task err WorkerQueueControl
new = AtomicVar.containing (Accepting 0) |> Task.map WorkerQueueControl


isAccepting :: WorkerQueueControl -> Task err Bool
isAccepting (WorkerQueueControl status) = do
  current <- AtomicVar.peek status
  case current of
    Accepting _ -> Task.yield True
    _ -> Task.yield False


withAdmission :: WorkerQueueControl -> Task err result -> Task err (Maybe result)
withAdmission (WorkerQueueControl status) write = Task.mask \restore -> do
  admitted <- AtomicVar.modifyWithResult status \current -> case current of
    Accepting writers -> (Accepting (writers + 1), True)
    _ -> (current, False)
  if admitted
    then do
      result <- Task.finally (release status) (restore write)
      Task.yield (Just result)
    else Task.yield Nothing


release :: AtomicVar QueueStatus -> Task err Unit
release status = do
  let decrement current = case current of
        Accepting writers -> Accepting (writers - 1)
        Draining writers -> Draining (writers - 1)
        Stopping writers -> Stopping (writers - 1)
        Stopped -> Stopped
  AtomicVar.modify decrement status


markDraining :: WorkerQueueControl -> AtomicVar.Transaction Bool
markDraining (WorkerQueueControl status) = do
  let transition current = case current of
        Accepting writers -> (Draining writers, True)
        _ -> (current, False)
  current <- AtomicVar.peekSTM status
  case current of
    Accepting _ -> AtomicVar.modifyWithResultSTM transition status
    _ -> pure False


shouldReplace :: WorkerQueueControl -> AtomicVar.Transaction Bool
shouldReplace (WorkerQueueControl status) = do
  current <- AtomicVar.peekSTM status
  case current of
    Accepting _ -> pure False
    _ -> pure True


claimStop :: WorkerQueueControl -> Task err Bool
claimStop (WorkerQueueControl status) =
  AtomicVar.modifyWithResult status \current -> case current of
    Accepting writers -> (Stopping writers, True)
    Draining writers -> (Stopping writers, True)
    _ -> (current, False)


waitForWriters :: WorkerQueueControl -> Task err Unit
waitForWriters (WorkerQueueControl status) = do
  let ready current = case current of
        Stopping writers -> writers == 0
        _ -> True
  AtomicVar.waitUntil ready status


finishStop :: WorkerQueueControl -> Task err Unit
finishStop (WorkerQueueControl status) = do
  let finish current = case current of
        Stopping 0 -> Stopped
        _ -> current
  AtomicVar.modify finish status
