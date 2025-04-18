module Lock (
  Lock,
  new,
  acquire,
  release,
  with,
) where

import Basics
import ConcurrentVar (ConcurrentVar)
import ConcurrentVar qualified
import Control.Exception qualified
import Task (Task)
import Task qualified


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


with :: Lock -> Task _ Unit -> Task _ Unit
with self task =
  do
    Control.Exception.bracket
      (acquire self |> Task.runNoErrors)
      (\_ -> release self |> Task.runNoErrors)
      (\_ -> task |> Task.runNoErrors)
    |> Task.fromIO
