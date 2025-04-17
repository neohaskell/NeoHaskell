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
with self task = do
  -- FIXME: use resourceT or try/catch/finally
  acquire self
  task -- will deadlock on panic
  release self
