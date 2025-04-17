module Lock (Lock, new, read, write) where

import Array (Array)
import Array qualified
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
  ConcurrentVar.set unit
