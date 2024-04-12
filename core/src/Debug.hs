module Debug (
  unsafelyPanicAndDie,
  todo,
  report,
) where

import Debug.Panic (unsafelyPanicAndDie)
import Debug.ToDo (todo)
import Debug.Trace (report)
