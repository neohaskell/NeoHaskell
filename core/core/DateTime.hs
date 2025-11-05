module DateTime (
  DateTime,
  now,
) where

import Basics
import Nanotime qualified
import Task (Task)
import Task qualified
import ToText (Show)


-- | A @DateTime@ represents a point in time using POSIX time (seconds since
-- the Unix epoch, 1970-01-01 00:00:00 UTC).
newtype DateTime = DateTime Nanotime.PosixTime
  deriving (Eq, Ord, Show)


now :: Task _ DateTime
now = do
  posixTime <-
    Nanotime.currentTime @Nanotime.PosixTime
      |> Task.fromIO
  posixTime
    |> DateTime
    |> Task.yield