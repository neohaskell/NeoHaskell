module DateTime (
  DateTime,
  now,
) where

import Basics
import Data.Time.Clock qualified as Clock
import Json (FromJSON, ToJSON)
import Task (Task)
import Task qualified


newtype DateTime = DateTime Clock.UTCTime
  deriving (Eq, Ord, Show, Generic)


instance FromJSON DateTime


instance ToJSON DateTime


now :: Task _ DateTime
now = do
  posixTime <-
    Clock.getCurrentTime
      |> Task.fromIO
  posixTime
    |> DateTime
    |> Task.yield