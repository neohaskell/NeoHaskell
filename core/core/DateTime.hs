module DateTime (
  DateTime,
  now,
  addSeconds,
  toEpochSeconds,
  fromEpochSeconds,
) where

import Basics
import Data.Time.Clock qualified as Clock
import Data.Time.Clock.POSIX qualified as Posix
import GHC.Real qualified as GhcReal
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


-- | Add seconds to a DateTime
addSeconds :: Int64 -> DateTime -> DateTime
addSeconds seconds (DateTime utc) = do
  let nominalSeconds = fromIntegral seconds :: Clock.NominalDiffTime
  DateTime (Clock.addUTCTime nominalSeconds utc)


-- | Convert DateTime to Unix epoch seconds
toEpochSeconds :: DateTime -> Int64
toEpochSeconds (DateTime utc) = do
  let posixTime = Posix.utcTimeToPOSIXSeconds utc
  GhcReal.truncate posixTime


-- | Create DateTime from Unix epoch seconds
fromEpochSeconds :: Int64 -> DateTime
fromEpochSeconds seconds = do
  let posixTime = fromIntegral seconds :: Posix.POSIXTime
  DateTime (Posix.posixSecondsToUTCTime posixTime)