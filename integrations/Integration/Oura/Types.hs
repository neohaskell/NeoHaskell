module Integration.Oura.Types
  ( PaginatedResponse (..)
  , SleepData (..)
  , SleepContributors (..)
  , ActivityData (..)
  , ReadinessData (..)
  , ReadinessContributors (..)
  , HeartRateData (..)
  )
where

import Basics
import Json qualified
import Maybe (Maybe)
import Text (Text)
import GHC.Generics ()


-- | Wrapper for paginated API responses
data PaginatedResponse value = PaginatedResponse
  { data_ :: [value]
  , nextToken :: Maybe Text
  }
  deriving (Show, Eq)


instance (Json.FromJSON value) => Json.FromJSON (PaginatedResponse value) where
  parseJSON = Json.withObject "PaginatedResponse" \obj -> do
    data_ <- obj Json..: "data"
    nextToken <- obj Json..:? "next_token"
    Json.yield (PaginatedResponse data_ nextToken)


-- | Contributors to sleep score
data SleepContributors = SleepContributors
  { deepSleep :: Maybe Int
  , efficiency :: Maybe Int
  , latency :: Maybe Int
  , remSleep :: Maybe Int
  , restfulness :: Maybe Int
  , timing :: Maybe Int
  , totalSleep :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SleepContributors


instance Json.FromJSON SleepContributors where
  parseJSON = Json.withObject "SleepContributors" \obj -> do
    deepSleep <- obj Json..:? "deep_sleep"
    efficiency <- obj Json..:? "efficiency"
    latency <- obj Json..:? "latency"
    remSleep <- obj Json..:? "rem_sleep"
    restfulness <- obj Json..:? "restfulness"
    timing <- obj Json..:? "timing"
    totalSleep <- obj Json..:? "total_sleep"
    Json.yield (SleepContributors deepSleep efficiency latency remSleep restfulness timing totalSleep)


-- | Daily sleep data from Oura Ring
data SleepData = SleepData
  { id :: Text
  , day :: Text
  , score :: Maybe Int
  , timestamp :: Text
  , contributors :: Maybe SleepContributors
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SleepData


instance Json.FromJSON SleepData where
  parseJSON = Json.withObject "SleepData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    score <- obj Json..:? "score"
    timestamp <- obj Json..: "timestamp"
    contributors <- obj Json..:? "contributors"
    Json.yield (SleepData id day score timestamp contributors)


-- | Daily activity data from Oura Ring
data ActivityData = ActivityData
  { id :: Text
  , day :: Text
  , score :: Maybe Int
  , activeCalories :: Maybe Int
  , steps :: Maybe Int
  , totalCalories :: Maybe Int
  , highActivityTime :: Maybe Int
  , mediumActivityTime :: Maybe Int
  , lowActivityTime :: Maybe Int
  , timestamp :: Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ActivityData


instance Json.FromJSON ActivityData where
  parseJSON = Json.withObject "ActivityData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    score <- obj Json..:? "score"
    activeCalories <- obj Json..:? "active_calories"
    steps <- obj Json..:? "steps"
    totalCalories <- obj Json..:? "total_calories"
    highActivityTime <- obj Json..:? "high_activity_time"
    mediumActivityTime <- obj Json..:? "medium_activity_time"
    lowActivityTime <- obj Json..:? "low_activity_time"
    timestamp <- obj Json..: "timestamp"
    Json.yield (ActivityData id day score activeCalories steps totalCalories highActivityTime mediumActivityTime lowActivityTime timestamp)


-- | Contributors to readiness score
data ReadinessContributors = ReadinessContributors
  { activityBalance :: Maybe Int
  , bodyTemperature :: Maybe Int
  , hrvBalance :: Maybe Int
  , previousDayActivity :: Maybe Int
  , previousNight :: Maybe Int
  , recoveryIndex :: Maybe Int
  , restingHeartRate :: Maybe Int
  , sleepBalance :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ReadinessContributors


instance Json.FromJSON ReadinessContributors where
  parseJSON = Json.withObject "ReadinessContributors" \obj -> do
    activityBalance <- obj Json..:? "activity_balance"
    bodyTemperature <- obj Json..:? "body_temperature"
    hrvBalance <- obj Json..:? "hrv_balance"
    previousDayActivity <- obj Json..:? "previous_day_activity"
    previousNight <- obj Json..:? "previous_night"
    recoveryIndex <- obj Json..:? "recovery_index"
    restingHeartRate <- obj Json..:? "resting_heart_rate"
    sleepBalance <- obj Json..:? "sleep_balance"
    Json.yield (ReadinessContributors activityBalance bodyTemperature hrvBalance previousDayActivity previousNight recoveryIndex restingHeartRate sleepBalance)


-- | Daily readiness data from Oura Ring
data ReadinessData = ReadinessData
  { id :: Text
  , day :: Text
  , score :: Maybe Int
  , temperatureDeviation :: Maybe Int
  , temperatureTrendDeviation :: Maybe Int
  , timestamp :: Text
  , contributors :: Maybe ReadinessContributors
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ReadinessData


instance Json.FromJSON ReadinessData where
  parseJSON = Json.withObject "ReadinessData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    score <- obj Json..:? "score"
    temperatureDeviation <- obj Json..:? "temperature_deviation"
    temperatureTrendDeviation <- obj Json..:? "temperature_trend_deviation"
    timestamp <- obj Json..: "timestamp"
    contributors <- obj Json..:? "contributors"
    Json.yield (ReadinessData id day score temperatureDeviation temperatureTrendDeviation timestamp contributors)


-- | Heart rate data from Oura Ring
data HeartRateData = HeartRateData
  { bpm :: Int
  , source :: Text
  , timestamp :: Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON HeartRateData


instance Json.FromJSON HeartRateData where
  parseJSON = Json.withObject "HeartRateData" \obj -> do
    bpm <- obj Json..: "bpm"
    source <- obj Json..: "source"
    timestamp <- obj Json..: "timestamp"
    Json.yield (HeartRateData bpm source timestamp)
