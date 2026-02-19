module Integration.Oura.Types
  ( PaginatedResponse (..)
  , SleepData (..)
  , SleepContributors (..)
  , ActivityData (..)
  , ReadinessData (..)
  , ReadinessContributors (..)
  , HeartRateData (..)
  , SampleModel (..)
  , ActivityContributors (..)
  , ResilienceContributors (..)
  , SpO2AggregatedValues (..)
  , DailyStressSummary (..)
  , RestModeEpisode (..)
  , SleepPeriodReadiness (..)
  )
where

import Basics (Float, Int, Show, Eq)
import Json qualified
import Maybe (Maybe)
import Text (Text)
import GHC.Generics (Generic)


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

instance Json.ToJSON SleepContributors where
  toJSON contributors =
    Json.object
      [ "deep_sleep" Json..= contributors.deepSleep
      , "efficiency" Json..= contributors.efficiency
      , "latency" Json..= contributors.latency
      , "rem_sleep" Json..= contributors.remSleep
      , "restfulness" Json..= contributors.restfulness
      , "timing" Json..= contributors.timing
      , "total_sleep" Json..= contributors.totalSleep
      ]


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

instance Json.ToJSON SleepData where
  toJSON sleepData =
    Json.object
      [ "id" Json..= sleepData.id
      , "day" Json..= sleepData.day
      , "score" Json..= sleepData.score
      , "timestamp" Json..= sleepData.timestamp
      , "contributors" Json..= sleepData.contributors
      ]


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
  , averageMetMinutes :: Maybe Float
  , class5Min :: Maybe Text
  , contributors :: Maybe ActivityContributors
  , equivalentWalkingDistance :: Maybe Int
  , highActivityMetMinutes :: Maybe Int
  , inactivityAlerts :: Maybe Int
  , lowActivityMetMinutes :: Maybe Int
  , mediumActivityMetMinutes :: Maybe Int
  , met :: Maybe SampleModel
  , metersToTarget :: Maybe Int
  , nonWearTime :: Maybe Int
  , restingTime :: Maybe Int
  , sedentaryMetMinutes :: Maybe Int
  , sedentaryTime :: Maybe Int
  , targetCalories :: Maybe Int
  , targetMeters :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ActivityData where
  toJSON activityData =
    Json.object
      [ "id" Json..= activityData.id
      , "day" Json..= activityData.day
      , "score" Json..= activityData.score
      , "active_calories" Json..= activityData.activeCalories
      , "steps" Json..= activityData.steps
      , "total_calories" Json..= activityData.totalCalories
      , "high_activity_time" Json..= activityData.highActivityTime
      , "medium_activity_time" Json..= activityData.mediumActivityTime
      , "low_activity_time" Json..= activityData.lowActivityTime
      , "timestamp" Json..= activityData.timestamp
      , "average_met_minutes" Json..= activityData.averageMetMinutes
      , "class_5_min" Json..= activityData.class5Min
      , "contributors" Json..= activityData.contributors
      , "equivalent_walking_distance" Json..= activityData.equivalentWalkingDistance
      , "high_activity_met_minutes" Json..= activityData.highActivityMetMinutes
      , "inactivity_alerts" Json..= activityData.inactivityAlerts
      , "low_activity_met_minutes" Json..= activityData.lowActivityMetMinutes
      , "medium_activity_met_minutes" Json..= activityData.mediumActivityMetMinutes
      , "met" Json..= activityData.met
      , "meters_to_target" Json..= activityData.metersToTarget
      , "non_wear_time" Json..= activityData.nonWearTime
      , "resting_time" Json..= activityData.restingTime
      , "sedentary_met_minutes" Json..= activityData.sedentaryMetMinutes
      , "sedentary_time" Json..= activityData.sedentaryTime
      , "target_calories" Json..= activityData.targetCalories
      , "target_meters" Json..= activityData.targetMeters
      ]


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
    averageMetMinutes <- obj Json..:? "average_met_minutes"
    class5Min <- obj Json..:? "class_5_min"
    contributors <- obj Json..:? "contributors"
    equivalentWalkingDistance <- obj Json..:? "equivalent_walking_distance"
    highActivityMetMinutes <- obj Json..:? "high_activity_met_minutes"
    inactivityAlerts <- obj Json..:? "inactivity_alerts"
    lowActivityMetMinutes <- obj Json..:? "low_activity_met_minutes"
    mediumActivityMetMinutes <- obj Json..:? "medium_activity_met_minutes"
    met <- obj Json..:? "met"
    metersToTarget <- obj Json..:? "meters_to_target"
    nonWearTime <- obj Json..:? "non_wear_time"
    restingTime <- obj Json..:? "resting_time"
    sedentaryMetMinutes <- obj Json..:? "sedentary_met_minutes"
    sedentaryTime <- obj Json..:? "sedentary_time"
    targetCalories <- obj Json..:? "target_calories"
    targetMeters <- obj Json..:? "target_meters"
    Json.yield (ActivityData id day score activeCalories steps totalCalories highActivityTime mediumActivityTime lowActivityTime timestamp averageMetMinutes class5Min contributors equivalentWalkingDistance highActivityMetMinutes inactivityAlerts lowActivityMetMinutes mediumActivityMetMinutes met metersToTarget nonWearTime restingTime sedentaryMetMinutes sedentaryTime targetCalories targetMeters)


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
  , sleepRegularity :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ReadinessContributors where
  toJSON contributors =
    Json.object
      [ "activity_balance" Json..= contributors.activityBalance
      , "body_temperature" Json..= contributors.bodyTemperature
      , "hrv_balance" Json..= contributors.hrvBalance
      , "previous_day_activity" Json..= contributors.previousDayActivity
      , "previous_night" Json..= contributors.previousNight
      , "recovery_index" Json..= contributors.recoveryIndex
      , "resting_heart_rate" Json..= contributors.restingHeartRate
      , "sleep_balance" Json..= contributors.sleepBalance
      , "sleep_regularity" Json..= contributors.sleepRegularity
      ]


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
    sleepRegularity <- obj Json..:? "sleep_regularity"
    Json.yield (ReadinessContributors activityBalance bodyTemperature hrvBalance previousDayActivity previousNight recoveryIndex restingHeartRate sleepBalance sleepRegularity)


-- | Daily readiness data from Oura Ring
data ReadinessData = ReadinessData
  { id :: Text
  , day :: Text
  , score :: Maybe Int
  , temperatureDeviation :: Maybe Float
  , temperatureTrendDeviation :: Maybe Float
  , timestamp :: Text
  , contributors :: Maybe ReadinessContributors
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ReadinessData where
  toJSON readinessData =
    Json.object
      [ "id" Json..= readinessData.id
      , "day" Json..= readinessData.day
      , "score" Json..= readinessData.score
      , "temperature_deviation" Json..= readinessData.temperatureDeviation
      , "temperature_trend_deviation" Json..= readinessData.temperatureTrendDeviation
      , "timestamp" Json..= readinessData.timestamp
      , "contributors" Json..= readinessData.contributors
      ]


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

instance Json.ToJSON HeartRateData where
  toJSON heartRateData =
    Json.object
      [ "bpm" Json..= heartRateData.bpm
      , "source" Json..= heartRateData.source
      , "timestamp" Json..= heartRateData.timestamp
      ]


instance Json.FromJSON HeartRateData where
  parseJSON = Json.withObject "HeartRateData" \obj -> do
    bpm <- obj Json..: "bpm"
    source <- obj Json..: "source"
    timestamp <- obj Json..: "timestamp"
    Json.yield (HeartRateData bpm source timestamp)


-- | Time-series data with sampling interval and items
data SampleModel = SampleModel
  { interval :: Maybe Float
  , items :: Maybe [Maybe Float]
  , timestamp :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SampleModel where
  toJSON sampleModel =
    Json.object
      [ "interval" Json..= sampleModel.interval
      , "items" Json..= sampleModel.items
      , "timestamp" Json..= sampleModel.timestamp
      ]

instance Json.FromJSON SampleModel where
  parseJSON = Json.withObject "SampleModel" \obj -> do
    interval <- obj Json..:? "interval"
    items <- obj Json..:? "items"
    timestamp <- obj Json..:? "timestamp"
    Json.yield (SampleModel interval items timestamp)


-- | Contributors to activity score
data ActivityContributors = ActivityContributors
  { meetDailyTargets :: Maybe Int
  , moveEveryHour :: Maybe Int
  , recoveryTime :: Maybe Int
  , stayActive :: Maybe Int
  , trainingFrequency :: Maybe Int
  , trainingVolume :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ActivityContributors where
  toJSON contributors =
    Json.object
      [ "meet_daily_targets" Json..= contributors.meetDailyTargets
      , "move_every_hour" Json..= contributors.moveEveryHour
      , "recovery_time" Json..= contributors.recoveryTime
      , "stay_active" Json..= contributors.stayActive
      , "training_frequency" Json..= contributors.trainingFrequency
      , "training_volume" Json..= contributors.trainingVolume
      ]

instance Json.FromJSON ActivityContributors where
  parseJSON = Json.withObject "ActivityContributors" \obj -> do
    meetDailyTargets <- obj Json..:? "meet_daily_targets"
    moveEveryHour <- obj Json..:? "move_every_hour"
    recoveryTime <- obj Json..:? "recovery_time"
    stayActive <- obj Json..:? "stay_active"
    trainingFrequency <- obj Json..:? "training_frequency"
    trainingVolume <- obj Json..:? "training_volume"
    Json.yield (ActivityContributors meetDailyTargets moveEveryHour recoveryTime stayActive trainingFrequency trainingVolume)


-- | Contributors to resilience score
data ResilienceContributors = ResilienceContributors
  { sleepRecovery :: Maybe Float
  , daytimeRecovery :: Maybe Float
  , stress :: Maybe Float
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON ResilienceContributors where
  toJSON contributors =
    Json.object
      [ "sleep_recovery" Json..= contributors.sleepRecovery
      , "daytime_recovery" Json..= contributors.daytimeRecovery
      , "stress" Json..= contributors.stress
      ]

instance Json.FromJSON ResilienceContributors where
  parseJSON = Json.withObject "ResilienceContributors" \obj -> do
    sleepRecovery <- obj Json..:? "sleep_recovery"
    daytimeRecovery <- obj Json..:? "daytime_recovery"
    stress <- obj Json..:? "stress"
    Json.yield (ResilienceContributors sleepRecovery daytimeRecovery stress)


-- | Aggregated SpO2 values
data SpO2AggregatedValues = SpO2AggregatedValues
  { average :: Float
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SpO2AggregatedValues where
  toJSON values =
    Json.object
      [ "average" Json..= values.average
      ]

instance Json.FromJSON SpO2AggregatedValues where
  parseJSON = Json.withObject "SpO2AggregatedValues" \obj -> do
    average <- obj Json..: "average"
    Json.yield (SpO2AggregatedValues average)


-- | Daily stress summary
data DailyStressSummary = DailyStressSummary
  { averageStress :: Maybe Float
  , maxStress :: Maybe Float
  , stressHigh :: Maybe Float
  , recoveryHigh :: Maybe Float
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON DailyStressSummary where
  toJSON summary =
    Json.object
      [ "average_stress" Json..= summary.averageStress
      , "max_stress" Json..= summary.maxStress
      , "stress_high" Json..= summary.stressHigh
      , "recovery_high" Json..= summary.recoveryHigh
      ]

instance Json.FromJSON DailyStressSummary where
  parseJSON = Json.withObject "DailyStressSummary" \obj -> do
    averageStress <- obj Json..:? "average_stress"
    maxStress <- obj Json..:? "max_stress"
    stressHigh <- obj Json..:? "stress_high"
    recoveryHigh <- obj Json..:? "recovery_high"
    Json.yield (DailyStressSummary averageStress maxStress stressHigh recoveryHigh)


-- | Rest mode episode
data RestModeEpisode = RestModeEpisode
  { tags :: Maybe [Text]
  , timestamp :: Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON RestModeEpisode where
  toJSON episode =
    Json.object
      [ "tags" Json..= episode.tags
      , "timestamp" Json..= episode.timestamp
      ]

instance Json.FromJSON RestModeEpisode where
  parseJSON = Json.withObject "RestModeEpisode" \obj -> do
    tags <- obj Json..:? "tags"
    timestamp <- obj Json..: "timestamp"
    Json.yield (RestModeEpisode tags timestamp)


-- | Sleep period readiness data
data SleepPeriodReadiness = SleepPeriodReadiness
  { contributors :: Maybe ReadinessContributors
  , score :: Maybe Int
  , temperatureDeviation :: Maybe Float
  , temperatureTrendDeviation :: Maybe Float
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SleepPeriodReadiness where
  toJSON readiness =
    Json.object
      [ "contributors" Json..= readiness.contributors
      , "score" Json..= readiness.score
      , "temperature_deviation" Json..= readiness.temperatureDeviation
      , "temperature_trend_deviation" Json..= readiness.temperatureTrendDeviation
      ]

instance Json.FromJSON SleepPeriodReadiness where
  parseJSON = Json.withObject "SleepPeriodReadiness" \obj -> do
    contributors <- obj Json..:? "contributors"
    score <- obj Json..:? "score"
    temperatureDeviation <- obj Json..:? "temperature_deviation"
    temperatureTrendDeviation <- obj Json..:? "temperature_trend_deviation"
    Json.yield (SleepPeriodReadiness contributors score temperatureDeviation temperatureTrendDeviation)
