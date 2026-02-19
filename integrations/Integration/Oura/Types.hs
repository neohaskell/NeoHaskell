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
  , RestModeEpisode (..)
  , SleepPeriodReadiness (..)
  , SleepPeriodData (..)
  , WorkoutData (..)
  , SessionData (..)
  , PersonalInfoData (..)
  , DailyStressData (..)
  , DailySpO2Data (..)
  , DailyResilienceData (..)
  , DailyCardiovascularAgeData (..)
  , VO2MaxData (..)
  , EnhancedTagData (..)
  , SleepTimeData (..)
  , RestModePeriodData (..)
  , RingConfigurationData (..)
  )
where

import Basics (Bool, Float, Int, Show, Eq)
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


-- | Detailed sleep period data from Oura Ring
data SleepPeriodData = SleepPeriodData
  { id :: Text
  , day :: Text
  , timestamp :: Text
  , averageBreath :: Maybe Float
  , averageHeartRate :: Maybe Float
  , averageHrv :: Maybe Float
  , awakeTime :: Maybe Int
  , bedtimeEnd :: Maybe Text
  , bedtimeStart :: Maybe Text
  , deepSleepDuration :: Maybe Int
  , efficiency :: Maybe Int
  , heartRate :: Maybe SampleModel
  , hrv :: Maybe SampleModel
  , latency :: Maybe Int
  , lightSleepDuration :: Maybe Int
  , lowBatteryAlert :: Maybe Bool
  , lowestHeartRate :: Maybe Int
  , movementThirtySec :: Maybe Text
  , period :: Maybe Int
  , readiness :: Maybe SleepPeriodReadiness
  , readinessScoreDelta :: Maybe Float
  , remSleepDuration :: Maybe Int
  , restlessPeriods :: Maybe Int
  , sleepAlgorithmVersion :: Maybe Text
  , sleepPhase5Min :: Maybe Text
  , sleepScoreDelta :: Maybe Float
  , timeInBed :: Maybe Int
  , totalSleepDuration :: Maybe Int
  , type_ :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SleepPeriodData where
  toJSON sleepPeriod =
    Json.object
      [ "id" Json..= sleepPeriod.id
      , "day" Json..= sleepPeriod.day
      , "timestamp" Json..= sleepPeriod.timestamp
      , "average_breath" Json..= sleepPeriod.averageBreath
      , "average_heart_rate" Json..= sleepPeriod.averageHeartRate
      , "average_hrv" Json..= sleepPeriod.averageHrv
      , "awake_time" Json..= sleepPeriod.awakeTime
      , "bedtime_end" Json..= sleepPeriod.bedtimeEnd
      , "bedtime_start" Json..= sleepPeriod.bedtimeStart
      , "deep_sleep_duration" Json..= sleepPeriod.deepSleepDuration
      , "efficiency" Json..= sleepPeriod.efficiency
      , "heart_rate" Json..= sleepPeriod.heartRate
      , "hrv" Json..= sleepPeriod.hrv
      , "latency" Json..= sleepPeriod.latency
      , "light_sleep_duration" Json..= sleepPeriod.lightSleepDuration
      , "low_battery_alert" Json..= sleepPeriod.lowBatteryAlert
      , "lowest_heart_rate" Json..= sleepPeriod.lowestHeartRate
      , "movement_30_sec" Json..= sleepPeriod.movementThirtySec
      , "period" Json..= sleepPeriod.period
      , "readiness" Json..= sleepPeriod.readiness
      , "readiness_score_delta" Json..= sleepPeriod.readinessScoreDelta
      , "rem_sleep_duration" Json..= sleepPeriod.remSleepDuration
      , "restless_periods" Json..= sleepPeriod.restlessPeriods
      , "sleep_algorithm_version" Json..= sleepPeriod.sleepAlgorithmVersion
      , "sleep_phase_5_min" Json..= sleepPeriod.sleepPhase5Min
      , "sleep_score_delta" Json..= sleepPeriod.sleepScoreDelta
      , "time_in_bed" Json..= sleepPeriod.timeInBed
      , "total_sleep_duration" Json..= sleepPeriod.totalSleepDuration
      , "type" Json..= sleepPeriod.type_
      ]

instance Json.FromJSON SleepPeriodData where
  parseJSON = Json.withObject "SleepPeriodData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    timestamp <- obj Json..: "timestamp"
    averageBreath <- obj Json..:? "average_breath"
    averageHeartRate <- obj Json..:? "average_heart_rate"
    averageHrv <- obj Json..:? "average_hrv"
    awakeTime <- obj Json..:? "awake_time"
    bedtimeEnd <- obj Json..:? "bedtime_end"
    bedtimeStart <- obj Json..:? "bedtime_start"
    deepSleepDuration <- obj Json..:? "deep_sleep_duration"
    efficiency <- obj Json..:? "efficiency"
    heartRate <- obj Json..:? "heart_rate"
    hrv <- obj Json..:? "hrv"
    latency <- obj Json..:? "latency"
    lightSleepDuration <- obj Json..:? "light_sleep_duration"
    lowBatteryAlert <- obj Json..:? "low_battery_alert"
    lowestHeartRate <- obj Json..:? "lowest_heart_rate"
    movementThirtySec <- obj Json..:? "movement_30_sec"
    period <- obj Json..:? "period"
    readiness <- obj Json..:? "readiness"
    readinessScoreDelta <- obj Json..:? "readiness_score_delta"
    remSleepDuration <- obj Json..:? "rem_sleep_duration"
    restlessPeriods <- obj Json..:? "restless_periods"
    sleepAlgorithmVersion <- obj Json..:? "sleep_algorithm_version"
    sleepPhase5Min <- obj Json..:? "sleep_phase_5_min"
    sleepScoreDelta <- obj Json..:? "sleep_score_delta"
    timeInBed <- obj Json..:? "time_in_bed"
    totalSleepDuration <- obj Json..:? "total_sleep_duration"
    type_ <- obj Json..:? "type"
    Json.yield (SleepPeriodData id day timestamp averageBreath averageHeartRate averageHrv awakeTime bedtimeEnd bedtimeStart deepSleepDuration efficiency heartRate hrv latency lightSleepDuration lowBatteryAlert lowestHeartRate movementThirtySec period readiness readinessScoreDelta remSleepDuration restlessPeriods sleepAlgorithmVersion sleepPhase5Min sleepScoreDelta timeInBed totalSleepDuration type_)


-- | Workout data from Oura Ring
data WorkoutData = WorkoutData
  { id :: Text
  , day :: Text
  , activity :: Maybe Text
  , calories :: Maybe Float
  , distance :: Maybe Float
  , endDatetime :: Maybe Text
  , intensity :: Maybe Text
  , label :: Maybe Text
  , source :: Maybe Text
  , startDatetime :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON WorkoutData where
  toJSON workout =
    Json.object
      [ "id" Json..= workout.id
      , "day" Json..= workout.day
      , "activity" Json..= workout.activity
      , "calories" Json..= workout.calories
      , "distance" Json..= workout.distance
      , "end_datetime" Json..= workout.endDatetime
      , "intensity" Json..= workout.intensity
      , "label" Json..= workout.label
      , "source" Json..= workout.source
      , "start_datetime" Json..= workout.startDatetime
      ]

instance Json.FromJSON WorkoutData where
  parseJSON = Json.withObject "WorkoutData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    activity <- obj Json..:? "activity"
    calories <- obj Json..:? "calories"
    distance <- obj Json..:? "distance"
    endDatetime <- obj Json..:? "end_datetime"
    intensity <- obj Json..:? "intensity"
    label <- obj Json..:? "label"
    source <- obj Json..:? "source"
    startDatetime <- obj Json..:? "start_datetime"
    Json.yield (WorkoutData id day activity calories distance endDatetime intensity label source startDatetime)


-- | Session data from Oura Ring
data SessionData = SessionData
  { id :: Text
  , day :: Text
  , endDatetime :: Maybe Text
  , heartRate :: Maybe SampleModel
  , hrv :: Maybe SampleModel
  , mood :: Maybe Text
  , motionCount :: Maybe SampleModel
  , startDatetime :: Maybe Text
  , type_ :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SessionData where
  toJSON session =
    Json.object
      [ "id" Json..= session.id
      , "day" Json..= session.day
      , "end_datetime" Json..= session.endDatetime
      , "heart_rate" Json..= session.heartRate
      , "hrv" Json..= session.hrv
      , "mood" Json..= session.mood
      , "motion_count" Json..= session.motionCount
      , "start_datetime" Json..= session.startDatetime
      , "type" Json..= session.type_
      ]

instance Json.FromJSON SessionData where
  parseJSON = Json.withObject "SessionData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    endDatetime <- obj Json..:? "end_datetime"
    heartRate <- obj Json..:? "heart_rate"
    hrv <- obj Json..:? "hrv"
    mood <- obj Json..:? "mood"
    motionCount <- obj Json..:? "motion_count"
    startDatetime <- obj Json..:? "start_datetime"
    type_ <- obj Json..:? "type"
    Json.yield (SessionData id day endDatetime heartRate hrv mood motionCount startDatetime type_)


-- | Personal info data from Oura Ring
data PersonalInfoData = PersonalInfoData
  { id :: Text
  , age :: Maybe Int
  , weight :: Maybe Float
  , height :: Maybe Float
  , biologicalSex :: Maybe Text
  , email :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON PersonalInfoData where
  toJSON info =
    Json.object
      [ "id" Json..= info.id
      , "age" Json..= info.age
      , "weight" Json..= info.weight
      , "height" Json..= info.height
      , "biological_sex" Json..= info.biologicalSex
      , "email" Json..= info.email
      ]

instance Json.FromJSON PersonalInfoData where
  parseJSON = Json.withObject "PersonalInfoData" \obj -> do
    id <- obj Json..: "id"
    age <- obj Json..:? "age"
    weight <- obj Json..:? "weight"
    height <- obj Json..:? "height"
    biologicalSex <- obj Json..:? "biological_sex"
    email <- obj Json..:? "email"
    Json.yield (PersonalInfoData id age weight height biologicalSex email)


-- | Daily stress data from Oura Ring
data DailyStressData = DailyStressData
  { id :: Text
  , day :: Text
  , stressHigh :: Maybe Float
  , recoveryHigh :: Maybe Float
  , daySummary :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON DailyStressData where
  toJSON stress =
    Json.object
      [ "id" Json..= stress.id
      , "day" Json..= stress.day
      , "stress_high" Json..= stress.stressHigh
      , "recovery_high" Json..= stress.recoveryHigh
      , "day_summary" Json..= stress.daySummary
      ]

instance Json.FromJSON DailyStressData where
  parseJSON = Json.withObject "DailyStressData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    stressHigh <- obj Json..:? "stress_high"
    recoveryHigh <- obj Json..:? "recovery_high"
    daySummary <- obj Json..:? "day_summary"
    Json.yield (DailyStressData id day stressHigh recoveryHigh daySummary)


-- | Daily SpO2 data from Oura Ring
data DailySpO2Data = DailySpO2Data
  { id :: Text
  , day :: Text
  , spO2Percentage :: Maybe SpO2AggregatedValues
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON DailySpO2Data where
  toJSON spo2 =
    Json.object
      [ "id" Json..= spo2.id
      , "day" Json..= spo2.day
      , "spo2_percentage" Json..= spo2.spO2Percentage
      ]

instance Json.FromJSON DailySpO2Data where
  parseJSON = Json.withObject "DailySpO2Data" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    spO2Percentage <- obj Json..:? "spo2_percentage"
    Json.yield (DailySpO2Data id day spO2Percentage)


-- | Daily resilience data from Oura Ring
data DailyResilienceData = DailyResilienceData
  { id :: Text
  , day :: Text
  , contributors :: Maybe ResilienceContributors
  , level :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON DailyResilienceData where
  toJSON resilience =
    Json.object
      [ "id" Json..= resilience.id
      , "day" Json..= resilience.day
      , "contributors" Json..= resilience.contributors
      , "level" Json..= resilience.level
      ]

instance Json.FromJSON DailyResilienceData where
  parseJSON = Json.withObject "DailyResilienceData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    contributors <- obj Json..:? "contributors"
    level <- obj Json..:? "level"
    Json.yield (DailyResilienceData id day contributors level)


-- | Daily cardiovascular age data from Oura Ring
data DailyCardiovascularAgeData = DailyCardiovascularAgeData
  { id :: Text
  , day :: Text
  , vascularAge :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON DailyCardiovascularAgeData where
  toJSON cardio =
    Json.object
      [ "id" Json..= cardio.id
      , "day" Json..= cardio.day
      , "vascular_age" Json..= cardio.vascularAge
      ]

instance Json.FromJSON DailyCardiovascularAgeData where
  parseJSON = Json.withObject "DailyCardiovascularAgeData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    vascularAge <- obj Json..:? "vascular_age"
    Json.yield (DailyCardiovascularAgeData id day vascularAge)


-- | VO2 max data from Oura Ring
data VO2MaxData = VO2MaxData
  { id :: Text
  , day :: Text
  , vo2Max :: Maybe Float
  , timestamp :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON VO2MaxData where
  toJSON vo2 =
    Json.object
      [ "id" Json..= vo2.id
      , "day" Json..= vo2.day
      , "vo2_max" Json..= vo2.vo2Max
      , "timestamp" Json..= vo2.timestamp
      ]

instance Json.FromJSON VO2MaxData where
  parseJSON = Json.withObject "VO2MaxData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    vo2Max <- obj Json..:? "vo2_max"
    timestamp <- obj Json..:? "timestamp"
    Json.yield (VO2MaxData id day vo2Max timestamp)


-- | Enhanced tag data from Oura Ring
data EnhancedTagData = EnhancedTagData
  { id :: Text
  , day :: Text
  , tagTypeCode :: Maybe Text
  , startTime :: Maybe Text
  , endTime :: Maybe Text
  , startDatetime :: Maybe Text
  , endDatetime :: Maybe Text
  , comment :: Maybe Text
  , timestamp :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON EnhancedTagData where
  toJSON tag =
    Json.object
      [ "id" Json..= tag.id
      , "day" Json..= tag.day
      , "tag_type_code" Json..= tag.tagTypeCode
      , "start_time" Json..= tag.startTime
      , "end_time" Json..= tag.endTime
      , "start_datetime" Json..= tag.startDatetime
      , "end_datetime" Json..= tag.endDatetime
      , "comment" Json..= tag.comment
      , "timestamp" Json..= tag.timestamp
      ]

instance Json.FromJSON EnhancedTagData where
  parseJSON = Json.withObject "EnhancedTagData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    tagTypeCode <- obj Json..:? "tag_type_code"
    startTime <- obj Json..:? "start_time"
    endTime <- obj Json..:? "end_time"
    startDatetime <- obj Json..:? "start_datetime"
    endDatetime <- obj Json..:? "end_datetime"
    comment <- obj Json..:? "comment"
    timestamp <- obj Json..:? "timestamp"
    Json.yield (EnhancedTagData id day tagTypeCode startTime endTime startDatetime endDatetime comment timestamp)


-- | Sleep time recommendation data from Oura Ring
data SleepTimeData = SleepTimeData
  { id :: Text
  , day :: Text
  , optimalBedtimeStart :: Maybe Text
  , optimalBedtimeEnd :: Maybe Text
  , recommendation :: Maybe Text
  , status :: Maybe Text
  , timestamp :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON SleepTimeData where
  toJSON sleepTime =
    Json.object
      [ "id" Json..= sleepTime.id
      , "day" Json..= sleepTime.day
      , "optimal_bedtime_start" Json..= sleepTime.optimalBedtimeStart
      , "optimal_bedtime_end" Json..= sleepTime.optimalBedtimeEnd
      , "recommendation" Json..= sleepTime.recommendation
      , "status" Json..= sleepTime.status
      , "timestamp" Json..= sleepTime.timestamp
      ]

instance Json.FromJSON SleepTimeData where
  parseJSON = Json.withObject "SleepTimeData" \obj -> do
    id <- obj Json..: "id"
    day <- obj Json..: "day"
    optimalBedtimeStart <- obj Json..:? "optimal_bedtime_start"
    optimalBedtimeEnd <- obj Json..:? "optimal_bedtime_end"
    recommendation <- obj Json..:? "recommendation"
    status <- obj Json..:? "status"
    timestamp <- obj Json..:? "timestamp"
    Json.yield (SleepTimeData id day optimalBedtimeStart optimalBedtimeEnd recommendation status timestamp)


-- | Rest mode period data from Oura Ring
data RestModePeriodData = RestModePeriodData
  { id :: Text
  , endDate :: Maybe Text
  , endDay :: Maybe Text
  , episodes :: Maybe [RestModeEpisode]
  , startDate :: Maybe Text
  , startDay :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON RestModePeriodData where
  toJSON restMode =
    Json.object
      [ "id" Json..= restMode.id
      , "end_date" Json..= restMode.endDate
      , "end_day" Json..= restMode.endDay
      , "episodes" Json..= restMode.episodes
      , "start_date" Json..= restMode.startDate
      , "start_day" Json..= restMode.startDay
      ]

instance Json.FromJSON RestModePeriodData where
  parseJSON = Json.withObject "RestModePeriodData" \obj -> do
    id <- obj Json..: "id"
    endDate <- obj Json..:? "end_date"
    endDay <- obj Json..:? "end_day"
    episodes <- obj Json..:? "episodes"
    startDate <- obj Json..:? "start_date"
    startDay <- obj Json..:? "start_day"
    Json.yield (RestModePeriodData id endDate endDay episodes startDate startDay)


-- | Ring configuration data from Oura Ring
data RingConfigurationData = RingConfigurationData
  { id :: Text
  , color :: Maybe Text
  , design :: Maybe Text
  , firmwareVersion :: Maybe Text
  , hardwareType :: Maybe Text
  , setUpAt :: Maybe Text
  , size :: Maybe Int
  }
  deriving (Show, Eq, Generic)

instance Json.ToJSON RingConfigurationData where
  toJSON ring =
    Json.object
      [ "id" Json..= ring.id
      , "color" Json..= ring.color
      , "design" Json..= ring.design
      , "firmware_version" Json..= ring.firmwareVersion
      , "hardware_type" Json..= ring.hardwareType
      , "set_up_at" Json..= ring.setUpAt
      , "size" Json..= ring.size
      ]

instance Json.FromJSON RingConfigurationData where
  parseJSON = Json.withObject "RingConfigurationData" \obj -> do
    id <- obj Json..: "id"
    color <- obj Json..:? "color"
    design <- obj Json..:? "design"
    firmwareVersion <- obj Json..:? "firmware_version"
    hardwareType <- obj Json..:? "hardware_type"
    setUpAt <- obj Json..:? "set_up_at"
    size <- obj Json..:? "size"
    Json.yield (RingConfigurationData id color design firmwareVersion hardwareType setUpAt size)
