module Integration.Oura.TypesSpec (spec) where

import Basics
import Integration.Oura.Types
import Json qualified
import Maybe (Maybe (Just, Nothing))
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "PaginatedResponse" do
    it "parses data and next_token" do
      let jsonText = "{\"data\": [], \"next_token\": \"abc123\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text (PaginatedResponse Int)
      case decoded of
        Result.Ok (PaginatedResponse [] (Just "abc123")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse PaginatedResponse with next_token"

    it "parses with null next_token" do
      let jsonText = "{\"data\": [], \"next_token\": null}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text (PaginatedResponse Int)
      case decoded of
        Result.Ok (PaginatedResponse [] Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse PaginatedResponse with null next_token"

  describe "SleepData" do
    it "parses from JSON with snake_case fields" do
      let jsonText = "{\"id\": \"sleep_123\", \"day\": \"2024-01-01\", \"score\": 85, \"timestamp\": \"2024-01-01T08:00:00Z\", \"contributors\": {\"deep_sleep\": 45, \"efficiency\": 90, \"latency\": 10, \"rem_sleep\": 60, \"restfulness\": 85, \"timing\": 75, \"total_sleep\": 480}}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepData
      case decoded of
        Result.Ok (SleepData "sleep_123" "2024-01-01" (Just 85) "2024-01-01T08:00:00Z" (Just (SleepContributors (Just 45) (Just 90) (Just 10) (Just 60) (Just 85) (Just 75) (Just 480)))) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepData with contributors"

    it "handles null score gracefully" do
      let jsonText = "{\"id\": \"sleep_456\", \"day\": \"2024-01-02\", \"score\": null, \"timestamp\": \"2024-01-02T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepData
      case decoded of
        Result.Ok (SleepData "sleep_456" "2024-01-02" Nothing "2024-01-02T08:00:00Z" Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepData with null score"

    it "handles missing contributors" do
      let jsonText = "{\"id\": \"sleep_789\", \"day\": \"2024-01-03\", \"score\": 75, \"timestamp\": \"2024-01-03T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepData
      case decoded of
        Result.Ok (SleepData "sleep_789" "2024-01-03" (Just 75) "2024-01-03T08:00:00Z" Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepData without contributors"

  describe "SleepContributors" do
    it "parses all contributor fields" do
      let jsonText = "{\"deep_sleep\": 50, \"efficiency\": 92, \"latency\": 8, \"rem_sleep\": 65, \"restfulness\": 88, \"timing\": 80, \"total_sleep\": 500}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepContributors
      case decoded of
        Result.Ok (SleepContributors (Just 50) (Just 92) (Just 8) (Just 65) (Just 88) (Just 80) (Just 500)) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepContributors"

    it "handles null contributor fields" do
      let jsonText = "{\"deep_sleep\": null, \"efficiency\": 92, \"latency\": null, \"rem_sleep\": 65, \"restfulness\": null, \"timing\": 80, \"total_sleep\": null}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepContributors
      case decoded of
        Result.Ok (SleepContributors Nothing (Just 92) Nothing (Just 65) Nothing (Just 80) Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepContributors with nulls"

  describe "ActivityData" do
    it "parses from JSON with all fields" do
      let jsonText = "{\"id\": \"activity_123\", \"day\": \"2024-01-01\", \"score\": 88, \"active_calories\": 450, \"steps\": 8500, \"total_calories\": 2100, \"high_activity_time\": 45, \"medium_activity_time\": 120, \"low_activity_time\": 300, \"timestamp\": \"2024-01-01T23:59:59Z\", \"average_met_minutes\": 1.5, \"class_5_min\": \"0112340\", \"contributors\": {\"meet_daily_targets\": 80, \"move_every_hour\": 90, \"recovery_time\": 75, \"stay_active\": 85, \"training_frequency\": 70, \"training_volume\": 65}, \"equivalent_walking_distance\": 7200, \"high_activity_met_minutes\": 30, \"inactivity_alerts\": 2, \"low_activity_met_minutes\": 60, \"medium_activity_met_minutes\": 45, \"met\": {\"interval\": 60.0, \"items\": [1.0, 2.5], \"timestamp\": \"2024-01-01T00:00:00Z\"}, \"meters_to_target\": 3000, \"non_wear_time\": 1800, \"resting_time\": 28800, \"sedentary_met_minutes\": 20, \"sedentary_time\": 36000, \"target_calories\": 350, \"target_meters\": 10000}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ActivityData
      case decoded of
        Result.Ok (ActivityData "activity_123" "2024-01-01" (Just 88) (Just 450) (Just 8500) (Just 2100) (Just 45) (Just 120) (Just 300) "2024-01-01T23:59:59Z" (Just 1.5) (Just "0112340") (Just (ActivityContributors (Just 80) (Just 90) (Just 75) (Just 85) (Just 70) (Just 65))) (Just 7200) (Just 30) (Just 2) (Just 60) (Just 45) (Just (SampleModel (Just 60.0) (Just [Just 1.0, Just 2.5]) (Just "2024-01-01T00:00:00Z"))) (Just 3000) (Just 1800) (Just 28800) (Just 20) (Just 36000) (Just 350) (Just 10000)) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ActivityData"

    it "handles null and missing optional fields" do
      let jsonText = "{\"id\": \"activity_456\", \"day\": \"2024-01-02\", \"score\": null, \"active_calories\": null, \"steps\": 5000, \"total_calories\": null, \"high_activity_time\": 0, \"medium_activity_time\": 60, \"low_activity_time\": 500, \"timestamp\": \"2024-01-02T23:59:59Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ActivityData
      case decoded of
        Result.Ok (ActivityData "activity_456" "2024-01-02" Nothing Nothing (Just 5000) Nothing (Just 0) (Just 60) (Just 500) "2024-01-02T23:59:59Z" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ActivityData with nulls"

  describe "ReadinessData" do
    it "parses from JSON with snake_case fields" do
      let jsonText = "{\"id\": \"readiness_123\", \"day\": \"2024-01-01\", \"score\": 82, \"temperature_deviation\": 0, \"temperature_trend_deviation\": 0, \"timestamp\": \"2024-01-01T08:00:00Z\", \"contributors\": {\"activity_balance\": 80, \"body_temperature\": 85, \"hrv_balance\": 75, \"previous_day_activity\": 88, \"previous_night\": 90, \"recovery_index\": 78, \"resting_heart_rate\": 82, \"sleep_balance\": 85, \"sleep_regularity\": 70}}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ReadinessData
      case decoded of
        Result.Ok (ReadinessData "readiness_123" "2024-01-01" (Just 82) (Just 0) (Just 0) "2024-01-01T08:00:00Z" (Just (ReadinessContributors (Just 80) (Just 85) (Just 75) (Just 88) (Just 90) (Just 78) (Just 82) (Just 85) (Just 70)))) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ReadinessData with contributors"

    it "handles null score and contributors" do
      let jsonText = "{\"id\": \"readiness_456\", \"day\": \"2024-01-02\", \"score\": null, \"temperature_deviation\": 1, \"temperature_trend_deviation\": -1, \"timestamp\": \"2024-01-02T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ReadinessData
      case decoded of
        Result.Ok (ReadinessData "readiness_456" "2024-01-02" Nothing (Just 1) (Just (-1)) "2024-01-02T08:00:00Z" Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ReadinessData with nulls"

  describe "ReadinessContributors" do
    it "parses all contributor fields" do
      let jsonText = "{\"activity_balance\": 80, \"body_temperature\": 85, \"hrv_balance\": 75, \"previous_day_activity\": 88, \"previous_night\": 90, \"recovery_index\": 78, \"resting_heart_rate\": 82, \"sleep_balance\": 85, \"sleep_regularity\": 70}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ReadinessContributors
      case decoded of
        Result.Ok (ReadinessContributors (Just 80) (Just 85) (Just 75) (Just 88) (Just 90) (Just 78) (Just 82) (Just 85) (Just 70)) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ReadinessContributors"

  describe "HeartRateData" do
    it "parses from JSON" do
      let jsonText = "{\"bpm\": 72, \"source\": \"awake\", \"timestamp\": \"2024-01-01T10:30:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text HeartRateData
      case decoded of
        Result.Ok (HeartRateData 72 "awake" "2024-01-01T10:30:00Z") ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse HeartRateData"

    it "parses different heart rate sources" do
      let jsonText = "{\"bpm\": 65, \"source\": \"sleep\", \"timestamp\": \"2024-01-01T03:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text HeartRateData
      case decoded of
        Result.Ok (HeartRateData 65 "sleep" "2024-01-01T03:00:00Z") ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse HeartRateData with sleep source"

  describe "SleepPeriodData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"sp_1\", \"day\": \"2024-01-01\", \"timestamp\": \"2024-01-01T08:00:00Z\", \"average_breath\": 15.5, \"average_heart_rate\": 55.2, \"average_hrv\": 42.0, \"awake_time\": 1800, \"bedtime_end\": \"2024-01-01T07:30:00Z\", \"bedtime_start\": \"2024-01-01T00:00:00Z\", \"deep_sleep_duration\": 3600, \"efficiency\": 92, \"heart_rate\": {\"interval\": 300.0, \"items\": [55.0, 52.0], \"timestamp\": \"2024-01-01T00:00:00Z\"}, \"hrv\": {\"interval\": 300.0, \"items\": [40.0], \"timestamp\": \"2024-01-01T00:00:00Z\"}, \"latency\": 600, \"light_sleep_duration\": 14400, \"low_battery_alert\": false, \"lowest_heart_rate\": 48, \"movement_30_sec\": \"11100\", \"period\": 0, \"readiness\": {\"contributors\": null, \"score\": 80, \"temperature_deviation\": 0.1, \"temperature_trend_deviation\": -0.05}, \"readiness_score_delta\": 1.5, \"rem_sleep_duration\": 5400, \"restless_periods\": 3, \"sleep_algorithm_version\": \"v2\", \"sleep_phase_5_min\": \"443221\", \"sleep_score_delta\": -2.0, \"time_in_bed\": 27000, \"total_sleep_duration\": 25200, \"type\": \"long_sleep\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepPeriodData
      case decoded of
        Result.Ok sleepPeriod -> do
          sleepPeriod.id `shouldBe` "sp_1"
          sleepPeriod.day `shouldBe` "2024-01-01"
          sleepPeriod.timestamp `shouldBe` "2024-01-01T08:00:00Z"
          sleepPeriod.averageBreath `shouldBe` Just 15.5
          sleepPeriod.averageHeartRate `shouldBe` Just 55.2
          sleepPeriod.averageHrv `shouldBe` Just 42.0
          sleepPeriod.awakeTime `shouldBe` Just 1800
          sleepPeriod.deepSleepDuration `shouldBe` Just 3600
          sleepPeriod.efficiency `shouldBe` Just 92
          sleepPeriod.latency `shouldBe` Just 600
          sleepPeriod.lightSleepDuration `shouldBe` Just 14400
          sleepPeriod.lowBatteryAlert `shouldBe` Just False
          sleepPeriod.lowestHeartRate `shouldBe` Just 48
          sleepPeriod.movementThirtySec `shouldBe` Just "11100"
          sleepPeriod.period `shouldBe` Just 0
          sleepPeriod.readinessScoreDelta `shouldBe` Just 1.5
          sleepPeriod.remSleepDuration `shouldBe` Just 5400
          sleepPeriod.restlessPeriods `shouldBe` Just 3
          sleepPeriod.sleepAlgorithmVersion `shouldBe` Just "v2"
          sleepPeriod.sleepPhase5Min `shouldBe` Just "443221"
          sleepPeriod.sleepScoreDelta `shouldBe` Just (-2.0)
          sleepPeriod.timeInBed `shouldBe` Just 27000
          sleepPeriod.totalSleepDuration `shouldBe` Just 25200
          sleepPeriod.type_ `shouldBe` Just "long_sleep"
        _ ->
          expectationFailure "Failed to parse SleepPeriodData"

    it "parses with minimal required fields" do
      let jsonText = "{\"id\": \"sp_2\", \"day\": \"2024-01-02\", \"timestamp\": \"2024-01-02T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepPeriodData
      case decoded of
        Result.Ok sleepPeriod -> do
          sleepPeriod.id `shouldBe` "sp_2"
          sleepPeriod.averageBreath `shouldBe` Nothing
          sleepPeriod.lowBatteryAlert `shouldBe` Nothing
          sleepPeriod.type_ `shouldBe` Nothing
        _ ->
          expectationFailure "Failed to parse SleepPeriodData with minimal fields"

  describe "WorkoutData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"workout_1\", \"day\": \"2024-01-01\", \"activity\": \"running\", \"calories\": 350.5, \"distance\": 5200.0, \"end_datetime\": \"2024-01-01T08:00:00Z\", \"intensity\": \"moderate\", \"label\": \"Morning Run\", \"source\": \"manual\", \"start_datetime\": \"2024-01-01T07:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text WorkoutData
      case decoded of
        Result.Ok (WorkoutData "workout_1" "2024-01-01" (Just "running") (Just 350.5) (Just 5200.0) (Just "2024-01-01T08:00:00Z") (Just "moderate") (Just "Morning Run") (Just "manual") (Just "2024-01-01T07:00:00Z")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse WorkoutData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"workout_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text WorkoutData
      case decoded of
        Result.Ok (WorkoutData "workout_2" "2024-01-02" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse WorkoutData with minimal fields"

  describe "SessionData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"session_1\", \"day\": \"2024-01-01\", \"end_datetime\": \"2024-01-01T09:00:00Z\", \"heart_rate\": {\"interval\": 60.0, \"items\": [70.0], \"timestamp\": \"2024-01-01T08:30:00Z\"}, \"hrv\": {\"interval\": 60.0, \"items\": [45.0], \"timestamp\": \"2024-01-01T08:30:00Z\"}, \"mood\": \"good\", \"motion_count\": {\"interval\": 60.0, \"items\": [5.0], \"timestamp\": \"2024-01-01T08:30:00Z\"}, \"start_datetime\": \"2024-01-01T08:30:00Z\", \"type\": \"breathing\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SessionData
      case decoded of
        Result.Ok session -> do
          session.id `shouldBe` "session_1"
          session.day `shouldBe` "2024-01-01"
          session.mood `shouldBe` Just "good"
          session.type_ `shouldBe` Just "breathing"
        _ ->
          expectationFailure "Failed to parse SessionData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"session_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SessionData
      case decoded of
        Result.Ok (SessionData "session_2" "2024-01-02" Nothing Nothing Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SessionData with minimal fields"

  describe "PersonalInfoData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"pi_1\", \"age\": 30, \"weight\": 75.5, \"height\": 180.0, \"biological_sex\": \"male\", \"email\": \"test@example.com\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text PersonalInfoData
      case decoded of
        Result.Ok (PersonalInfoData "pi_1" (Just 30) (Just 75.5) (Just 180.0) (Just "male") (Just "test@example.com")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse PersonalInfoData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"pi_2\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text PersonalInfoData
      case decoded of
        Result.Ok (PersonalInfoData "pi_2" Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse PersonalInfoData with minimal fields"

  describe "DailyStressData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"stress_1\", \"day\": \"2024-01-01\", \"stress_high\": 85.5, \"recovery_high\": 70.2, \"day_summary\": \"restored\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyStressData
      case decoded of
        Result.Ok (DailyStressData "stress_1" "2024-01-01" (Just 85.5) (Just 70.2) (Just "restored")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyStressData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"stress_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyStressData
      case decoded of
        Result.Ok (DailyStressData "stress_2" "2024-01-02" Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyStressData with minimal fields"

  describe "DailySpO2Data" do
    it "parses with SpO2 percentage" do
      let jsonText = "{\"id\": \"spo2_1\", \"day\": \"2024-01-01\", \"spo2_percentage\": {\"average\": 98.5}}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailySpO2Data
      case decoded of
        Result.Ok (DailySpO2Data "spo2_1" "2024-01-01" (Just (SpO2AggregatedValues 98.5))) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailySpO2Data"

    it "parses with null SpO2 percentage" do
      let jsonText = "{\"id\": \"spo2_2\", \"day\": \"2024-01-02\", \"spo2_percentage\": null}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailySpO2Data
      case decoded of
        Result.Ok (DailySpO2Data "spo2_2" "2024-01-02" Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailySpO2Data with null percentage"

  describe "DailyResilienceData" do
    it "parses with contributors" do
      let jsonText = "{\"id\": \"res_1\", \"day\": \"2024-01-01\", \"contributors\": {\"sleep_recovery\": 80.0, \"daytime_recovery\": 75.5, \"stress\": 60.0}, \"level\": \"good\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyResilienceData
      case decoded of
        Result.Ok (DailyResilienceData "res_1" "2024-01-01" (Just (ResilienceContributors (Just 80.0) (Just 75.5) (Just 60.0))) (Just "good")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyResilienceData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"res_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyResilienceData
      case decoded of
        Result.Ok (DailyResilienceData "res_2" "2024-01-02" Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyResilienceData with minimal fields"

  describe "DailyCardiovascularAgeData" do
    it "parses with vascular age" do
      let jsonText = "{\"id\": \"cardio_1\", \"day\": \"2024-01-01\", \"vascular_age\": 35}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyCardiovascularAgeData
      case decoded of
        Result.Ok (DailyCardiovascularAgeData "cardio_1" "2024-01-01" (Just 35)) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyCardiovascularAgeData"

    it "parses with null vascular age" do
      let jsonText = "{\"id\": \"cardio_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text DailyCardiovascularAgeData
      case decoded of
        Result.Ok (DailyCardiovascularAgeData "cardio_2" "2024-01-02" Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse DailyCardiovascularAgeData with null age"

  describe "VO2MaxData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"vo2_1\", \"day\": \"2024-01-01\", \"vo2_max\": 45.5, \"timestamp\": \"2024-01-01T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text VO2MaxData
      case decoded of
        Result.Ok (VO2MaxData "vo2_1" "2024-01-01" (Just 45.5) (Just "2024-01-01T08:00:00Z")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse VO2MaxData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"vo2_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text VO2MaxData
      case decoded of
        Result.Ok (VO2MaxData "vo2_2" "2024-01-02" Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse VO2MaxData with minimal fields"

  describe "EnhancedTagData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"tag_1\", \"day\": \"2024-01-01\", \"tag_type_code\": \"tag_generic_nocturnal\", \"start_time\": \"22:00:00\", \"end_time\": \"06:00:00\", \"start_datetime\": \"2024-01-01T22:00:00Z\", \"end_datetime\": \"2024-01-02T06:00:00Z\", \"comment\": \"Felt restless\", \"timestamp\": \"2024-01-01T22:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text EnhancedTagData
      case decoded of
        Result.Ok tag -> do
          tag.id `shouldBe` "tag_1"
          tag.tagTypeCode `shouldBe` Just "tag_generic_nocturnal"
          tag.startTime `shouldBe` Just "22:00:00"
          tag.endTime `shouldBe` Just "06:00:00"
          tag.comment `shouldBe` Just "Felt restless"
        _ ->
          expectationFailure "Failed to parse EnhancedTagData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"tag_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text EnhancedTagData
      case decoded of
        Result.Ok (EnhancedTagData "tag_2" "2024-01-02" Nothing Nothing Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse EnhancedTagData with minimal fields"

  describe "SleepTimeData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"st_1\", \"day\": \"2024-01-01\", \"optimal_bedtime_start\": \"22:30:00\", \"optimal_bedtime_end\": \"23:30:00\", \"recommendation\": \"improve_timing\", \"status\": \"not_enough_data\", \"timestamp\": \"2024-01-01T08:00:00Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepTimeData
      case decoded of
        Result.Ok (SleepTimeData "st_1" "2024-01-01" (Just "22:30:00") (Just "23:30:00") (Just "improve_timing") (Just "not_enough_data") (Just "2024-01-01T08:00:00Z")) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepTimeData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"st_2\", \"day\": \"2024-01-02\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text SleepTimeData
      case decoded of
        Result.Ok (SleepTimeData "st_2" "2024-01-02" Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse SleepTimeData with minimal fields"

  describe "RestModePeriodData" do
    it "parses with episodes" do
      let jsonText = "{\"id\": \"rmp_1\", \"end_date\": \"2024-01-05\", \"end_day\": \"2024-01-05\", \"episodes\": [{\"tags\": [\"sick\"], \"timestamp\": \"2024-01-01T08:00:00Z\"}], \"start_date\": \"2024-01-01\", \"start_day\": \"2024-01-01\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text RestModePeriodData
      case decoded of
        Result.Ok restMode -> do
          restMode.id `shouldBe` "rmp_1"
          restMode.endDate `shouldBe` Just "2024-01-05"
          restMode.startDay `shouldBe` Just "2024-01-01"
          case restMode.episodes of
            Just [RestModeEpisode (Just ["sick"]) "2024-01-01T08:00:00Z"] ->
              True `shouldBe` True
            _ ->
              expectationFailure "Failed to parse episodes"
        _ ->
          expectationFailure "Failed to parse RestModePeriodData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"rmp_2\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text RestModePeriodData
      case decoded of
        Result.Ok (RestModePeriodData "rmp_2" Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse RestModePeriodData with minimal fields"

  describe "RingConfigurationData" do
    it "parses with all fields" do
      let jsonText = "{\"id\": \"ring_1\", \"color\": \"silver\", \"design\": \"heritage\", \"firmware_version\": \"2.8.10\", \"hardware_type\": \"gen3\", \"set_up_at\": \"2023-06-15T10:00:00Z\", \"size\": 9}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text RingConfigurationData
      case decoded of
        Result.Ok (RingConfigurationData "ring_1" (Just "silver") (Just "heritage") (Just "2.8.10") (Just "gen3") (Just "2023-06-15T10:00:00Z") (Just 9)) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse RingConfigurationData"

    it "parses with minimal fields" do
      let jsonText = "{\"id\": \"ring_2\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text RingConfigurationData
      case decoded of
        Result.Ok (RingConfigurationData "ring_2" Nothing Nothing Nothing Nothing Nothing Nothing) ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse RingConfigurationData with minimal fields"
