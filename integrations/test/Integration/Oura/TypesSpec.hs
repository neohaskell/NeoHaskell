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
    it "parses from JSON with snake_case fields" do
      let jsonText = "{\"id\": \"activity_123\", \"day\": \"2024-01-01\", \"score\": 88, \"active_calories\": 450, \"steps\": 8500, \"total_calories\": 2100, \"high_activity_time\": 45, \"medium_activity_time\": 120, \"low_activity_time\": 300, \"timestamp\": \"2024-01-01T23:59:59Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ActivityData
      case decoded of
        Result.Ok (ActivityData "activity_123" "2024-01-01" (Just 88) (Just 450) (Just 8500) (Just 2100) (Just 45) (Just 120) (Just 300) "2024-01-01T23:59:59Z") ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ActivityData"

    it "handles null score and calories" do
      let jsonText = "{\"id\": \"activity_456\", \"day\": \"2024-01-02\", \"score\": null, \"active_calories\": null, \"steps\": 5000, \"total_calories\": null, \"high_activity_time\": 0, \"medium_activity_time\": 60, \"low_activity_time\": 500, \"timestamp\": \"2024-01-02T23:59:59Z\"}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ActivityData
      case decoded of
        Result.Ok (ActivityData "activity_456" "2024-01-02" Nothing Nothing (Just 5000) Nothing (Just 0) (Just 60) (Just 500) "2024-01-02T23:59:59Z") ->
          True `shouldBe` True
        _ ->
          expectationFailure "Failed to parse ActivityData with nulls"

  describe "ReadinessData" do
    it "parses from JSON with snake_case fields" do
      let jsonText = "{\"id\": \"readiness_123\", \"day\": \"2024-01-01\", \"score\": 82, \"temperature_deviation\": 0, \"temperature_trend_deviation\": 0, \"timestamp\": \"2024-01-01T08:00:00Z\", \"contributors\": {\"activity_balance\": 80, \"body_temperature\": 85, \"hrv_balance\": 75, \"previous_day_activity\": 88, \"previous_night\": 90, \"recovery_index\": 78, \"resting_heart_rate\": 82, \"sleep_balance\": 85}}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ReadinessData
      case decoded of
        Result.Ok (ReadinessData "readiness_123" "2024-01-01" (Just 82) (Just 0) (Just 0) "2024-01-01T08:00:00Z" (Just (ReadinessContributors (Just 80) (Just 85) (Just 75) (Just 88) (Just 90) (Just 78) (Just 82) (Just 85)))) ->
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
      let jsonText = "{\"activity_balance\": 80, \"body_temperature\": 85, \"hrv_balance\": 75, \"previous_day_activity\": 88, \"previous_night\": 90, \"recovery_index\": 78, \"resting_heart_rate\": 82, \"sleep_balance\": 85}" :: Text
      let decoded = Json.decodeText jsonText :: Result Text ReadinessContributors
      case decoded of
        Result.Ok (ReadinessContributors (Just 80) (Just 85) (Just 75) (Just 88) (Just 90) (Just 78) (Just 82) (Just 85)) ->
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
