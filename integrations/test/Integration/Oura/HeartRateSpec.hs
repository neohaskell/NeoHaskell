module Integration.Oura.HeartRateSpec (spec) where

import Integration.Oura.HeartRate (HeartRate (..))
import Maybe (Maybe (..))
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "HeartRate" do
    it "constructs with datetime fields" do
      let userId = "user_123" :: Text
      let startDatetime = "2021-11-01T00:00:00-08:00" :: Text
      let endDatetime = "2021-11-02T00:00:00-08:00" :: Text
      let onSuccess = \_ -> "success" :: Text
      let onError = Nothing :: Maybe (Text -> Text)
      let heartRate = HeartRate userId startDatetime endDatetime onSuccess onError
      heartRate.userId `shouldBe` "user_123"
      heartRate.startDatetime `shouldBe` "2021-11-01T00:00:00-08:00"
      heartRate.endDatetime `shouldBe` "2021-11-02T00:00:00-08:00"

    it "supports ISO 8601 datetime with timezone" do
      let startDatetime = "2024-01-15T08:30:00+05:30" :: Text
      let endDatetime = "2024-01-15T18:45:00+05:30" :: Text
      let onSuccess = \_ -> "success" :: Text
      let heartRate = HeartRate "user_456" startDatetime endDatetime onSuccess Nothing
      heartRate.startDatetime `shouldBe` "2024-01-15T08:30:00+05:30"
      heartRate.endDatetime `shouldBe` "2024-01-15T18:45:00+05:30"

    it "supports negative timezone offsets" do
      let startDatetime = "2024-01-15T08:30:00-05:00" :: Text
      let endDatetime = "2024-01-15T18:45:00-05:00" :: Text
      let onSuccess = \_ -> "success" :: Text
      let heartRate = HeartRate "user_789" startDatetime endDatetime onSuccess Nothing
      heartRate.startDatetime `shouldBe` "2024-01-15T08:30:00-05:00"
      heartRate.endDatetime `shouldBe` "2024-01-15T18:45:00-05:00"

    it "supports error callback" do
      let onSuccess = \_ -> "success" :: Text
      let onError = Just (\_ -> "error_handled") :: Maybe (Text -> Text)
      let heartRate = HeartRate "user_999" "2024-01-01T00:00:00Z" "2024-01-02T00:00:00Z" onSuccess onError
      case heartRate.onError of
        Just errorHandler ->
          errorHandler "test error" `shouldBe` "error_handled"
        Nothing ->
          expectationFailure "Expected error handler to be present"
