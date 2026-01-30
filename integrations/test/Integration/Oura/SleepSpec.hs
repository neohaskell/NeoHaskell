module Integration.Oura.SleepSpec (spec) where

import Integration.Oura.Sleep (DailySleep (..))
import Maybe (Maybe (..))
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "DailySleep" do
    it "can be constructed with all fields" do
      let userId = "user-123" :: Text
      let startDate = "2024-01-01" :: Text
      let endDate = "2024-01-31" :: Text
      let onSuccess = \_ -> "success" :: Text
      let onError = Just (\_ -> "error" :: Text)
      let request = DailySleep userId startDate endDate onSuccess onError
      request.userId `shouldBe` userId
      request.startDate `shouldBe` startDate
      request.endDate `shouldBe` endDate

    it "can be constructed without error callback" do
      let userId = "user-456" :: Text
      let startDate = "2024-02-01" :: Text
      let endDate = "2024-02-28" :: Text
      let onSuccess = \_ -> "success" :: Text
      let request = DailySleep userId startDate endDate onSuccess Nothing
      request.userId `shouldBe` userId

    it "stores date range correctly" do
      let request = DailySleep "user-789" "2024-03-01" "2024-03-31" (\_ -> ()) Nothing
      request.startDate `shouldBe` "2024-03-01"
      request.endDate `shouldBe` "2024-03-31"
