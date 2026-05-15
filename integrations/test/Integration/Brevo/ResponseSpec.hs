module Integration.Brevo.ResponseSpec (spec) where

import Integration.Brevo.Response (Response (..))
import Json qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "Integration.Brevo.Response" do
    it "constructs Response with messageId (happy path)" do
      let r = Response { messageId = "msg-abc-123" }
      r.messageId `shouldBe` "msg-abc-123"

    it "round-trips Response to JSON and back (serialization)" do
      let r = Response { messageId = "msg-xyz-789" }
      let encoded = Json.encodeText r
      let decoded = Json.decodeText encoded :: Result Text Response
      decoded `shouldBe` Result.Ok r
