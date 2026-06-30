module Integration.Acs.ResponseSpec (spec) where

import Integration.Acs.Response (Response (..))
import Json qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)


spec :: Spec
spec = do
  describe "Integration.Acs.Response" do
    it "constructs Response with operationId (happy path)" do
      let r = Response { operationId = "op-abc-123" }
      r.operationId `shouldBe` "op-abc-123"

    it "round-trips Response to JSON and back (serialization: ToJSON / FromJSON)" do
      let r = Response { operationId = "op-xyz-789" }
      let encoded = Json.encodeText r
      let decoded = Json.decodeText encoded :: Result Text Response
      decoded `shouldBe` Result.Ok r

  describe "Integration.Acs.Response serialization" do
    it "round-trips Response through JSON encoding/decoding" do
      let original = Response { operationId = "op-123" }
      let encoded = Json.encodeText original
      let decoded = Json.decodeText encoded :: Result Text Response
      decoded `shouldBe` Result.Ok original
