module Integration.Acs.ResponseSpec (spec) where

import Basics
import Integration.Acs.Response (Response (..))
import Json qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)
import Text qualified


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

    it "decodes the ACS wire key 'id' into operationId (contract: external field is 'id')" do
      -- A real ACS 202 body uses "id", not "operationId".
      let decoded = Json.decodeText "{\"id\":\"raw-op-id\",\"status\":\"Running\"}" :: Result Text Response
      decoded `shouldBe` Result.Ok (Response { operationId = "raw-op-id" })

    it "encodes operationId under the wire key 'id', never 'operationId' (contract)" do
      let encoded = Json.encodeText (Response { operationId = "op-555" })
      Text.contains "\"id\"" encoded `shouldBe` True
      Text.contains "op-555" encoded `shouldBe` True
      Text.contains "operationId" encoded `shouldBe` False

  describe "Integration.Acs.Response serialization" do
    it "round-trips Response through JSON encoding/decoding" do
      let original = Response { operationId = "op-123" }
      let encoded = Json.encodeText original
      let decoded = Json.decodeText encoded :: Result Text Response
      decoded `shouldBe` Result.Ok original
