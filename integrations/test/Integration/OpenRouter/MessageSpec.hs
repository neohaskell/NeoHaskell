module Integration.OpenRouter.MessageSpec (spec) where

import Basics
import Integration.OpenRouter.Message
import Json qualified
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "Message" do
    describe "smart constructors" do
      it "creates user message" do
        let msg = user "Hello"
        msg.role `shouldBe` User
        msg.content `shouldBe` "Hello"

      it "creates assistant message" do
        let msg = assistant "Hi there"
        msg.role `shouldBe` Assistant
        msg.content `shouldBe` "Hi there"

      it "creates system message" do
        let msg = system "You are helpful"
        msg.role `shouldBe` System
        msg.content `shouldBe` "You are helpful"

    describe "JSON encoding" do
      it "encodes Role to correct JSON strings" do
        Json.encodeText User `shouldBe` "\"user\""
        Json.encodeText Assistant `shouldBe` "\"assistant\""
        Json.encodeText System `shouldBe` "\"system\""

      it "encodes Message to JSON object with role and content" do
        let msg = user "Hello"
        let encoded = Json.encodeText msg
        Text.contains "\"role\":\"user\"" encoded `shouldBe` True
        Text.contains "\"content\":\"Hello\"" encoded `shouldBe` True

    describe "JSON decoding" do
      it "decodes Role from JSON strings" do
        (Json.decodeText "\"user\"" :: Result Text Role) `shouldBe` Result.Ok User
        (Json.decodeText "\"assistant\"" :: Result Text Role) `shouldBe` Result.Ok Assistant
        (Json.decodeText "\"system\"" :: Result Text Role) `shouldBe` Result.Ok System

      it "decodes Message from JSON object" do
        let jsonText = "{\"role\":\"user\",\"content\":\"Hello\"}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Message
        case decoded of
          Result.Ok msg -> do
            msg.role `shouldBe` User
            msg.content `shouldBe` "Hello"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])
