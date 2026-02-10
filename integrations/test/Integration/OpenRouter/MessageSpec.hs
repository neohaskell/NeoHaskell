module Integration.OpenRouter.MessageSpec (spec) where

import Array qualified
import Basics
import Integration.OpenRouter.Message
import Json qualified
import Maybe (Maybe (..))
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
        msg.content `shouldBe` TextContent "Hello"

      it "creates assistant message" do
        let msg = assistant "Hi there"
        msg.role `shouldBe` Assistant
        msg.content `shouldBe` TextContent "Hi there"

      it "creates system message" do
        let msg = system "You are helpful"
        msg.role `shouldBe` System
        msg.content `shouldBe` TextContent "You are helpful"

    describe "JSON encoding" do
      it "encodes Role to correct JSON strings" do
        Json.encodeText User `shouldBe` "\"user\""
        Json.encodeText Assistant `shouldBe` "\"assistant\""
        Json.encodeText System `shouldBe` "\"system\""

      it "encodes text-only Message to JSON with string content (backward compatible)" do
        let msg = user "Hello"
        let encoded = Json.encodeText msg
        Text.contains "\"role\":\"user\"" encoded `shouldBe` True
        Text.contains "\"content\":\"Hello\"" encoded `shouldBe` True

      it "encodes multimodal Message to JSON with content array" do
        let msg = userWithAttachment "Describe this" "base64data" "image/png"
        let encoded = Json.encodeText msg
        Text.contains "\"role\":\"user\"" encoded `shouldBe` True
        Text.contains "\"type\":\"text\"" encoded `shouldBe` True
        Text.contains "\"type\":\"image_url\"" encoded `shouldBe` True
        Text.contains "data:image/png;base64,base64data" encoded `shouldBe` True

    describe "JSON decoding" do
      it "decodes Role from JSON strings" do
        (Json.decodeText "\"user\"" :: Result Text Role) `shouldBe` Result.Ok User
        (Json.decodeText "\"assistant\"" :: Result Text Role) `shouldBe` Result.Ok Assistant
        (Json.decodeText "\"system\"" :: Result Text Role) `shouldBe` Result.Ok System

      it "decodes Message with string content (backward compatible)" do
        let jsonText = "{\"role\":\"user\",\"content\":\"Hello\"}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Message
        case decoded of
          Result.Ok msg -> do
            msg.role `shouldBe` User
            msg.content `shouldBe` TextContent "Hello"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      it "decodes Message with array content (multimodal)" do
        let jsonText = "{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"Hello\"},{\"type\":\"image_url\",\"image_url\":{\"url\":\"data:image/png;base64,abc\"}}]}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Message
        case decoded of
          Result.Ok msg -> do
            msg.role `shouldBe` User
            case msg.content of
              MultiContent parts -> do
                Array.length parts `shouldBe` 2
              TextContent _ ->
                expectationFailure "Expected MultiContent, got TextContent"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

    describe "Content JSON roundtrip" do
      it "roundtrips TextContent" do
        let content = TextContent "hello"
        let encoded = Json.encodeText content
        let decoded = Json.decodeText encoded :: Result Text Content
        decoded `shouldBe` Result.Ok content

      it "roundtrips MultiContent" do
        let content = MultiContent (Array.fromLinkedList [TextPart "hello", ImageUrlPart ImageUrl {url = "data:image/png;base64,abc"}])
        let encoded = Json.encodeText content
        let decoded = Json.decodeText encoded :: Result Text Content
        decoded `shouldBe` Result.Ok content

    describe "userWithAttachment" do
      it "creates message with correct data URL" do
        let msg = userWithAttachment "Extract text" "AQID" "application/pdf"
        msg.role `shouldBe` User
        case msg.content of
          MultiContent parts -> do
            Array.length parts `shouldBe` 2
          TextContent _ ->
            expectationFailure "Expected MultiContent"

      it "includes text prompt as first part" do
        let msg = userWithAttachment "My prompt" "data" "application/pdf"
        case msg.content of
          MultiContent parts -> do
            let firstPart = Array.first parts
            firstPart `shouldBe` Just (TextPart "My prompt")
          TextContent _ ->
            expectationFailure "Expected MultiContent"

      it "includes image URL with data URI as second part" do
        let msg = userWithAttachment "prompt" "AQID" "application/pdf"
        case msg.content of
          MultiContent parts -> do
            let secondPart = parts |> Array.drop 1 |> Array.first
            secondPart `shouldBe` Just (ImageUrlPart ImageUrl {url = "data:application/pdf;base64,AQID"})
          TextContent _ ->
            expectationFailure "Expected MultiContent"

      it "handles empty prompt" do
        let msg = userWithAttachment "" "AQID" "application/pdf"
        msg.role `shouldBe` User
        case msg.content of
          MultiContent parts -> do
            Array.length parts `shouldBe` 2
            Array.first parts `shouldBe` Just (TextPart "")
          TextContent _ ->
            expectationFailure "Expected MultiContent"

      it "handles empty base64 data" do
        let msg = userWithAttachment "prompt" "" "application/pdf"
        msg.role `shouldBe` User
        case msg.content of
          MultiContent parts -> do
            let secondPart = parts |> Array.drop 1 |> Array.first
            secondPart `shouldBe` Just (ImageUrlPart ImageUrl {url = "data:application/pdf;base64,"})
          TextContent _ ->
            expectationFailure "Expected MultiContent"

      it "handles empty MIME type" do
        let msg = userWithAttachment "prompt" "AQID" ""
        msg.role `shouldBe` User
        case msg.content of
          MultiContent parts -> do
            let secondPart = parts |> Array.drop 1 |> Array.first
            secondPart `shouldBe` Just (ImageUrlPart ImageUrl {url = "data:;base64,AQID"})
          TextContent _ ->
            expectationFailure "Expected MultiContent"
