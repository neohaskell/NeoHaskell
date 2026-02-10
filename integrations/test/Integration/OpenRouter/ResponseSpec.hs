module Integration.OpenRouter.ResponseSpec (spec) where

import Array qualified
import Basics
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response
import Json qualified
import Maybe (Maybe (..))
import Result (Result)
import Result qualified
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "Response" do
    describe "FinishReason" do
      it "decodes stop reason" do
        (Json.decodeText "\"stop\"" :: Result Text FinishReason) `shouldBe` Result.Ok Stop

      it "decodes length reason" do
        (Json.decodeText "\"length\"" :: Result Text FinishReason) `shouldBe` Result.Ok Length

      it "decodes content_filter reason" do
        (Json.decodeText "\"content_filter\"" :: Result Text FinishReason) `shouldBe` Result.Ok ContentFilter

      it "decodes unknown reasons as Unknown variant" do
        let decoded = Json.decodeText "\"something_else\"" :: Result Text FinishReason
        case decoded of
          Result.Ok (Unknown txt) -> txt `shouldBe` "something_else"
          other -> expectationFailure (Text.toLinkedList [fmt|Expected Unknown, got #{other}|])

      describe "JSON encoding" do
        it "encodes Stop to correct JSON string" do
          Json.encodeText Stop `shouldBe` "\"stop\""

        it "encodes Length to correct JSON string" do
          Json.encodeText Length `shouldBe` "\"length\""

        it "encodes ContentFilter to correct JSON string" do
          Json.encodeText ContentFilter `shouldBe` "\"content_filter\""

        it "encodes Unknown to correct JSON string" do
          Json.encodeText (Unknown "custom_reason") `shouldBe` "\"custom_reason\""

      describe "JSON round-trip" do
        it "round-trips Stop" do
          let original = Stop
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text FinishReason
          decoded `shouldBe` Result.Ok original

        it "round-trips Length" do
          let original = Length
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text FinishReason
          decoded `shouldBe` Result.Ok original

        it "round-trips ContentFilter" do
          let original = ContentFilter
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text FinishReason
          decoded `shouldBe` Result.Ok original

        it "round-trips Unknown" do
          let original = Unknown "custom_reason"
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text FinishReason
          decoded `shouldBe` Result.Ok original

    describe "Usage" do
      it "decodes usage stats from JSON" do
        let jsonText = "{\"prompt_tokens\":10,\"completion_tokens\":20,\"total_tokens\":30}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Usage
        case decoded of
          Result.Ok usage -> do
            usage.promptTokens `shouldBe` 10
            usage.completionTokens `shouldBe` 20
            usage.totalTokens `shouldBe` 30
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      describe "JSON encoding" do
        it "encodes Usage to JSON object with correct fields" do
          let usage = Usage {promptTokens = 10, completionTokens = 20, totalTokens = 30}
          let encoded = Json.encodeText usage
          Text.contains "\"prompt_tokens\":10" encoded `shouldBe` True
          Text.contains "\"completion_tokens\":20" encoded `shouldBe` True
          Text.contains "\"total_tokens\":30" encoded `shouldBe` True

      describe "JSON round-trip" do
        it "round-trips Usage" do
          let original = Usage {promptTokens = 10, completionTokens = 20, totalTokens = 30}
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Usage
          decoded `shouldBe` Result.Ok original

    describe "Choice" do
      it "decodes choice from JSON" do
        let jsonText =
              "{\"message\":{\"role\":\"assistant\",\"content\":\"Hello!\"},\"finish_reason\":\"stop\",\"index\":0}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Choice
        case decoded of
          Result.Ok (choice :: Choice) -> do
            choice.message.role `shouldBe` Message.Assistant
            choice.message.content `shouldBe` Message.TextContent "Hello!"
            choice.finishReason `shouldBe` Stop
            choice.index `shouldBe` 0
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      describe "JSON encoding" do
        it "encodes Choice to JSON object with correct fields" do
          let choice = Choice
                { message = Message.assistant "Hello!"
                , finishReason = Stop
                , index = 0
                }
          let encoded = Json.encodeText choice
          Text.contains "\"message\"" encoded `shouldBe` True
          Text.contains "\"finish_reason\"" encoded `shouldBe` True
          Text.contains "\"index\":0" encoded `shouldBe` True

      describe "JSON round-trip" do
        it "round-trips Choice" do
          let original = Choice
                { message = Message.assistant "Hello!"
                , finishReason = Stop
                , index = 0
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Choice
          decoded `shouldBe` Result.Ok original

    describe "Response" do
      it "decodes full response from JSON" do
        let jsonText =
              "{\"id\":\"gen-123\",\"model\":\"claude-3.5-sonnet\",\"choices\":[{\"message\":{\"role\":\"assistant\",\"content\":\"Hi\"},\"finish_reason\":\"stop\",\"index\":0}],\"usage\":{\"prompt_tokens\":5,\"completion_tokens\":10,\"total_tokens\":15}}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Response
        case decoded of
          Result.Ok (response :: Response) -> do
            response.id `shouldBe` "gen-123"
            response.model `shouldBe` "claude-3.5-sonnet"
            Array.length response.choices `shouldBe` 1
            case response.usage of
              Just (usage :: Usage) -> usage.totalTokens `shouldBe` (15 :: Int)
              Nothing -> expectationFailure "Expected usage to be present"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      describe "JSON encoding" do
        it "encodes Response to JSON object with correct fields" do
          let choice = Choice
                { message = Message.assistant "Hi"
                , finishReason = Stop
                , index = 0
                }
          let usage = Usage {promptTokens = 5, completionTokens = 10, totalTokens = 15}
          let response = Response
                { id = "gen-123"
                , model = "claude-3.5-sonnet"
                , choices = Array.fromLinkedList [choice]
                , usage = Maybe.Just usage
                }
          let encoded = Json.encodeText response
          Text.contains "\"id\":\"gen-123\"" encoded `shouldBe` True
          Text.contains "\"model\":\"claude-3.5-sonnet\"" encoded `shouldBe` True
          Text.contains "\"choices\"" encoded `shouldBe` True
          Text.contains "\"usage\"" encoded `shouldBe` True

      describe "JSON round-trip" do
        it "round-trips Response with usage" do
          let choice = Choice
                { message = Message.assistant "Hi"
                , finishReason = Stop
                , index = 0
                }
          let usage = Usage {promptTokens = 5, completionTokens = 10, totalTokens = 15}
          let original = Response
                { id = "gen-123"
                , model = "claude-3.5-sonnet"
                , choices = Array.fromLinkedList [choice]
                , usage = Maybe.Just usage
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Response
          decoded `shouldBe` Result.Ok original

        it "round-trips Response without usage" do
          let choice = Choice
                { message = Message.assistant "Hi"
                , finishReason = Stop
                , index = 0
                }
          let original = Response
                { id = "gen-456"
                , model = "gpt-4"
                , choices = Array.fromLinkedList [choice]
                , usage = Maybe.Nothing
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Response
          decoded `shouldBe` Result.Ok original
