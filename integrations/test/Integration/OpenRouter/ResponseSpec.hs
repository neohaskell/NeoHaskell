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

    describe "Choice" do
      it "decodes choice from JSON" do
        let jsonText =
              "{\"message\":{\"role\":\"assistant\",\"content\":\"Hello!\"},\"finish_reason\":\"stop\",\"index\":0}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Choice
        case decoded of
          Result.Ok (choice :: Choice) -> do
            choice.message.role `shouldBe` Message.Assistant
            choice.message.content `shouldBe` "Hello!"
            choice.finishReason `shouldBe` Stop
            choice.index `shouldBe` 0
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

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
