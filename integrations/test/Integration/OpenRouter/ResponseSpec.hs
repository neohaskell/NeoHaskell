module Integration.OpenRouter.ResponseSpec (spec) where

import Array qualified
import Basics
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
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

    describe "ToolCallFunction" do
      it "constructs with name and redacted arguments" do
        let tcf = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c1\",\"quantity\":2}" :: Text)
              }
        tcf.name `shouldBe` "AddItem"

      it "Eq compares by unwrapped arguments" do
        let tcf1 = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c1\"}" :: Text)
              }
        let tcf2 = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c1\"}" :: Text)
              }
        (tcf1 == tcf2) `shouldBe` True

      it "Eq distinguishes different arguments" do
        let tcf1 = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c1\"}" :: Text)
              }
        let tcf2 = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c2\"}" :: Text)
              }
        (tcf1 == tcf2) `shouldBe` False

      it "decodes from JSON" do
        let jsonText = "{\"name\":\"AddItem\",\"arguments\":\"{\\\"cartId\\\":\\\"c1\\\"}\"}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text ToolCallFunction
        case decoded of
          Result.Ok tcf -> tcf.name `shouldBe` "AddItem"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      it "encodes to JSON with unwrapped arguments" do
        let tcf = ToolCallFunction
              { name = "AddItem"
              , arguments = Redacted.wrap ("{\"cartId\":\"c1\"}" :: Text)
              }
        let encoded = Json.encodeText tcf
        Text.contains "\"name\":\"AddItem\"" encoded `shouldBe` True
        Text.contains "\"arguments\"" encoded `shouldBe` True

    describe "ToolCall" do
      it "constructs with id and function" do
        let tc = ToolCall
              { id = "call_abc123"
              , function = ToolCallFunction
                  { name = "AddItem"
                  , arguments = Redacted.wrap ("{}" :: Text)
                  }
              }
        tc.id `shouldBe` "call_abc123"
        tc.function.name `shouldBe` "AddItem"

      it "Eq compares id and function" do
        let tc1 = ToolCall
              { id = "call_abc123"
              , function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) }
              }
        let tc2 = ToolCall
              { id = "call_abc123"
              , function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) }
              }
        (tc1 == tc2) `shouldBe` True

      it "decodes from JSON" do
        let jsonText = "{\"id\":\"call_abc123\",\"function\":{\"name\":\"AddItem\",\"arguments\":\"{}\"}}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text ToolCall
        case decoded of
          Result.Ok tc -> do
            tc.id `shouldBe` "call_abc123"
            tc.function.name `shouldBe` "AddItem"
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      it "encodes to JSON with id and function" do
        let tc = ToolCall
              { id = "call_abc123"
              , function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) }
              }
        let encoded = Json.encodeText tc
        Text.contains "\"id\":\"call_abc123\"" encoded `shouldBe` True
        Text.contains "\"function\"" encoded `shouldBe` True

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

      it "defaults toolCalls to empty array when absent from JSON" do
        let jsonText =
              "{\"message\":{\"role\":\"assistant\",\"content\":\"Hello!\"},\"finish_reason\":\"stop\",\"index\":0}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Choice
        case decoded of
          Result.Ok choice ->
            Array.length choice.toolCalls `shouldBe` 0
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      it "decodes tool_calls field when present" do
        let jsonText =
              "{\"message\":{\"role\":\"assistant\",\"content\":null},\"finish_reason\":\"tool_calls\",\"index\":0,\"tool_calls\":[{\"id\":\"call_abc\",\"function\":{\"name\":\"AddItem\",\"arguments\":\"{}\"}}]}" :: Text
        let decoded = Json.decodeText jsonText :: Result Text Choice
        case decoded of
          Result.Ok choice ->
            Array.length choice.toolCalls `shouldBe` 1
          Result.Err err ->
            expectationFailure (Text.toLinkedList [fmt|Failed to decode: #{err}|])

      describe "JSON encoding" do
        it "encodes Choice to JSON object with correct fields" do
          let choice = Choice
                { message = Message.assistant "Hello!"
                , finishReason = Stop
                , index = 0
                , toolCalls = Array.fromLinkedList []
                }
          let encoded = Json.encodeText choice
          Text.contains "\"message\"" encoded `shouldBe` True
          Text.contains "\"finish_reason\"" encoded `shouldBe` True
          Text.contains "\"index\":0" encoded `shouldBe` True

      describe "JSON round-trip" do
        it "round-trips Choice without tool calls" do
          let original = Choice
                { message = Message.assistant "Hello!"
                , finishReason = Stop
                , index = 0
                , toolCalls = Array.fromLinkedList []
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Choice
          case decoded of
            Result.Ok choice -> do
              choice.message.role `shouldBe` original.message.role
              choice.finishReason `shouldBe` original.finishReason
              choice.index `shouldBe` original.index
              Array.length choice.toolCalls `shouldBe` Array.length original.toolCalls
            Result.Err err ->
              expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])

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
                , toolCalls = Array.fromLinkedList []
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
                , toolCalls = Array.fromLinkedList []
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
          case decoded of
            Result.Ok response -> do
              response.id `shouldBe` original.id
              response.model `shouldBe` original.model
              Array.length response.choices `shouldBe` Array.length original.choices
            Result.Err err ->
              expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])

        it "round-trips Response without usage" do
          let choice = Choice
                { message = Message.assistant "Hi"
                , finishReason = Stop
                , index = 0
                , toolCalls = Array.fromLinkedList []
                }
          let original = Response
                { id = "gen-456"
                , model = "gpt-4"
                , choices = Array.fromLinkedList [choice]
                , usage = Maybe.Nothing
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Response
          case decoded of
            Result.Ok response -> do
              response.id `shouldBe` original.id
              response.model `shouldBe` original.model
              response.usage `shouldBe` Maybe.Nothing
            Result.Err err ->
              expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])

        it "round-trips Response with tool calls in choices" do
          let tc = ToolCall
                { id = "call_abc123"
                , function = ToolCallFunction { name = "AddItem", arguments = Redacted.wrap ("{}" :: Text) }
                }
          let choice = Choice
                { message = Message.assistant ""
                , finishReason = Stop
                , index = 0
                , toolCalls = Array.fromLinkedList [tc]
                }
          let original = Response
                { id = "gen-789"
                , model = "claude-3.5-sonnet"
                , choices = Array.fromLinkedList [choice]
                , usage = Maybe.Nothing
                }
          let encoded = Json.encodeText original
          let decoded = Json.decodeText encoded :: Result Text Response
          case decoded of
            Result.Ok response -> do
              response.id `shouldBe` original.id
              Array.length response.choices `shouldBe` 1
            Result.Err err ->
              expectationFailure (Text.toLinkedList [fmt|Expected Ok, got: #{err}|])
