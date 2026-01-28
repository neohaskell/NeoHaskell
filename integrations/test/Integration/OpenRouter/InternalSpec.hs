module Integration.OpenRouter.InternalSpec (spec) where

import Array qualified
import Basics
import Integration.Http qualified as Http
import Integration.OpenRouter.Internal
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Request (Request (..))
import Integration.OpenRouter.Request qualified as OpenRouter
import Json qualified
import Maybe (Maybe (..))
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "toHttpRequest" do
    it "creates POST request to OpenRouter API" do
      let request = makeTestRequest
      let httpRequest = toHttpRequest request
      httpRequest.method `shouldBe` Http.POST

    it "sets correct URL" do
      let request = makeTestRequest
      let httpRequest = toHttpRequest request
      httpRequest.url `shouldBe` "https://openrouter.ai/api/v1/chat/completions"

    it "uses Bearer auth with OPENROUTER_API_KEY placeholder" do
      let request = makeTestRequest
      let httpRequest = toHttpRequest request
      httpRequest.auth `shouldBe` Http.Bearer "${OPENROUTER_API_KEY}"

    it "disables retry for LLM calls" do
      let request = makeTestRequest
      let httpRequest = toHttpRequest request
      httpRequest.retry `shouldBe` Http.noRetry

    it "uses timeout from config" do
      let request = makeTestRequest {config = OpenRouter.defaultConfig {OpenRouter.timeoutSeconds = 120}}
      let httpRequest = toHttpRequest request
      httpRequest.timeoutSeconds `shouldBe` 120

  describe "buildHeaders" do
    it "includes Referer header when set" do
      let config = OpenRouter.defaultConfig {OpenRouter.referer = Just "https://myapp.com"}
      let headers = buildHeaders config
      Array.contains ("HTTP-Referer", "https://myapp.com") headers `shouldBe` True

    it "includes X-Title header when set" do
      let config = OpenRouter.defaultConfig {OpenRouter.title = Just "My App"}
      let headers = buildHeaders config
      Array.contains ("X-Title", "My App") headers `shouldBe` True

    it "returns empty array when no optional headers set" do
      let headers = buildHeaders OpenRouter.defaultConfig
      Array.isEmpty headers `shouldBe` True

  describe "RequestBody JSON encoding" do
    it "encodes required fields" do
      let body =
            RequestBody
              { messages = [Message.user "Hello"]
              , model = "test-model"
              , stream = False
              , temperature = Nothing
              , max_tokens = Nothing
              , top_p = Nothing
              , frequency_penalty = Nothing
              , presence_penalty = Nothing
              }
      let encoded = Json.encodeText body
      Text.contains "\"model\":\"test-model\"" encoded `shouldBe` True
      Text.contains "\"stream\":false" encoded `shouldBe` True

    it "includes optional fields when present" do
      let body =
            RequestBody
              { messages = [Message.user "Hello"]
              , model = "test-model"
              , stream = False
              , temperature = Just 0.7
              , max_tokens = Just 100
              , top_p = Nothing
              , frequency_penalty = Nothing
              , presence_penalty = Nothing
              }
      let encoded = Json.encodeText body
      Text.contains "\"temperature\"" encoded `shouldBe` True
      Text.contains "\"max_tokens\"" encoded `shouldBe` True


-- Test helpers

makeTestRequest :: Request Text
makeTestRequest =
  Request
    { messages = [Message.user "Hello"]
    , model = "anthropic/claude-3.5-sonnet"
    , config = OpenRouter.defaultConfig
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }
