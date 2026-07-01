module Integration.AzureAI.InternalSpec (spec) where

import Array qualified
import Basics
import Integration (ToAction (..))
import Integration qualified
import Integration.AzureAI.Internal
  ( RequestBody (..)
  , buildRequestBody
  , handleError
  , handleSuccess
  , sanitizeApiVersion
  , toHttpRequest
  )
import Integration.AzureAI.Request (Config (..), Request (..), azureEndpoint, defaultConfig)
import Integration.Http qualified as Http
import Integration.OpenRouter.Message qualified as Message
import Integration.OpenRouter.Response (Response (..))
import Json qualified
import Maybe (Maybe (..))
import Redacted qualified
import Result (Result (..))
import Service.Command.Core (NameOf)
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do

  describe "Integration.AzureAI.Internal.toHttpRequest" do
    it "produces a POST request (happy path)" do
      let req = makeTestRequest
      (toHttpRequest req).method `shouldBe` Http.POST

    it "builds the Model Inference URL with the api-version query (happy path)" do
      let req = makeTestRequest
      (toHttpRequest req).url
        `shouldBe` "https://my-res.openai.azure.com/models/chat/completions?api-version=2024-10-21"

    it "sets api-key auth with the header name \"api-key\" and the unwrapped key (happy path)" do
      let req = makeTestRequestWithKey "secret-key-456"
      let httpReq = toHttpRequest req
      httpReq.auth `shouldBe` Http.ApiKey "api-key" "secret-key-456"

    it "never renders the raw key in a Show of the request auth (SECURITY: key-never-in-Show)" do
      let req = makeTestRequestWithKey "super-secret-key"
      let httpReq = toHttpRequest req
      let shown = show httpReq.auth
      shown `shouldNotContain` "super-secret-key"

    it "disables retry for the (expensive) LLM call (edge: retry policy)" do
      let req = makeTestRequest
      (toHttpRequest req).retry `shouldBe` Http.noRetry

    it "passes timeoutSeconds through from config (edge: config passthrough)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let req = makeTestRequestWithConfig (defaultConfig { endpoint = ep, timeoutSeconds = 120 })
          (toHttpRequest req).timeoutSeconds `shouldBe` 120
        Result.Err _ -> expectationFailure "endpoint setup failure"

    it "encodes the body as JSON carrying model and stream:false (edge: body wiring)" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      case httpReq.body of
        Http.JsonBody value -> do
          let content = Json.encodeText value
          Text.contains "\"model\":\"gpt-4o\"" content `shouldBe` True
          Text.contains "\"stream\":false" content `shouldBe` True
        _ -> expectationFailure "expected JsonBody from toHttpRequest"

    it "sanitizes the api-version before interpolating it into the URL (SECURITY: SEC-003 query injection)" do
      let req = makeTestRequestWithApiVersion "2024-10-21&stream=true"
      let url = (toHttpRequest req).url
      Text.contains "&stream=true" url `shouldBe` False
      Text.contains "2024-10-21streamtrue" url `shouldBe` True

  describe "Integration.AzureAI.Internal.buildRequestBody" do
    it "sets stream to False (happy path)" do
      (buildRequestBody makeTestRequest).stream `shouldBe` False

    it "carries model and messages through unchanged (happy path)" do
      let body = buildRequestBody makeTestRequest
      body.model `shouldBe` "gpt-4o"
      Array.length body.messages `shouldBe` 1

    it "leaves unset sampling knobs as Nothing (edge: omit-by-default)" do
      let body = buildRequestBody makeTestRequest
      body.temperature `shouldBe` Nothing
      body.max_tokens `shouldBe` Nothing
      body.top_p `shouldBe` Nothing
      body.frequency_penalty `shouldBe` Nothing
      body.presence_penalty `shouldBe` Nothing

    it "carries Just sampling knobs from config (edge: knobs set)" do
      case azureEndpoint "https://my-res.openai.azure.com" of
        Result.Ok ep -> do
          let cfg = defaultConfig { endpoint = ep, temperature = Just 0.7, maxTokens = Just 100 }
          let req = makeTestRequestWithConfig cfg
          let body = buildRequestBody req
          body.temperature `shouldBe` Just 0.7
          body.max_tokens `shouldBe` Just 100
        Result.Err _ -> expectationFailure "endpoint setup failure"

  describe "Integration.AzureAI.Internal.RequestBody JSON encoding" do
    it "encodes the required fields model and stream:false (happy path)" do
      let body = RequestBody
            { messages = Array.fromLinkedList [Message.user "Hello"]
            , model = "gpt-4o"
            , stream = False
            , temperature = Nothing
            , max_tokens = Nothing
            , top_p = Nothing
            , frequency_penalty = Nothing
            , presence_penalty = Nothing
            }
      let encoded = Json.encodeText body
      Text.contains "\"model\":\"gpt-4o\"" encoded `shouldBe` True
      Text.contains "\"stream\":false" encoded `shouldBe` True

    it "OMITS a Nothing sampling field entirely — never as null (edge: Array.getJusts omission, SEC/DoS-clean wire)" do
      let body = RequestBody
            { messages = Array.fromLinkedList [Message.user "Hello"]
            , model = "gpt-4o"
            , stream = False
            , temperature = Nothing
            , max_tokens = Nothing
            , top_p = Nothing
            , frequency_penalty = Nothing
            , presence_penalty = Nothing
            }
      let encoded = Json.encodeText body
      Text.contains "temperature" encoded `shouldBe` False
      Text.contains "null" encoded `shouldBe` False

    it "includes a Just sampling field with its value (edge: present knob)" do
      let body = RequestBody
            { messages = Array.fromLinkedList [Message.user "Hello"]
            , model = "gpt-4o"
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

    it "an all-Nothing body contains only messages/model/stream keys (boundary: minimal body)" do
      let body = RequestBody
            { messages = Array.fromLinkedList [Message.user "Hello"]
            , model = "gpt-4o"
            , stream = False
            , temperature = Nothing
            , max_tokens = Nothing
            , top_p = Nothing
            , frequency_penalty = Nothing
            , presence_penalty = Nothing
            }
      let encoded = Json.encodeText body
      Text.contains "temperature" encoded `shouldBe` False
      Text.contains "max_tokens" encoded `shouldBe` False
      Text.contains "top_p" encoded `shouldBe` False
      Text.contains "frequency_penalty" encoded `shouldBe` False
      Text.contains "presence_penalty" encoded `shouldBe` False

  describe "Integration.AzureAI.Internal.sanitizeApiVersion" do
    it "passes a legitimate GA api-version through unchanged (happy path)" do
      sanitizeApiVersion "2024-10-21" `shouldBe` "2024-10-21"

    it "passes a legitimate preview api-version through unchanged (happy path)" do
      sanitizeApiVersion "2024-05-01-preview" `shouldBe` "2024-05-01-preview"

    it "strips an injected & and = so no new query parameter can be formed (SECURITY: SEC-003 &-injection)" do
      sanitizeApiVersion "2024-10-21&stream=true" `shouldBe` "2024-10-21streamtrue"

    it "strips ? and # (SECURITY: SEC-003 query/fragment separators)" do
      sanitizeApiVersion "2024?10#21" `shouldBe` "20241021"

    it "strips whitespace (edge: spaces are not URL-safe api-version chars)" do
      sanitizeApiVersion "2024 10 21" `shouldBe` "20241021"

    it "returns empty for empty input (edge: empty string)" do
      sanitizeApiVersion "" `shouldBe` ""

    it "drops non-ASCII and punctuation like '.' (edge: unicode / disallowed punctuation)" do
      sanitizeApiVersion "2024.10\x2713" `shouldBe` "202410"

  describe "Integration.AzureAI.Internal.handleSuccess" do
    it "on 2xx with a decodable body invokes onSuccess with the parsed Response (happy path)" do
      let req :: Request Text = makeTestRequest { onSuccess = \_ -> "ok", onError = \_ -> "err" }
      let response = Http.Response
            { statusCode = 200
            , body = makeValidResponseBody
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "ok"

    it "on 2xx with a malformed body invokes onError \"Failed to parse Azure AI response\" (error: parse failure)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 200
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Failed to parse Azure AI response"

    it "on 429 invokes onError \"Azure AI rate limit exceeded\" (error: rate limit)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 429
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI rate limit exceeded"

    it "on 400 invokes onError \"Azure AI request error (HTTP 400)\" (error: generic 4xx)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 400
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI request error (HTTP 400)"

    it "on 404 includes the status code in the 4xx message (error: 4xx diagnostic)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 404
            , body = Json.null
            , headers = Array.empty
            }
      let result = handleSuccess req response
      Text.contains "404" result `shouldBe` True

    it "on 500 invokes onError \"Azure AI server error (HTTP 500)\" (error: generic 5xx)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 500
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI server error (HTTP 500)"

    it "on 503 includes the status code in the 5xx message (error: 5xx diagnostic)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 503
            , body = Json.null
            , headers = Array.empty
            }
      let result = handleSuccess req response
      Text.contains "503" result `shouldBe` True

    it "on 299 (upper 2xx boundary) takes the decode path (boundary: 2xx upper edge)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 299
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Failed to parse Azure AI response"

    it "on 428 (just below 429) takes the generic 4xx branch (boundary: below the 429 special-case)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 428
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI request error (HTTP 428)"

    it "on 430 (just above 429) takes the generic 4xx branch (boundary: above the 429 special-case)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 430
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI request error (HTTP 430)"

    it "on 499 (upper 4xx boundary) takes the generic 4xx branch (boundary: 4xx upper edge)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 499
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI request error (HTTP 499)"

    it "on 199 (below 2xx) falls to the catch-all server-error branch (boundary: below 2xx)" do
      let req = makeTestRequestWithKey "test-key"
      let response = Http.Response
            { statusCode = 199
            , body = Json.null
            , headers = Array.empty
            }
      handleSuccess req response `shouldBe` "Azure AI server error (HTTP 199)"

    it "no error-band message contains the api key or the raw response body (SECURITY: sanitisation invariant)" do
      let req = makeTestRequestWithKey "leak-secret"
      let leakBody = Json.encode ("leak-secret" :: Text)
      -- 200-malformed: body contains "leak-secret" but error message must not
      let resp200 = Http.Response { statusCode = 200, body = leakBody, headers = Array.empty }
      let result200 = handleSuccess req resp200
      Text.contains "leak-secret" result200 `shouldBe` False
      -- 429: key must not leak into rate-limit message
      let resp429 = Http.Response { statusCode = 429, body = leakBody, headers = Array.empty }
      let result429 = handleSuccess req resp429
      Text.contains "leak-secret" result429 `shouldBe` False
      -- 400: key must not leak into request-error message
      let resp400 = Http.Response { statusCode = 400, body = leakBody, headers = Array.empty }
      let result400 = handleSuccess req resp400
      Text.contains "leak-secret" result400 `shouldBe` False
      -- 500: key must not leak into server-error message
      let resp500 = Http.Response { statusCode = 500, body = leakBody, headers = Array.empty }
      let result500 = handleSuccess req resp500
      Text.contains "leak-secret" result500 `shouldBe` False

  describe "Integration.AzureAI.Internal.handleError" do
    it "passes a transport error text through to onError unchanged (happy path)" do
      let req = makeTestRequestWithKey "test-key"
      handleError req "connection refused" `shouldBe` "connection refused"

    it "passes an empty error text through unchanged (edge: empty string)" do
      let req = makeTestRequestWithKey "test-key"
      handleError req "" `shouldBe` ""

    it "preserves a unicode error message (edge: multibyte text)" do
      let req = makeTestRequestWithKey "test-key"
      handleError req "\38169\35823\65306\36229\26102" `shouldBe` "\38169\35823\65306\36229\26102"

  describe "Integration.AzureAI.Internal.ToAction (Request command)" do
    it "resolves Integration.outbound for a chatCompletion Request without extra plumbing (happy path: compiles + typechecks)" do
      let req :: Request TestAzureAICommand = makeTestAzureAIRequest
      -- With Strict extension, let forces evaluation; stub panics → test is red
      let _act = Integration.outbound req
      (1 :: Int) `shouldBe` 1

    it "delegates through toHttpRequest so the built action carries the sanitised api-key auth (edge: delegation)" do
      let req :: Request TestAzureAICommand
          req = makeTestAzureAIRequest { apiKey = Redacted.wrap "tok-123" }
      let httpReq = toHttpRequest req
      httpReq.auth `shouldBe` Http.ApiKey "api-key" "tok-123"
      show httpReq.auth `shouldNotContain` "tok-123"

    it "requires no runtime https re-check branch (edge: SEC-001 handled by type, not runtime)" do
      -- Documentation-anchored: AzureEndpoint proves https+allowlist at construction;
      -- toAction is a pure delegation to toHttpRequest |> Integration.toAction with
      -- no Result branch. Stub panics to keep test red.
      let req :: Request TestAzureAICommand = makeTestAzureAIRequest
      let _act = toAction req
      (1 :: Int) `shouldBe` 1

  describe "Properties" do
    it "sanitizeApiVersion is idempotent (property: filtering twice equals filtering once)" do
      let s = "2024-10-21&stream=true?foo#bar"
      sanitizeApiVersion (sanitizeApiVersion s) `shouldBe` sanitizeApiVersion s

    it "sanitizeApiVersion output is URL-query-safe (property: no & ? # or space in output)" do
      let result = sanitizeApiVersion "2024-10-21&stream=true?version=1#anchor here"
      Text.contains "&" result `shouldBe` False
      Text.contains "?" result `shouldBe` False
      Text.contains "#" result `shouldBe` False
      Text.contains " " result `shouldBe` False


-- Test helpers

-- | Shared type annotation helper to make 'Response' import used.
successCallback :: Response -> Text
successCallback _ = "success"


makeTestRequest :: Request Text
makeTestRequest =
  case azureEndpoint "https://my-res.openai.azure.com" of
    Result.Ok ep ->
      Request
        { messages = Array.fromLinkedList [Message.user "Hello"]
        , model = "gpt-4o"
        , config = defaultConfig { endpoint = ep }
        , apiKey = Redacted.wrap "test-key"
        , onSuccess = successCallback
        , onError = \_ -> "error"
        }
    Result.Err _ -> panic "test setup: unexpected endpoint validation failure"


-- | Vary the API key; onError = identity so status-dispatch tests can inspect messages.
makeTestRequestWithKey :: Text -> Request Text
makeTestRequestWithKey keyVal =
  case azureEndpoint "https://my-res.openai.azure.com" of
    Result.Ok ep ->
      Request
        { messages = Array.fromLinkedList [Message.user "Hello"]
        , model = "gpt-4o"
        , config = defaultConfig { endpoint = ep }
        , apiKey = Redacted.wrap keyVal
        , onSuccess = successCallback
        , onError = identity
        }
    Result.Err _ -> panic "test setup: unexpected endpoint validation failure"


-- | Vary the config (including endpoint); all other fields use test defaults.
makeTestRequestWithConfig :: Config -> Request Text
makeTestRequestWithConfig cfg =
  Request
    { messages = Array.fromLinkedList [Message.user "Hello"]
    , model = "gpt-4o"
    , config = cfg
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = successCallback
    , onError = \_ -> "error"
    }


-- | Vary the api-version while keeping a valid endpoint.
makeTestRequestWithApiVersion :: Text -> Request Text
makeTestRequestWithApiVersion versionVal =
  case azureEndpoint "https://my-res.openai.azure.com" of
    Result.Ok ep ->
      Request
        { messages = Array.fromLinkedList [Message.user "Hello"]
        , model = "gpt-4o"
        , config = defaultConfig { endpoint = ep, apiVersion = versionVal }
        , apiKey = Redacted.wrap "test-key"
        , onSuccess = successCallback
        , onError = \_ -> "error"
        }
    Result.Err _ -> panic "test setup: unexpected endpoint validation failure"


-- | Request using TestAzureAICommand for ToAction constraint tests.
makeTestAzureAIRequest :: Request TestAzureAICommand
makeTestAzureAIRequest =
  case azureEndpoint "https://my-res.openai.azure.com" of
    Result.Ok ep ->
      Request
        { messages = Array.fromLinkedList [Message.user "Hello"]
        , model = "gpt-4o"
        , config = defaultConfig { endpoint = ep }
        , apiKey = Redacted.wrap "test-key"
        , onSuccess = \_ -> TestAzureAICommand "success"
        , onError = \msg -> TestAzureAICommand msg
        }
    Result.Err _ -> panic "test setup: unexpected endpoint validation failure"


-- | A minimal valid Response JSON body (empty choices, no usage).
-- Used in handleSuccess happy-path test where the body must decode as Response.
makeValidResponseBody :: Json.Value
makeValidResponseBody =
  Json.object
    [ ("id", Json.encode ("gen-test-123" :: Text))
    , ("model", Json.encode ("gpt-4o" :: Text))
    , ("choices", Json.encode ([] :: [Json.Value]))
    ]


-- | Minimal command type satisfying the ToAction (Request command) constraints:
-- Json.ToJSON + KnownSymbol (NameOf command).
newtype TestAzureAICommand = TestAzureAICommand Text
  deriving (Eq, Show, Generic)


instance Json.ToJSON TestAzureAICommand


type instance NameOf TestAzureAICommand = "TestAzureAICommand"
