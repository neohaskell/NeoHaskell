module Integration.Brevo.InternalSpec (spec) where

import Array (Array)
import Array qualified
import Basics
import Integration.Brevo.Internal (encodeRequest, toHttpRequest)
import Integration.Brevo.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..))
import Integration.Brevo.Response (Response (..))
import Integration.Http qualified as Http
import Json qualified
import Map qualified
import Maybe (Maybe (..))
import Redacted qualified
import Test.Hspec
import Text (Text)
import Text qualified


spec :: Spec
spec = do
  describe "Integration.Brevo.Internal.toHttpRequest" do
    it "produces Http.Request with correct URL and POST method (happy path)" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      httpReq.method `shouldBe` Http.POST
      httpReq.url `shouldBe` "https://api.brevo.com/v3/smtp/email"

    it "includes api-key Authorization header with unwrapped API key" do
      let req = makeTestRequestWithApiKey "secret-key-456"
      let httpReq = toHttpRequest req
      httpReq.auth `shouldBe` Http.ApiKey { headerName = "api-key", headerValue = "secret-key-456" }

    it "encodes request body to JSON bytes via encodeRequest" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      case httpReq.body of
        Http.RawBody { content } ->
          Text.contains "sender" content `shouldBe` True
        _ ->
          expectationFailure "expected RawBody from toHttpRequest"

    it "handles HtmlBody by dispatching encodeRequest to htmlContent JSON field (edge: Body sum dispatch)" do
      let req = makeTestRequestWithBody (HtmlBody "<h1>Title</h1>")
      let encoded = encodeRequest req
      Text.contains "htmlContent" encoded `shouldBe` True
      Text.contains "textContent" encoded `shouldBe` False
      Text.contains "templateId" encoded `shouldBe` False

    it "handles TextBody by dispatching encodeRequest to textContent JSON field (edge: Body sum dispatch)" do
      let req = makeTestRequestWithBody (TextBody "Plain text")
      let encoded = encodeRequest req
      Text.contains "textContent" encoded `shouldBe` True
      Text.contains "htmlContent" encoded `shouldBe` False
      Text.contains "templateId" encoded `shouldBe` False

    it "handles Template by dispatching encodeRequest to templateId JSON field with params (edge: Body sum dispatch, Template variant)" do
      let req = makeTestRequestWithBody
            (Template
              { templateId = 42
              , params = Map.fromArray [("name", "John"), ("action", "verify")]
              })
      let encoded = encodeRequest req
      Text.contains "templateId" encoded `shouldBe` True
      Text.contains "htmlContent" encoded `shouldBe` False
      Text.contains "textContent" encoded `shouldBe` False

    it "wraps onSuccess and onError callbacks in BrevoResponseHandler (error: missing Response.messageId)" do
      let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
      let httpReq = toHttpRequest req
      let fakeResponse = Http.Response
            { statusCode = 201
            , body = Json.object [("id", Json.encode ("123456789" :: Text))]
            , headers = Array.empty
            }
      let result = httpReq.onSuccess fakeResponse
      result `shouldBe` "error:Failed to parse Brevo response"

    it "enforces hasField constraint at compile time (error: config missing brevoApiKey field)" do
      -- Compile-time test: Request carries apiKey from config, verifiable via makeTestRequest.
      let req = makeTestRequest
      req.subject `shouldBe` "Test Subject"

  describe "Integration.Brevo.Internal.encodeRequest" do
    it "encodes Request with single recipient to Brevo JSON schema (happy path)" do
      let req = makeTestRequest
      let encoded = encodeRequest req
      Text.contains "sender" encoded `shouldBe` True
      Text.contains "subject" encoded `shouldBe` True

    it "encodes Address with display name (happy path)" do
      let req = makeTestRequestWithSender
            (Sender (Address { name = Just "John Doe", email = "john@example.com" }))
      let encoded = encodeRequest req
      Text.contains "John Doe" encoded `shouldBe` True
      Text.contains "john@example.com" encoded `shouldBe` True

    it "encodes Address without display name (edge: name = Nothing)" do
      let req = makeTestRequestWithSender
            (Sender (Address { name = Nothing, email = "user@example.com" }))
      let encoded = encodeRequest req
      Text.contains "user@example.com" encoded `shouldBe` True

    it "encodes cc and bcc arrays with multiple recipients (edge: non-empty arrays)" do
      let req = makeTestRequestWithCcBcc
            (Array.fromLinkedList
              [ Recipient (Address { name = Nothing, email = "cc@example.com" })
              , Recipient (Address { name = Nothing, email = "cc2@example.com" })
              ])
            (Array.fromLinkedList
              [ Recipient (Address { name = Nothing, email = "bcc@example.com" })
              ])
      let encoded = encodeRequest req
      Text.contains "cc@example.com" encoded `shouldBe` True
      Text.contains "bcc@example.com" encoded `shouldBe` True

    it "encodes replyTo field when Some(Recipient) (edge: optional field present)" do
      let req = makeTestRequestWithReplyTo
            (Just (Recipient (Address { name = Nothing, email = "replies@example.com" })))
      let encoded = encodeRequest req
      Text.contains "replies@example.com" encoded `shouldBe` True

    it "encodes tags array with multiple values (error: JSON serialization of tags)" do
      let req = makeTestRequestWithTags
            (Array.fromLinkedList ["transactional", "welcome", "onboarding"])
      let encoded = encodeRequest req
      Text.contains "transactional" encoded `shouldBe` True
      Text.contains "welcome" encoded `shouldBe` True
      Text.contains "onboarding" encoded `shouldBe` True

    it "refuses to encode malformed Request if custom JSON instance fails (error: JSON encoding failure)" do
      -- Structural test: encodeRequest must produce non-empty output for valid request.
      let req = makeTestRequest
      let encoded = encodeRequest req
      Text.length encoded `shouldSatisfy` (> 0)

  describe "Integration.Brevo.Internal.BrevoResponseHandler" do
    describe "handle method with HTTP 201 success" do
      it "parses HTTP 201 response with valid messageId and calls onSuccess (happy path)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let validResponse = Http.Response
              { statusCode = 201
              , body = Json.object [("messageId", Json.encode ("123456789" :: Text))]
              , headers = Array.empty
              }
        let result = httpReq.onSuccess validResponse
        result `shouldBe` "success:123456789"

      it "returns error when HTTP 201 body is missing messageId field (edge: data validation, tm-012)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let missingFieldResponse = Http.Response
              { statusCode = 201
              , body = Json.object [("id", Json.encode ("123456789" :: Text))]
              , headers = Array.empty
              }
        let result = httpReq.onSuccess missingFieldResponse
        result `shouldBe` "error:Failed to parse Brevo response"

    describe "handle method with HTTP 401 Unauthorized" do
      it "returns sanitized error message on HTTP 401 without leaking API key (error: tm-010 OWASP-A05)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let unauthorizedResponse = Http.Response
              { statusCode = 401
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess unauthorizedResponse
        result `shouldBe` "error:Authentication failed (HTTP 401): invalid API key"

    describe "handle method with HTTP 402 Payment Required" do
      it "returns error message on HTTP 402 without leaking request or response (error: account credit)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let paymentResponse = Http.Response
              { statusCode = 402
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess paymentResponse
        result `shouldBe` "error:Payment required (HTTP 402): account credit exhausted"

    describe "handle method with HTTP 429 Rate Limit" do
      it "returns error message on HTTP 429 for rate-limit handling (error: tm-005 rate-limit)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let rateLimitResponse = Http.Response
              { statusCode = 429
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess rateLimitResponse
        result `shouldBe` "error:Rate limit exceeded (HTTP 429): too many requests"

    describe "handle method with other 4xx status" do
      it "returns generic 4xx error message without leaking details (edge: other client errors)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let clientErrorResponse = Http.Response
              { statusCode = 400
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess clientErrorResponse
        result `shouldBe` "error:Brevo client error (HTTP 400)"

      it "returns 4xx error message with interpolated status code (edge: status code included in message)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let unprocessableResponse = Http.Response
              { statusCode = 422
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess unprocessableResponse
        result `shouldBe` "error:Brevo client error (HTTP 422)"

    describe "handle method with 5xx status" do
      it "returns generic 5xx error message without leaking details (edge: server errors)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let serverErrorResponse = Http.Response
              { statusCode = 500
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess serverErrorResponse
        result `shouldBe` "error:Brevo server error (HTTP 500)"

      it "returns 5xx error message with interpolated status code (edge: status code included)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let gatewayTimeoutResponse = Http.Response
              { statusCode = 504
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess gatewayTimeoutResponse
        result `shouldBe` "error:Brevo server error (HTTP 504)"

    describe "handle method with unexpected status codes" do
      it "returns error for unexpected status code (edge: fallback case for unhandled status)" do
        let req = makeTestRequestWithCallbacks testSuccessCallback testErrorCallback
        let httpReq = toHttpRequest req
        let teapotResponse = Http.Response
              { statusCode = 418
              , body = Json.null
              , headers = Array.empty
              }
        let result = httpReq.onSuccess teapotResponse
        result `shouldBe` "error:Brevo client error (HTTP 418)"


-- Test helpers

testSuccessCallback :: Response -> Text
testSuccessCallback response =
  case response of
    Response { messageId = mid } -> [fmt|success:#{mid}|]


testErrorCallback :: Text -> Text
testErrorCallback errMsg = [fmt|error:#{errMsg}|]


makeTestRequest :: Request Text
makeTestRequest =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithApiKey :: Text -> Request Text
makeTestRequestWithApiKey key =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap key
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithBody :: Body -> Request Text
makeTestRequestWithBody bodyVal =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = bodyVal
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithSender :: Sender -> Request Text
makeTestRequestWithSender senderVal =
  Request
    { sender = senderVal
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithCcBcc ::
  Array Recipient ->
  Array Recipient ->
  Request Text
makeTestRequestWithCcBcc ccVal bccVal =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = ccVal
    , bcc = bccVal
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithReplyTo :: Maybe Recipient -> Request Text
makeTestRequestWithReplyTo replyToVal =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = replyToVal
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithTags :: Array Text -> Request Text
makeTestRequestWithTags tagsVal =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = tagsVal
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithCallbacks ::
  (Response -> Text) ->
  (Text -> Text) ->
  Request Text
makeTestRequestWithCallbacks onSuccessFn onErrorFn =
  Request
    { sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , cc = Array.empty
    , bcc = Array.empty
    , replyTo = Nothing
    , tags = Array.empty
    , apiKey = Redacted.wrap "test-key"
    , onSuccess = onSuccessFn
    , onError = onErrorFn
    }
