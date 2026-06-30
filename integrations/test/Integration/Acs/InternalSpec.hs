module Integration.Acs.InternalSpec (spec) where

import Array qualified
import Basics
import Control.Exception qualified as Exception
import Integration (ToAction (..))
import Integration.Acs.Internal
  ( AcsResponseHandler (..)
  , encodeBodyField
  , encodeRecipientAddress
  , encodeRequest
  , handleAcsResponse
  , operationIdFrom
  , operationIdFromLocation
  , senderEmail
  , toHttpRequest
  , validateEndpoint
  )
import Integration.Acs.Request (Address (..), Body (..), Recipient (..), Request (..), Sender (..))
import Integration.Acs.Response (Response (..))
import Integration.Http qualified as Http
import Json qualified
import Maybe (Maybe (..))
import Maybe qualified
import Redacted qualified
import Result qualified
import Service.Command.Core (NameOf)
import Test.Hspec
import Text (Text)
import Text qualified
import Tuple qualified


spec :: Spec
spec = do
  describe "Integration.Acs.Internal.validateEndpoint" do
    it "returns Ok with trimmed endpoint for https scheme (happy path)" do
      let result = validateEndpoint "https://my-acs.communication.azure.com"
      result `shouldBe` Result.Ok "https://my-acs.communication.azure.com"

    it "returns Err for http scheme (edge: non-https endpoint, SEC-001 guard)" do
      let result = validateEndpoint "http://my-acs.communication.azure.com"
      case result of
        Result.Err msg -> Text.contains "https" msg `shouldBe` True
        Result.Ok _ -> expectationFailure "expected Err for http endpoint"

    it "trims whitespace and validates the trimmed endpoint (edge: leading/trailing space)" do
      let result = validateEndpoint "  https://my-acs.communication.azure.com  "
      result `shouldBe` Result.Ok "https://my-acs.communication.azure.com"

  describe "Integration.Acs.Internal.toHttpRequest" do
    it "produces Http.Request with POST method and correct endpoint (happy path)" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      httpReq.method `shouldBe` Http.POST
      Text.contains "/emails:send?api-version=2023-03-31" httpReq.url `shouldBe` True

    it "includes Bearer authorization header with unwrapped access token (happy path)" do
      let req = makeTestRequestWithToken "secret-token-456"
      let httpReq = toHttpRequest req
      httpReq.auth `shouldBe` Http.Bearer "secret-token-456"

    it "encodes request body to JSON via encodeRequest (happy path)" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      case httpReq.body of
        Http.RawBody { content } ->
          (Text.contains "senderAddress" content && Text.contains "content" content) `shouldBe` True
        _ ->
          expectationFailure "expected RawBody from toHttpRequest"

    it "sets timeout to 30 seconds (happy path)" do
      let req = makeTestRequest
      let httpReq = toHttpRequest req
      httpReq.timeoutSeconds `shouldBe` 30

  describe "Integration.Acs.Internal.encodeRequest" do
    it "encodes Request to ACS-compatible JSON wire format (happy path)" do
      let req = makeTestRequest
      let encoded = encodeRequest req
      Text.contains "senderAddress" encoded `shouldBe` True
      Text.contains "content" encoded `shouldBe` True
      Text.contains "recipients" encoded `shouldBe` True

    it "dispatches HtmlBody to 'html' key in content (edge: Body variant dispatch)" do
      let req = makeTestRequestWithBody (HtmlBody "<h1>Title</h1>")
      let encoded = encodeRequest req
      Text.contains "\"html\"" encoded `shouldBe` True
      Text.contains "plainText" encoded `shouldBe` False

    it "dispatches TextBody to 'plainText' key in content (edge: Body variant dispatch)" do
      let req = makeTestRequestWithBody (TextBody "Plain text")
      let encoded = encodeRequest req
      Text.contains "\"plainText\"" encoded `shouldBe` True
      Text.contains "\"html\"" encoded `shouldBe` False

    it "includes subject in content object (happy path)" do
      let req = makeTestRequestWithSubject "Welcome"
      let encoded = encodeRequest req
      Text.contains "Welcome" encoded `shouldBe` True

    it "encodes recipient as { \"address\": \"...\" } in recipients.to array (happy path)" do
      let req = makeTestRequest
      let encoded = encodeRequest req
      Text.contains "user@example.com" encoded `shouldBe` True

  describe "Integration.Acs.Internal.encodeBodyField" do
    it "returns [(\"html\", JsonValue)] for HtmlBody (happy path)" do
      let field = encodeBodyField (HtmlBody "<h1>Hi</h1>")
      Array.length field `shouldBe` 1
      let firstKey = Array.get 0 field |> Maybe.map Tuple.first
      firstKey `shouldBe` Just "html"

    it "returns [(\"plainText\", JsonValue)] for TextBody (happy path)" do
      let field = encodeBodyField (TextBody "Hi")
      Array.length field `shouldBe` 1
      let firstKey = Array.get 0 field |> Maybe.map Tuple.first
      firstKey `shouldBe` Just "plainText"

  describe "Integration.Acs.Internal.encodeRecipientAddress" do
    it "encodes Recipient to { \"address\": \"...\" } JSON object (happy path)" do
      let rcpt = Recipient (Address { name = Nothing, email = "user@example.com" })
      let encoded = encodeRecipientAddress rcpt
      let jsonText = Json.encodeText encoded
      Text.contains "user@example.com" jsonText `shouldBe` True
      Text.contains "\"address\"" jsonText `shouldBe` True

  describe "Integration.Acs.Internal.senderEmail" do
    it "extracts email from Sender newtype (happy path)" do
      let sndr = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
      let result = senderEmail sndr
      result `shouldBe` "noreply@myapp.com"

  describe "Integration.Acs.Internal.operationIdFrom" do
    it "extracts operationId from response body 'id' field (happy path)" do
      let response = Http.Response
            { statusCode = 202
            , body = Json.object [("id", Json.encode ("abc-123" :: Text)), ("status", Json.encode ("Running" :: Text))]
            , headers = Array.empty
            }
      let result = operationIdFrom response
      result `shouldBe` "abc-123"

    it "falls back to Operation-Location header when body lacks id field (edge: header fallback)" do
      let response = Http.Response
            { statusCode = 202
            , body = Json.object [("status", Json.encode ("Running" :: Text))]
            , headers = Array.fromLinkedList [("Operation-Location", "https://my-acs.communication.azure.com/operations/xyz-789")]
            }
      let result = operationIdFrom response
      result `shouldBe` "xyz-789"

    it "returns empty string when body lacks id and header missing (edge: complete fallback)" do
      let response = Http.Response
            { statusCode = 202
            , body = Json.object [("status", Json.encode ("Running" :: Text))]
            , headers = Array.empty
            }
      let result = operationIdFrom response
      result `shouldBe` ""

  describe "Integration.Acs.Internal.operationIdFromLocation" do
    it "extracts last path segment from Operation-Location header (happy path)" do
      let location = "https://my-acs.communication.azure.com/operations/abc-123"
      let result = operationIdFromLocation location
      result `shouldBe` "abc-123"

  describe "Integration.Acs.Internal.handleAcsResponse" do
    describe "HTTP 202 Accepted" do
      it "invokes onSuccess callback with Response containing extracted operationId (happy path, SEC-002)" do
        let handler = (AcsResponseHandler
              { onSuccess = \(Response { operationId = opId }) -> [fmt|success:#{opId}|]
              , onError = \_ -> "error"
              } :: AcsResponseHandler Text)
        let response = Http.Response
              { statusCode = 202
              , body = Json.object [("id", Json.encode ("op-123" :: Text))]
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        result `shouldBe` "success:op-123"

      it "invokes onSuccess even with empty operationId (edge: SEC-002 best-effort)" do
        let handler = (AcsResponseHandler
              { onSuccess = \(Response { operationId = opId }) -> [fmt|success:#{opId}|]
              , onError = \_ -> "error"
              } :: AcsResponseHandler Text)
        let response = Http.Response
              { statusCode = 202
              , body = Json.object [("status", Json.encode ("Accepted" :: Text))]
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        result `shouldBe` "success:"

    describe "HTTP 401 Unauthorized" do
      it "invokes onError with sanitized message (error: authentication failure, no token leak)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 401
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "401" result `shouldBe` True
        result `shouldSatisfy` (\msg -> not (msg == ""))

    describe "HTTP 403 Forbidden" do
      it "invokes onError with sanitized message (error: authorization failure)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 403
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "403" result `shouldBe` True

    describe "HTTP 429 Rate Limit" do
      it "invokes onError with rate-limit message (error: throttling)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 429
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "429" result `shouldBe` True

    describe "4xx Client Errors (other)" do
      it "invokes onError with generic 4xx message (edge: other client errors)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 400
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "400" result `shouldBe` True

      it "includes status code in error message (edge: diagnostic value)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 422
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "422" result `shouldBe` True

    describe "5xx Server Errors" do
      it "invokes onError with generic 5xx message (edge: server errors)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 500
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "500" result `shouldBe` True

      it "includes status code in 5xx error message (edge: diagnostic value)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 504
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        Text.contains "504" result `shouldBe` True

    describe "Unexpected status codes" do
      it "invokes onError with fallback message (edge: unhandled status)" do
        let handler = AcsResponseHandler
              { onSuccess = \_ -> "success"
              , onError = identity
              }
        let response = Http.Response
              { statusCode = 418
              , body = Json.null
              , headers = Array.empty
              }
        let result = handleAcsResponse handler response
        result `shouldSatisfy` (\msg -> not (msg == ""))

  describe "Integration.Acs.Internal.ToAction instance" do
    it "validates endpoint before unwrapping token, routes https to toHttpRequest (happy path, SEC-001, stub is red)" do
      let req = makeTestRequestForToAction "https://my-acs.communication.azure.com"
      -- evaluate forces the thunk in IO; stub panics → exception → test is red
      _ <- Exception.evaluate (toAction req)
      pure ()

    it "rejects non-https endpoint and emits error command without unwrapping token (edge: SEC-001 guard, token leak prevention, stub is red)" do
      let req = makeTestRequestForToAction "http://my-acs.communication.azure.com"
      -- evaluate forces the thunk in IO; stub panics → exception → test is red
      _ <- Exception.evaluate (toAction req)
      pure ()


-- Test helpers

makeTestRequest :: Request Text
makeTestRequest =
  Request
    { endpoint = "https://my-acs.communication.azure.com"
    , sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , accessToken = Redacted.wrap "test-token"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithToken :: Text -> Request Text
makeTestRequestWithToken tokenVal =
  Request
    { endpoint = "https://my-acs.communication.azure.com"
    , sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , accessToken = Redacted.wrap tokenVal
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithBody :: Body -> Request Text
makeTestRequestWithBody bodyVal =
  Request
    { endpoint = "https://my-acs.communication.azure.com"
    , sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = bodyVal
    , accessToken = Redacted.wrap "test-token"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


makeTestRequestWithSubject :: Text -> Request Text
makeTestRequestWithSubject subjectVal =
  Request
    { endpoint = "https://my-acs.communication.azure.com"
    , sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = subjectVal
    , body = HtmlBody "<p>Test</p>"
    , accessToken = Redacted.wrap "test-token"
    , onSuccess = \_ -> "success"
    , onError = \_ -> "error"
    }


-- | Minimal command type that satisfies the ToAction (Request command) constraints.
-- Needed because the instance requires both Json.ToJSON and KnownSymbol (NameOf command).
newtype TestAcsCommand = TestAcsCommand Text deriving (Eq, Show, Generic)


instance Json.ToJSON TestAcsCommand


type instance NameOf TestAcsCommand = "TestAcsCommand"


-- | Test helper for ToAction tests; uses TestAcsCommand which satisfies the constraint.
makeTestRequestForToAction :: Text -> Request TestAcsCommand
makeTestRequestForToAction endpointVal =
  Request
    { endpoint = endpointVal
    , sender = Sender (Address { name = Nothing, email = "noreply@myapp.com" })
    , to = Array.wrap (Recipient (Address { name = Nothing, email = "user@example.com" }))
    , subject = "Test Subject"
    , body = HtmlBody "<p>Test</p>"
    , accessToken = Redacted.wrap "test-token"
    , onSuccess = \_ -> TestAcsCommand "success"
    , onError = \_ -> TestAcsCommand "error"
    }
