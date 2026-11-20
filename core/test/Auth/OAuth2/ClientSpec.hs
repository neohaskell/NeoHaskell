module Auth.OAuth2.ClientSpec where

import Array qualified
import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Types (
  ClientId (..),
  CodeChallengeMethod (..),
  OAuth2Error (..),
  Provider (..),
  RedirectUri,
  Scope (..),
  TokenSet (..),
  mkAuthorizationCode,
  mkClientSecret,
  mkCodeVerifierUnsafe,
  mkRedirectUri,
  mkRefreshToken,
  mkState,
  unsafeValidatedProvider,
  unwrapAccessToken,
 )
import Bytes qualified
import ConcurrentVar qualified
import Core
import Data.ByteString qualified as GhcBS
import Data.ByteString.Lazy qualified as GhcLBS
import LinkedList qualified
import Maybe qualified
import Network.HTTP.Types qualified as GhcHTTP
import Network.Socket qualified as GhcSocket
import Network.Wai qualified as GhcWai
import Network.Wai.Handler.Warp qualified as GhcWarp
import Task qualified
import Test
import Text qualified


-- | Helper to create RedirectUri in tests, panics if invalid.
-- Only use with known-valid HTTPS test URLs.
testRedirectUri :: Text -> RedirectUri
testRedirectUri url =
  case mkRedirectUri url of
    Ok uri -> uri
    Err err -> panic [fmt|Test URL should be valid: #{url}, error: #{toText err}|]


spec :: Spec Unit
spec = parallel do
  describe "Auth.OAuth2.Client" do
    describe "HTTPS/SSRF Protection (security)" do
      -- CRITICAL: OAuth2 token endpoints MUST use HTTPS
      -- and MUST NOT allow SSRF via private IPs

      describe "exchangeCode" do
        it "rejects HTTP token endpoint (requires HTTPS)" \_ -> do
          let provider =
                Provider
                  { name = "insecure"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "http://example.com/token" -- HTTP, not HTTPS
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP endpoint"

        -- NOTE: 127.0.0.1 (localhost) is now ALLOWED for development (ADR-0018).
        -- This test uses 10.0.0.1 instead to verify SSRF protection for non-localhost private IPs.
        it "rejects private IP in token endpoint (SSRF protection)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-attempt"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "https://10.0.0.1/token" -- Private network (non-localhost)
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for private IP endpoint"

        it "rejects internal IP ranges in token endpoint (SSRF protection)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-internal"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "https://192.168.1.1/token" -- Private IP
                  }
          result <-
            OAuth2.exchangeCode
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (testRedirectUri "https://myapp.com/callback")
              (mkAuthorizationCode "auth-code")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for internal IP endpoint"

      describe "refreshToken" do
        it "rejects HTTP token endpoint (requires HTTPS)" \_ -> do
          let provider =
                Provider
                  { name = "insecure"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "http://example.com/token"
                  }
          result <-
            OAuth2.refreshToken
              provider
              (ClientId "client-id")
              (mkClientSecret "client-secret")
              (mkRefreshToken "refresh-token")
              |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP endpoint"

    describe "authorizeUrl" do
      it "builds a valid authorization URL with all required parameters" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        let url =
              OAuth2.authorizeUrl
                provider
                (ClientId "my-client-id")
                (testRedirectUri "https://myapp.com/callback")
                (Array.fromLinkedList [Scope "read", Scope "write"])
                (mkState "random-state-123")
        -- Should contain required OAuth2 parameters
        url |> shouldSatisfy (\u -> Text.contains "response_type=code" u)
        url |> shouldSatisfy (\u -> Text.contains "client_id=my-client-id" u)
        url |> shouldSatisfy (\u -> Text.contains "redirect_uri=" u)
        url |> shouldSatisfy (\u -> Text.contains "state=" u)
        url |> shouldSatisfy (\u -> Text.contains "scope=" u)
        -- Should start with the provider's authorize endpoint
        url |> shouldSatisfy (\u -> Text.contains "https://auth.example.com/authorize?" u)

      it "URL-encodes special characters in parameters" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        let url =
              OAuth2.authorizeUrl
                provider
                (ClientId "client-id")
                (testRedirectUri "https://myapp.com/callback?extra=param")
                (Array.fromLinkedList [Scope "profile email"])
                (mkState "state")
        -- Special characters should be encoded
        -- Space in scope -> %20, ? and = in redirect_uri -> %3F and %3D
        url |> shouldSatisfy (\u -> Text.contains "%20" u || Text.contains "+" u)

    describe "authorizeUrlWithPkce" do
      it "includes PKCE parameters in authorization URL" \_ -> do
        let provider =
              Provider
                { name = "test"
                , authorizeEndpoint = "https://auth.example.com/authorize"
                , tokenEndpoint = "https://auth.example.com/token"
                }
        verifier <- OAuth2.generateCodeVerifier
        let challenge = OAuth2.deriveCodeChallenge verifier
        let url =
              OAuth2.authorizeUrlWithPkce
                provider
                (ClientId "client-id")
                (testRedirectUri "https://myapp.com/callback")
                (Array.fromLinkedList [Scope "read"])
                (mkState "state")
                challenge
                S256
        -- Should include PKCE parameters
        url |> shouldSatisfy (\u -> Text.contains "code_challenge=" u)
        url |> shouldSatisfy (\u -> Text.contains "code_challenge_method=S256" u)

    describe "PKCE" do
      it "generateCodeVerifier produces valid verifier" \_ -> do
        verifier <- OAuth2.generateCodeVerifier
        -- Verifier should exist (opaque type, can't inspect value)
        -- but deriveCodeChallenge should work
        let _ = OAuth2.deriveCodeChallenge verifier
        Task.yield ()

      it "deriveCodeChallenge produces consistent results" \_ -> do
        verifier <- OAuth2.generateCodeVerifier
        let challenge1 = OAuth2.deriveCodeChallenge verifier
        let challenge2 = OAuth2.deriveCodeChallenge verifier
        -- Same verifier should produce same challenge
        challenge1 |> shouldBe challenge2

    describe "validateProvider" do
      describe "success cases" do
        -- NOTE: Can't test success without actual DNS resolution to public IPs.
        -- Real providers (github.com, google.com) would work but make tests flaky.
        -- Success path is tested via integration tests.
        pass

      describe "failure cases (security)" do
        it "rejects provider with HTTP authorize endpoint" \_ -> do
          let provider =
                Provider
                  { name = "http-auth"
                  , authorizeEndpoint = "http://example.com/authorize" -- HTTP, not HTTPS
                  , tokenEndpoint = "https://example.com/token"
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed msg) -> do
              -- Should mention HTTPS requirement
              msg |> shouldSatisfy (\m -> Text.contains "HTTPS" m || Text.contains "https" m)
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP authorize endpoint"

        it "rejects provider with HTTP token endpoint" \_ -> do
          let provider =
                Provider
                  { name = "http-token"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "http://example.com/token" -- HTTP, not HTTPS
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed msg) -> do
              msg |> shouldSatisfy (\m -> Text.contains "HTTPS" m || Text.contains "https" m)
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for HTTP token endpoint"

        it "rejects provider with private IP in authorize endpoint (SSRF)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-auth"
                  , authorizeEndpoint = "https://192.168.1.1/authorize" -- Private IP
                  , tokenEndpoint = "https://example.com/token"
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for private IP in authorize endpoint"

        -- NOTE: 127.0.0.1 and localhost are now ALLOWED for development (ADR-0018).
        -- Use 10.0.0.1 to test that non-localhost private IPs are still blocked.
        it "rejects provider with private IP in token endpoint (SSRF)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-token"
                  , authorizeEndpoint = "https://example.com/authorize"
                  , tokenEndpoint = "https://10.0.0.1/token" -- Private network (non-localhost)
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for private IP in token endpoint"

        -- NOTE: localhost is now ALLOWED for development (ADR-0018).
        -- Use 172.16.x.x to test that non-localhost private networks are still blocked.
        it "rejects provider with private network IP in endpoint (SSRF)" \_ -> do
          let provider =
                Provider
                  { name = "ssrf-private"
                  , authorizeEndpoint = "https://172.16.0.1/authorize" -- Private network (non-localhost)
                  , tokenEndpoint = "https://example.com/token"
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for private network IP in endpoint"

        it "rejects provider with malformed URL in authorize endpoint" \_ -> do
          let provider =
                Provider
                  { name = "malformed"
                  , authorizeEndpoint = "not-a-valid-url"
                  , tokenEndpoint = "https://example.com/token"
                  }
          result <- OAuth2.validateProvider provider |> Task.asResult
          case result of
            Err (EndpointValidationFailed _) -> Task.yield ()
            Err other -> fail [fmt|Expected EndpointValidationFailed, got: #{toText other}|]
            Ok _ -> fail "Expected error for malformed URL"

    describe "token endpoint responses" do
      tokenResponseSpec "exchangeCode" exchangeCodeEntrypoint
      tokenResponseSpec "exchangeCodeWithPkce" exchangeCodeWithPkceEntrypoint
      tokenResponseSpec "exchangeCodeValidated" exchangeCodeValidatedEntrypoint
      tokenResponseSpec "exchangeCodeWithPkceValidated" exchangeCodeWithPkceValidatedEntrypoint
      tokenResponseSpec "refreshToken" refreshTokenEntrypoint

      describe "status-first decoding" do
        it "returns ScopeDenied for an invalid_scope response" \_ -> do
          let reply = jsonReply GhcHTTP.status400 "{\"error\":\"invalid_scope\",\"error_description\":\"scope not granted\"}"
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (shouldBe (Err (ScopeDenied "scope not granted")))

        it "returns TokenRequestFailed with the provider description for other OAuth error codes" \_ -> do
          let reply = jsonReply GhcHTTP.status400 "{\"error\":\"unsupported_grant_type\",\"error_description\":\"grant type not enabled\"}"
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (shouldBe (Err (TokenRequestFailed "grant type not enabled")))

        it "uses the error code when the provider sends no description" \_ -> do
          let reply = jsonReply GhcHTTP.status400 "{\"error\":\"invalid_grant\"}"
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (shouldBe (Err (InvalidGrant "invalid_grant")))

        it "returns TokenRequestFailed for an error status whose JSON is not an OAuth error" \_ -> do
          let reply = jsonReply GhcHTTP.status500 "{\"message\":\"owned-error-secret-sentinel\"}"
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (expectGenericStatusFailure "500")

        it "does not accept tokens from an error status" \_ -> do
          let reply = jsonReply GhcHTTP.status500 "{\"access_token\":\"secret-sentinel-token\",\"token_type\":\"Bearer\"}"
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (expectGenericStatusFailure "500")

        it "accepts tokens from any 2xx status" \_ -> do
          let reply = jsonReply GhcHTTP.status201 validTokenResponse
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply \result -> do
            case result of
              Ok tokens -> unwrapAccessToken tokens.accessToken |> shouldBe "fresh-token"
              Err err -> fail [fmt|Expected tokens, got #{toText err}|]

        it "returns a redirect status without following it" \_ -> do
          let reply = GhcWai.responseLBS GhcHTTP.status302 [(GhcHTTP.hLocation, "/oauth/elsewhere")] GhcLBS.empty
          expectTokenResult exchangeCodeWithPkceValidatedEntrypoint reply (expectGenericStatusFailure "302")

        it "returns a sanitized NetworkError when the endpoint refuses the connection" \_ -> do
          withRefusingPort \refusingPort -> do
            result <-
              exchangeCodeWithPkceValidatedEntrypoint.call (loopbackProvider refusingPort)
                |> Task.asResult
            case result of
              Err (NetworkError message) -> do
                Text.contains "test-secret" message |> shouldBe False
                Text.contains "test-code" message |> shouldBe False
                Text.contains "test-verifier" message |> shouldBe False
                Text.contains "/oauth/token" message |> shouldBe False
              _ -> fail [fmt|Expected NetworkError, got #{toText result}|]


-- | One public token-request entrypoint and the form fields it must send.
data TokenEntrypoint = TokenEntrypoint
  { call :: Provider -> Task OAuth2Error TokenSet,
    form :: LinkedList (Text, Maybe Text)
  }


-- | What the token endpoint received in one request.
data ObservedTokenRequest = ObservedTokenRequest
  { method :: Text,
    path :: Text,
    accept :: Maybe Text,
    contentType :: Maybe Text,
    form :: LinkedList (Text, Maybe Text)
  }
  deriving (Eq, Show)


-- | The response contract that every token-request entrypoint shares.
-- 'OAuth2.refreshTokenValidated' proves the same contract in "Auth.OAuth2.TokenRefreshSpec".
tokenResponseSpec :: Text -> TokenEntrypoint -> Spec Unit
tokenResponseSpec name entrypoint = do
  describe name do
    it "returns the tokens of a 200 response with their optional fields" \_ -> do
      expectTokenResult entrypoint (jsonReply GhcHTTP.status200 validTokenResponse) \result -> do
        case result of
          Ok tokens -> do
            unwrapAccessToken tokens.accessToken |> shouldBe "fresh-token"
            tokens.refreshToken |> shouldBe (Just (mkRefreshToken "fresh-refresh"))
            tokens.expiresInSeconds |> shouldBe (Just 3600)
          Err err -> fail [fmt|Expected tokens, got #{toText err}|]

    it "returns InvalidGrant for a 400 invalid_grant response" \_ -> do
      let reply = jsonReply GhcHTTP.status400 "{\"error\":\"invalid_grant\",\"error_description\":\"owned grant rejected\"}"
      expectTokenResult entrypoint reply (shouldBe (Err (InvalidGrant "owned grant rejected")))

    it "returns InvalidClient for a 401 invalid_client response" \_ -> do
      let reply = jsonReply GhcHTTP.status401 "{\"error\":\"invalid_client\",\"error_description\":\"owned client rejected\"}"
      expectTokenResult entrypoint reply (shouldBe (Err (InvalidClient "owned client rejected")))

    it "returns MalformedResponse for a 200 response that is not JSON" \_ -> do
      expectTokenResult entrypoint (jsonReply GhcHTTP.status200 "not-json-secret-sentinel") expectMalformedTokenResponse

    it "returns MalformedResponse for a 200 response without access_token" \_ -> do
      let reply = jsonReply GhcHTTP.status200 "{\"token_type\":\"Bearer\",\"refresh_token\":\"secret-sentinel-refresh\"}"
      expectTokenResult entrypoint reply expectMalformedTokenResponse

    it "returns TokenRequestFailed for a non-JSON error status" \_ -> do
      expectTokenResult entrypoint (jsonReply GhcHTTP.status503 "owned-error-secret-sentinel") (expectGenericStatusFailure "503")


-- | Call the entrypoint against one canned response and check its result.
-- The endpoint must receive exactly one POST that carries the entrypoint's form.
expectTokenResult ::
  TokenEntrypoint ->
  GhcWai.Response ->
  (Result OAuth2Error TokenSet -> Task Text Unit) ->
  Task Text Unit
expectTokenResult entrypoint reply check =
  withTokenEndpoint reply \provider requests -> do
    result <- entrypoint.call provider |> Task.asResult
    check result
    seen <- requests
    seen
      |> shouldBe
        [ ObservedTokenRequest
            { method = "POST",
              path = "/oauth/token",
              accept = Just "application/json",
              contentType = Just "application/x-www-form-urlencoded",
              form = entrypoint.form |> LinkedList.sort
            }
        ]


expectMalformedTokenResponse :: Result OAuth2Error TokenSet -> Task Text Unit
expectMalformedTokenResponse result =
  case result of
    Err (MalformedResponse message) -> Text.contains "secret-sentinel" message |> shouldBe False
    _ -> fail [fmt|Expected MalformedResponse, got #{toText result}|]


-- | The failure names the HTTP status and nothing from the response body.
expectGenericStatusFailure :: Text -> Result OAuth2Error TokenSet -> Task Text Unit
expectGenericStatusFailure status result =
  case result of
    Err (TokenRequestFailed message) -> do
      Text.contains "secret-sentinel" message |> shouldBe False
      Text.contains status message |> shouldBe True
    _ -> fail [fmt|Expected TokenRequestFailed, got #{toText result}|]


-- | Serve one canned response on a loopback port that Warp owns, and record every request.
withTokenEndpoint ::
  GhcWai.Response ->
  (Provider -> Task Text (Array ObservedTokenRequest) -> Task Text Unit) ->
  Task Text Unit
withTokenEndpoint reply runTest = do
  received <- ConcurrentVar.containing (Array.empty :: Array ObservedTokenRequest)
  let application request respond = do
        requestBody <- GhcWai.strictRequestBody request
        let observed =
              ObservedTokenRequest
                { method = GhcWai.requestMethod request |> utf8Text,
                  path = GhcWai.rawPathInfo request |> utf8Text,
                  accept = requestHeader GhcHTTP.hAccept request,
                  contentType = requestHeader GhcHTTP.hContentType request,
                  form = requestBody |> GhcLBS.toStrict |> decodeForm
                }
        Task.runOrPanic (ConcurrentVar.modify (Array.push observed) received :: Task Text Unit)
        respond reply
  let settings = GhcWarp.defaultSettings |> GhcWarp.setHost "127.0.0.1"
  GhcWarp.testWithApplicationSettings
    settings
    (Task.runOrPanic (Task.yield application :: Task Text GhcWai.Application))
    (\port -> Task.runOrPanic (runTest (loopbackProvider port) (ConcurrentVar.peek received)))
    |> Task.fromIO


-- | Reserve a loopback port that refuses connections for the duration of the test.
-- The socket stays bound and never listens, so no other process can take the port.
withRefusingPort :: (Int -> Task Text Unit) -> Task Text Unit
withRefusingPort runTest = do
  let hints = GhcSocket.defaultHints {GhcSocket.addrSocketType = GhcSocket.Stream}
  addresses <- GhcSocket.getAddrInfo (Just hints) (Just "127.0.0.1") (Just "0") |> Task.fromIO
  case addresses of
    [] -> fail "The loopback address did not resolve"
    address : _ -> do
      socket <- GhcSocket.openSocket address |> Task.fromIO
      let reserveAndRun = do
            GhcSocket.bind socket (GhcSocket.addrAddress address) |> Task.fromIO
            port <- GhcSocket.socketPort socket |> Task.fromIO
            runTest (fromIntegral port)
      reserveAndRun |> Task.finally (GhcSocket.close socket |> Task.fromIO)


-- | Plain HTTP reaches the per-request validated entrypoints only through the loopback exception.
loopbackProvider :: Int -> Provider
loopbackProvider port =
  Provider
    { name = "owned-token-endpoint",
      authorizeEndpoint = [fmt|http://127.0.0.1:#{port}/oauth/authorize|],
      tokenEndpoint = [fmt|http://127.0.0.1:#{port}/oauth/token|]
    }


jsonReply :: GhcHTTP.Status -> GhcLBS.ByteString -> GhcWai.Response
jsonReply status body = GhcWai.responseLBS status [(GhcHTTP.hContentType, "application/json")] body


validTokenResponse :: GhcLBS.ByteString
validTokenResponse =
  "{\"access_token\":\"fresh-token\",\"refresh_token\":\"fresh-refresh\",\"expires_in\":3600,\"token_type\":\"Bearer\"}"


-- | Decode a url-encoded body into its sorted fields, because field order is not part of the contract.
decodeForm :: GhcBS.ByteString -> LinkedList (Text, Maybe Text)
decodeForm body =
  body
    |> GhcHTTP.parseQuery
    |> LinkedList.map (\(name, value) -> (utf8Text name, Maybe.map utf8Text value))
    |> LinkedList.sort


utf8Text :: GhcBS.ByteString -> Text
utf8Text bytes = bytes |> Bytes.fromLegacy |> Text.fromBytes


requestHeader :: GhcHTTP.HeaderName -> GhcWai.Request -> Maybe Text
requestHeader name request =
  GhcWai.requestHeaders request
    |> Array.fromLinkedList
    |> Array.find (\(headerName, _) -> headerName == name)
    |> Maybe.map (\(_, value) -> utf8Text value)


-- | The query string makes the redirect URI need url-encoding in the form body.
tokenRedirectUrl :: Text
tokenRedirectUrl = "https://app.example.com/callback?source=oauth&step=2"


authorizationCodeForm :: LinkedList (Text, Maybe Text)
authorizationCodeForm =
  [ ("grant_type", Just "authorization_code"),
    ("code", Just "test-code"),
    ("redirect_uri", Just tokenRedirectUrl),
    ("client_id", Just "test-client"),
    ("client_secret", Just "test-secret")
  ]


pkceForm :: LinkedList (Text, Maybe Text)
pkceForm = authorizationCodeForm ++ [("code_verifier", Just "test-verifier")]


exchangeCodeEntrypoint :: TokenEntrypoint
exchangeCodeEntrypoint =
  TokenEntrypoint
    { call = \provider ->
        OAuth2.exchangeCode
          provider
          (ClientId "test-client")
          (mkClientSecret "test-secret")
          (testRedirectUri tokenRedirectUrl)
          (mkAuthorizationCode "test-code"),
      form = authorizationCodeForm
    }


exchangeCodeWithPkceEntrypoint :: TokenEntrypoint
exchangeCodeWithPkceEntrypoint =
  TokenEntrypoint
    { call = \provider ->
        OAuth2.exchangeCodeWithPkce
          provider
          (ClientId "test-client")
          (mkClientSecret "test-secret")
          (testRedirectUri tokenRedirectUrl)
          (mkAuthorizationCode "test-code")
          (mkCodeVerifierUnsafe "test-verifier"),
      form = pkceForm
    }


exchangeCodeValidatedEntrypoint :: TokenEntrypoint
exchangeCodeValidatedEntrypoint =
  TokenEntrypoint
    { call = \provider ->
        OAuth2.exchangeCodeValidated
          (unsafeValidatedProvider provider)
          (ClientId "test-client")
          (mkClientSecret "test-secret")
          (testRedirectUri tokenRedirectUrl)
          (mkAuthorizationCode "test-code"),
      form = authorizationCodeForm
    }


exchangeCodeWithPkceValidatedEntrypoint :: TokenEntrypoint
exchangeCodeWithPkceValidatedEntrypoint =
  TokenEntrypoint
    { call = \provider ->
        OAuth2.exchangeCodeWithPkceValidated
          (unsafeValidatedProvider provider)
          (ClientId "test-client")
          (mkClientSecret "test-secret")
          (testRedirectUri tokenRedirectUrl)
          (mkAuthorizationCode "test-code")
          (mkCodeVerifierUnsafe "test-verifier"),
      form = pkceForm
    }


refreshTokenEntrypoint :: TokenEntrypoint
refreshTokenEntrypoint =
  TokenEntrypoint
    { call = \provider ->
        OAuth2.refreshToken
          provider
          (ClientId "test-client")
          (mkClientSecret "test-secret")
          (mkRefreshToken "test-refresh"),
      form =
        [ ("grant_type", Just "refresh_token"),
          ("refresh_token", Just "test-refresh"),
          ("client_id", Just "test-client"),
          ("client_secret", Just "test-secret")
        ]
    }
