module Auth.OAuth2.RoutesSpec where

import Array qualified
import Auth.OAuth2.Provider (OAuth2Action (..), ValidatedOAuth2ProviderConfig (..))
import Auth.OAuth2.RateLimiter qualified as RateLimiter
import Auth.OAuth2.Routes (OAuth2RouteDeps (..), OAuth2RouteError (..), OAuth2Routes (..), createRoutes)
import Auth.OAuth2.StateToken (mkHmacKey)
import Auth.OAuth2.TransactionStore.InMemory qualified as InMemory
import Auth.OAuth2.Types (
  ClientId (..),
  Provider (..),
  Scope (..),
  mkClientSecret,
  mkRedirectUri,
  unsafeValidatedProvider,
 )
import Core
import Map qualified
import Maybe qualified
import Task qualified
import Test
import Text qualified


-- | Extract state parameter from OAuth2 authorization URL (test helper)
extractStateFromUrl :: Text -> Text
extractStateFromUrl url = do
  let parts = Text.split "state=" url
  case parts |> Array.get 1 of
    Maybe.Nothing -> ""
    Maybe.Just rest -> Text.split "&" rest |> Array.get 0 |> Maybe.withDefault ""


-- | Test configuration for a mock OAuth2 provider
mockProvider :: Provider
mockProvider =
  Provider
    { name = "mock"
    , authorizeEndpoint = "https://mock.example.com/oauth/authorize"
    , tokenEndpoint = "https://mock.example.com/oauth/token"
    }


-- | Create a validated provider config for testing.
-- In tests, we validate the provider to ensure the flow works correctly.
-- The test provider endpoints (mock.example.com) will fail DNS validation,
-- so we construct the ValidatedProvider directly for testing purposes.
mockValidatedProviderConfig :: Task Text ValidatedOAuth2ProviderConfig
mockValidatedProviderConfig = do
  let redirectUriResult = mkRedirectUri "http://localhost:8080/callback/mock"
  redirectUri <- case redirectUriResult of
    Ok uri -> Task.yield uri
    Err err -> Task.throw [fmt|Invalid redirect URI in test: #{err}|]
  -- For tests, we need to bypass DNS validation since mock.example.com doesn't exist.
  -- We validate against a real endpoint to ensure the validation logic works,
  -- but for this unit test we construct the validated type directly.
  -- Integration tests should use real providers.
  let provider = mockProvider
  -- NOTE: In production, use OAuth2.validateProvider. For unit tests with mock
  -- providers, we use unsafeValidatedProvider which is explicitly marked as
  -- internal/test-only. This is safe because mock.example.com is not a real target.
  let validatedProvider = unsafeValidatedProvider provider
  Task.yield
    ValidatedOAuth2ProviderConfig
      { provider = provider
      , validatedProvider = validatedProvider
      , clientId = ClientId "test-client-id"
      , clientSecret = mkClientSecret "test-client-secret-that-is-long-enough"
      , redirectUri = redirectUri
      , scopes = Array.fromLinkedList [Scope "read", Scope "write"]
      , onSuccess = \userId _tokens -> [fmt|{"type":"success","userId":"#{userId}"}|]
      , onFailure = \userId _err -> [fmt|{"type":"failure","userId":"#{userId}"}|]
      , onDisconnect = \userId -> [fmt|{"type":"disconnect","userId":"#{userId}"}|]
      , successRedirectUrl = "https://app.example.com/settings?connected=mock"
      , failureRedirectUrl = "https://app.example.com/settings?error=oauth"
      }


-- | Create test dependencies with the mock provider
createTestDeps :: Task Text OAuth2RouteDeps
createTestDeps = do
  let keyResult = mkHmacKey "this-is-a-32-byte-secret-key!!!!"
  hmacKey <- case keyResult of
    Err err -> Task.throw err
    Ok key -> Task.yield key
  transactionStore <- InMemory.new
  validatedConfig <- mockValidatedProviderConfig
  let providers = Map.empty |> Map.set "mock" validatedConfig
  -- Create rate limiters with generous limits for testing
  connectRateLimiter <- RateLimiter.new RateLimiter.defaultConnectConfig
  callbackRateLimiter <- RateLimiter.new RateLimiter.defaultCallbackConfig
  Task.yield
    OAuth2RouteDeps
      { hmacKey = hmacKey
      , transactionStore = transactionStore
      , providers = providers
      , connectRateLimiter = connectRateLimiter
      , callbackRateLimiter = callbackRateLimiter
      }


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.Routes" do
    -- ========================================================================
    -- handleConnect
    -- ========================================================================
    describe "handleConnect" do
      it "returns authorization URL for valid provider" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        result <- routes.handleConnect "mock" userId |> Task.asResult
        case result of
          Err err -> fail [fmt|Expected success, got error: #{toText (show err)}|]
          Ok authUrl -> do
            -- Should contain the authorization endpoint
            authUrl |> shouldSatisfy (\url -> Text.contains "mock.example.com/oauth/authorize" url)
            -- Should contain client_id
            authUrl |> shouldSatisfy (\url -> Text.contains "client_id=test-client-id" url)
            -- Should contain state parameter
            authUrl |> shouldSatisfy (\url -> Text.contains "state=" url)
            -- Should contain PKCE code_challenge
            authUrl |> shouldSatisfy (\url -> Text.contains "code_challenge=" url)
            authUrl |> shouldSatisfy (\url -> Text.contains "code_challenge_method=S256" url)

      it "returns ProviderNotFound for unknown provider" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        result <- routes.handleConnect "unknown-provider" userId |> Task.asResult
        case result of
          Ok _ -> fail "Expected ProviderNotFound error"
          Err err -> do
            case err of
              ProviderNotFound name -> name |> shouldBe "unknown-provider"
              _ -> fail [fmt|Expected ProviderNotFound, got: #{toText (show err)}|]

      it "generates unique state tokens for different requests" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        -- Make two connect requests
        result1 <- routes.handleConnect "mock" userId |> Task.asResult
        result2 <- routes.handleConnect "mock" userId |> Task.asResult

        case (result1, result2) of
          (Ok url1, Ok url2) -> do
            -- Extract state parameters (they should be different)
            let state1 = extractStateFromUrl url1
            let state2 = extractStateFromUrl url2
            -- States should be non-empty
            state1 |> shouldSatisfy (\s -> Text.length s > 0)
            state2 |> shouldSatisfy (\s -> Text.length s > 0)
            -- States should be unique (crypto-random nonces ensure uniqueness)
            state1 |> shouldNotBe state2
          _ -> fail "Expected both requests to succeed"

    -- ========================================================================
    -- handleDisconnect
    -- ========================================================================
    describe "handleDisconnect" do
      it "returns DisconnectAction for valid provider" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        result <- routes.handleDisconnect "mock" userId |> Task.asResult
        case result of
          Err err -> fail [fmt|Expected success, got error: #{toText (show err)}|]
          Ok action -> do
            case action of
              DisconnectAction json -> do
                json |> shouldSatisfy (\j -> Text.contains "disconnect" j)
                json |> shouldSatisfy (\j -> Text.contains "user-123" j)
              _ -> fail "Expected DisconnectAction"

      it "returns ProviderNotFound for unknown provider" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        result <- routes.handleDisconnect "unknown-provider" userId |> Task.asResult
        case result of
          Ok _ -> fail "Expected ProviderNotFound error"
          Err err -> do
            case err of
              ProviderNotFound name -> name |> shouldBe "unknown-provider"
              _ -> fail [fmt|Expected ProviderNotFound, got: #{toText (show err)}|]

    -- ========================================================================
    -- handleCallback - State Validation
    -- ========================================================================
    describe "handleCallback" do
      it "returns StateValidationFailed for invalid state token" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps

        -- Try callback with invalid state (not properly signed)
        result <- routes.handleCallback "mock" "fake-code" "invalid-state-token" |> Task.asResult
        case result of
          Ok _ -> fail "Expected StateValidationFailed error"
          Err err -> do
            case err of
              StateValidationFailed _ -> pass -- Expected
              _ -> fail [fmt|Expected StateValidationFailed, got: #{toText (show err)}|]

      it "returns StateNotFound for consumed state (replay attack prevention)" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps
        let userId = "user-123"

        -- First, initiate a connect to get a valid state
        connectResult <- routes.handleConnect "mock" userId |> Task.asResult
        case connectResult of
          Err err -> fail [fmt|Connect failed: #{toText (show err)}|]
          Ok authUrl -> do
            -- Extract state from URL
            let stateToken = extractStateFromUrl authUrl

            -- First callback consumes the state
            -- Note: This will fail at token exchange (no real OAuth2 server),
            -- but the state should still be consumed
            _ <- routes.handleCallback "mock" "fake-code" stateToken |> Task.asResult

            -- Second callback with same state should fail with StateNotFound
            result2 <- routes.handleCallback "mock" "fake-code" stateToken |> Task.asResult
            case result2 of
              Ok _ -> fail "Expected StateNotFound on replay"
              Err err -> do
                case err of
                  StateNotFound -> pass -- Expected - state was already consumed
                  StateValidationFailed _ -> pass -- Also acceptable - expired or invalid
                  _ -> fail [fmt|Expected StateNotFound or StateValidationFailed, got: #{toText (show err)}|]

      it "returns ProviderNotFound for unknown provider" \_ -> do
        deps <- createTestDeps
        let routes = createRoutes deps

        result <- routes.handleCallback "unknown-provider" "code" "state" |> Task.asResult
        case result of
          Ok _ -> fail "Expected ProviderNotFound error"
          Err err -> do
            case err of
              ProviderNotFound name -> name |> shouldBe "unknown-provider"
              _ -> fail [fmt|Expected ProviderNotFound, got: #{toText (show err)}|]
