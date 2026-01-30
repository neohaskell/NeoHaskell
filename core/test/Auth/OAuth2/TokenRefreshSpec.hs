module Auth.OAuth2.TokenRefreshSpec (spec) where

import Auth.OAuth2.TokenRefresh (TokenRefreshError (..), withValidToken)
import Auth.OAuth2.Client qualified as OAuth2
import Auth.OAuth2.Types (
  ClientId (..),
  OAuth2Error (..),
  Provider (..),
  RefreshToken,
  TokenSet (..),
  mkAccessToken,
  mkClientSecret,
  mkRefreshToken,
  unsafeValidatedProvider,
  unwrapAccessToken,
 )
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import ConcurrentVar qualified
import Control.Concurrent qualified as GhcConcurrent
import Core
import Data.ByteString.Lazy qualified as GhcLBS
import Network.HTTP.Types qualified as GhcHTTP
import Network.Wai qualified as GhcWai
import Network.Wai.Handler.Warp qualified as GhcWarp
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "TokenRefresh.withValidToken" do
    -- Path 1: Happy path - action succeeds, no refresh needed
    it "returns action result when action succeeds" \_ -> do
      store <- InMemory.new
      let tokenSet = makeTokenSetWithRefresh "valid-token" "refresh-token"
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshSuccess
          isUnauthorized
          (\token -> Task.yield ([fmt|got #{token}|] :: Text))
          |> Task.asResult

      result |> shouldBe (Ok "got valid-token")

    -- Path 2: Refresh path - 401 triggers refresh and retry succeeds
    it "refreshes token and retries on unauthorized error" \_ -> do
      store <- InMemory.new
      let tokenSet = makeTokenSetWithRefresh "expired-token" "refresh-token"
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      attemptCounter <- ConcurrentVar.containing (0 :: Int)

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshSuccess -- Returns new token "new-access-token"
          isUnauthorized
          ( \token -> do
              count <- attemptCounter |> ConcurrentVar.peek
              attemptCounter |> ConcurrentVar.modify (\c -> c + 1)
              case count of
                0 -> Task.throw "401 Unauthorized" -- First attempt fails
                _ -> Task.yield ([fmt|got #{token}|] :: Text) -- Retry succeeds
          )
          |> Task.asResult

      result |> shouldBe (Ok "got new-access-token")
      -- Verify token was stored
      stored <- store.get key
      case stored of
        Just ts -> unwrapAccessToken ts.accessToken |> shouldBe "new-access-token"
        Nothing -> fail "Expected token to exist"

    -- Path 3: Token not found - no TokenSet stored
    it "returns TokenNotFound when no tokens stored for userId" \_ -> do
      store <- InMemory.new
      -- No tokens stored

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshSuccess
          isUnauthorized
          (\_ -> Task.yield "should not reach")
          |> Task.asResult

      result |> shouldBe (Err (TokenNotFound "user1"))

    -- Path 4: Refresh token missing - TokenSet has refreshToken = Nothing
    it "returns RefreshTokenMissing when stored TokenSet has no refresh token" \_ -> do
      store <- InMemory.new
      let tokenSet =
            TokenSet
              { accessToken = mkAccessToken "expired"
              , refreshToken = Nothing -- No refresh token!
              , expiresInSeconds = Just 3600
              }
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshSuccess
          isUnauthorized
          (\_ -> Task.throw "401 Unauthorized")
          |> Task.asResult

      result |> shouldBe (Err RefreshTokenMissing)

    -- Path 5: Refresh fails - OAuth2Error from refresh call
    it "returns RefreshFailed when token refresh fails" \_ -> do
      store <- InMemory.new
      let tokenSet = makeTokenSetWithRefresh "expired" "invalid-refresh"
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshFailure -- Returns OAuth2Error
          isUnauthorized
          (\_ -> Task.throw "401 Unauthorized")
          |> Task.asResult

      result |> shouldBe (Err (RefreshFailed (InvalidGrant "refresh token invalid")))

    -- Path 6: Retry fails - refresh succeeds but retry fails with non-401
    it "returns ActionFailed when retry fails with non-unauthorized error" \_ -> do
      store <- InMemory.new
      let tokenSet = makeTokenSetWithRefresh "expired" "valid-refresh"
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      attemptCounter <- ConcurrentVar.containing (0 :: Int)

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          mockRefreshSuccess
          isUnauthorized
          ( \_ -> do
              count <- attemptCounter |> ConcurrentVar.peek
              attemptCounter |> ConcurrentVar.modify (\c -> c + 1)
              case count of
                0 -> Task.throw "401 Unauthorized" -- First: unauthorized
                _ -> Task.throw "500 Internal Error" -- Retry: different error
          )
          |> Task.asResult

      result |> shouldBe (Err (ActionFailed "500 Internal Error"))

    -- Path 7: Retry also returns 401 - do NOT retry again (prevent infinite loop)
    it "does not retry twice when retry also returns unauthorized" \_ -> do
      store <- InMemory.new
      let tokenSet = makeTokenSetWithRefresh "expired" "valid-refresh"
      let key = TokenKey "oauth:provider:user1"
      store.put key tokenSet

      refreshCount <- ConcurrentVar.containing (0 :: Int)

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          store
          "provider"
          "user1"
          ( \_ -> do
              refreshCount |> ConcurrentVar.modify (\c -> c + 1)
              mockRefreshSuccessTask -- Track refresh calls
          )
          isUnauthorized
          (\_ -> Task.throw "401 Unauthorized") -- Always 401
          |> Task.asResult

      -- Should only refresh ONCE, then return ActionFailed
      finalRefreshCount <- refreshCount |> ConcurrentVar.peek
      finalRefreshCount |> shouldBe 1
      result |> shouldBe (Err (ActionFailed "401 Unauthorized"))

    -- Path 8: SecretStore.get error - wrapped in StorageError
    -- NOTE: Only tests get failure. atomicModify failure testing is out of scope for v1.
    it "returns StorageError when SecretStore.get fails" \_ -> do
      -- Create a store that fails on get
      let failingStore =
            SecretStore
              { get = \_ -> Task.throw "connection refused"
              , put = \_ _ -> Task.yield ()
              , delete = \_ -> Task.yield ()
              , atomicModify = \_ _ -> Task.yield ()
              }

      (result :: Result (TokenRefreshError Text) Text) <-
        withValidToken
          failingStore
          "provider"
          "user1"
          mockRefreshSuccess
          isUnauthorized
          (\_ -> Task.yield "should not reach")
          |> Task.asResult

      result |> shouldBe (Err (StorageError "connection refused"))

  -- Integration tests
  integrationSpec


-- | Mock refresh that always succeeds with a new token
mockRefreshSuccess :: RefreshToken -> Task OAuth2Error TokenSet
mockRefreshSuccess _ = mockRefreshSuccessTask


-- | Mock refresh task (for use when refresh token param is ignored)
mockRefreshSuccessTask :: Task OAuth2Error TokenSet
mockRefreshSuccessTask =
  Task.yield
    TokenSet
      { accessToken = mkAccessToken "new-access-token"
      , refreshToken = Just (mkRefreshToken "new-refresh-token")
      , expiresInSeconds = Just 3600
      }


-- | Mock refresh that always fails
mockRefreshFailure :: RefreshToken -> Task OAuth2Error TokenSet
mockRefreshFailure _ = Task.throw (InvalidGrant "refresh token invalid")


-- | Predicate: is this error a 401?
-- Uses precise matching to avoid false positives (e.g., "14010" should not match)
isUnauthorized :: Text -> Bool
isUnauthorized err = Text.startsWith "401 " err || err == "401"


-- | Helper to create TokenSet with refresh token
-- NOTE: Do NOT use TestUtils.makeTestTokenKeyWithPrefix - it uses oauth2: prefix
makeTokenSetWithRefresh :: Text -> Text -> TokenSet
makeTokenSetWithRefresh access refresh =
  TokenSet
    { accessToken = mkAccessToken access
    , refreshToken = Just (mkRefreshToken refresh)
    , expiresInSeconds = Just 3600
    }


-- | Mock OAuth2 token endpoint that returns fixed tokens
mockTokenApp :: GhcWai.Application
mockTokenApp request respond = do
  case (GhcWai.requestMethod request, GhcWai.pathInfo request) of
    ("POST", ["oauth", "token"]) -> do
      let responseBody = GhcLBS.fromStrict "{\"access_token\":\"fresh-token\",\"refresh_token\":\"fresh-refresh\",\"expires_in\":3600,\"token_type\":\"Bearer\"}"
      respond (GhcWai.responseLBS GhcHTTP.status200 [(GhcHTTP.hContentType, "application/json")] responseBody)
    _ ->
      respond (GhcWai.responseLBS GhcHTTP.status404 [] "Not Found")


-- Integration test using real HTTP
integrationSpec :: Spec Unit
integrationSpec = do
  describe "Integration: withValidToken with HTTP" do
    it "refreshes via HTTP and retries action" \_ -> do
      -- Use a fixed port but ensure cleanup via Task.finally
      -- Port 19876 is unlikely to conflict; CI runs tests in isolation
      let testPort = 19876

      -- Start server in background thread (IO-level, not Task)
      serverThread <- GhcConcurrent.forkIO (GhcWarp.run testPort mockTokenApp) |> Task.fromIO

      -- Give server time to bind (simple approach)
      GhcConcurrent.threadDelay 50000 |> Task.fromIO -- 50ms

      -- Run test with guaranteed cleanup via Task.finally
      let runTest = do
            -- Create provider pointing to mock server
            -- MUST use unsafeValidatedProvider (localhost fails SSRF validation)
            let mockProvider =
                  Provider
                    { name = "mock"
                    , authorizeEndpoint = [fmt|http://localhost:#{testPort}/oauth/authorize|]
                    , tokenEndpoint = [fmt|http://localhost:#{testPort}/oauth/token|]
                    }
            let validatedProvider = unsafeValidatedProvider mockProvider

            -- Store initial tokens
            store <- InMemory.new
            let tokenSet = makeTokenSetWithRefresh "expired" "old-refresh"
            let key = TokenKey "oauth:mock:user1"
            store.put key tokenSet

            -- Create real refresh action using refreshTokenValidated
            let clientId = ClientId "test-client"
            let clientSecret = mkClientSecret "test-secret"
            let refreshAction rt = OAuth2.refreshTokenValidated validatedProvider clientId clientSecret rt

            -- Create action that fails first with 401, succeeds on retry
            attemptCounter <- ConcurrentVar.containing (0 :: Int)
            let action token = do
                  count <- attemptCounter |> ConcurrentVar.peek
                  attemptCounter |> ConcurrentVar.modify (\c -> c + 1)
                  case count of
                    0 -> Task.throw "401 Unauthorized"
                    _ -> Task.yield ([fmt|success with #{token}|] :: Text)

            -- Call withValidToken
            (result :: Result (TokenRefreshError Text) Text) <-
              withValidToken
                store
                "mock"
                "user1"
                refreshAction
                isUnauthorized
                action
                |> Task.asResult

            -- Verify result
            result |> shouldBe (Ok "success with fresh-token")

            -- Verify new tokens stored
            stored <- store.get key
            case stored of
              Just ts -> unwrapAccessToken ts.accessToken |> shouldBe "fresh-token"
              Nothing -> fail "Expected token to exist"

      -- Cleanup: always kill server thread (even on test failure)
      let cleanup = GhcConcurrent.killThread serverThread |> Task.fromIO
      runTest |> Task.finally cleanup
