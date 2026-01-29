module Auth.OAuth2.TokenRefreshSpec (spec) where

import Auth.OAuth2.TokenRefresh (TokenRefreshError (..), withValidToken)
import Auth.OAuth2.Types (
  OAuth2Error (..),
  RefreshToken,
  TokenSet (..),
  mkAccessToken,
  mkRefreshToken,
  unwrapAccessToken,
 )
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import ConcurrentVar qualified
import Core
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
isUnauthorized :: Text -> Bool
isUnauthorized err = err |> Text.contains "401"


-- | Helper to create TokenSet with refresh token
-- NOTE: Do NOT use TestUtils.makeTestTokenKeyWithPrefix - it uses oauth2: prefix
makeTokenSetWithRefresh :: Text -> Text -> TokenSet
makeTokenSetWithRefresh access refresh =
  TokenSet
    { accessToken = mkAccessToken access
    , refreshToken = Just (mkRefreshToken refresh)
    , expiresInSeconds = Just 3600
    }
