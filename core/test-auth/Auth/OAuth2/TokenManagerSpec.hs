module Auth.OAuth2.TokenManagerSpec (spec) where

import Auth.OAuth2.TokenManager (TokenManager, TokenManagerConfig, mkTokenManagerConfig, new, getToken)
import Auth.OAuth2.TokenRefresh (TokenRefreshError (..))
import Auth.OAuth2.Types (OAuth2Error (..), TokenSet (..), mkAccessToken, mkRefreshToken)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import Core
import DateTime qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Auth.OAuth2.TokenManager" do
    describe "mkTokenManagerConfig" do
      it "returns Ok for valid config" \_ -> do
        let result = mkTokenManagerConfig 60 3
        result |> shouldSatisfy isOk

      it "returns Err when leadSeconds is 0" \_ -> do
        let result = mkTokenManagerConfig 0 3
        result |> shouldSatisfy isErr

      it "returns Err when retries is 0" \_ -> do
        let result = mkTokenManagerConfig 60 0
        result |> shouldSatisfy isErr

    describe "getToken" do
      it "returns TokenNotFound when no token stored" \_ -> do
        (_store, manager) <- mkTestManager 60 3
        let mockRefresh _rt = Task.throw (TokenRequestFailed "no refresh")
        result <-
          getToken manager "provider" "user1" mockRefresh
            |> Task.asResult
        case result of
          Err (TokenNotFound userId) -> userId |> shouldBe "user1"
          _ -> fail "Expected TokenNotFound"

      it "returns stored token when not near expiry" \_ -> do
        (store, manager) <- mkTestManager 60 3
        let key = TokenKey "oauth:provider:user2"
        -- Token expires far in the future (year 2099)
        let farFuture = DateTime.fromEpochSeconds (4102444800 :: Int64)
        let tokenSet =
              TokenSet
                { accessToken = mkAccessToken "my-token"
                , refreshToken = Nothing
                , expiresInSeconds = Nothing
                , expiresAt = Just farFuture
                , ttl = Nothing
                }
        store.put key tokenSet
        let mockRefresh _rt = Task.throw (TokenRequestFailed "should not be called")
        result <-
          getToken manager "provider" "user2" mockRefresh
            |> Task.asResult
        case result of
          Ok ts -> ts.accessToken |> shouldBe (mkAccessToken "my-token")
          Err err -> fail [fmt|Expected Ok but got: #{show err}|]

      it "proactively refreshes token near expiry" \_ -> do
        -- proactiveRefreshLeadSeconds = 120 means refresh if < 120s from expiry
        (store, manager) <- mkTestManager 120 3
        let key = TokenKey "oauth:provider:user-near-expiry"
        -- Token expires in 60 seconds — within the 120s lead window
        now <- DateTime.now
        let nearExpiry = DateTime.addSeconds 60 now
        let tokenSet =
              TokenSet
                { accessToken = mkAccessToken "about-to-expire"
                , refreshToken = Just (mkRefreshToken "refresh-me")
                , expiresInSeconds = Just 60
                , expiresAt = Just nearExpiry
                , ttl = Just 60
                }
        store.put key tokenSet
        let mockRefresh _rt =
              Task.yield
                TokenSet
                  { accessToken = mkAccessToken "refreshed-token"
                  , refreshToken = Just (mkRefreshToken "new-refresh")
                  , expiresInSeconds = Just 3600
                  , expiresAt = Nothing
                  , ttl = Just 3600
                  }
        result <-
          getToken manager "provider" "user-near-expiry" mockRefresh
            |> Task.asResult
        case result of
          Ok ts -> ts.accessToken |> shouldBe (mkAccessToken "refreshed-token")
          Err err -> fail [fmt|Expected proactive refresh, got: #{show err}|]


-- | Create an InMemory store and TokenManager with the given config.
-- Extracts the boilerplate of unwrapping mkTokenManagerConfig.
mkTestManager :: Int -> Int -> Task Text (SecretStore, TokenManager)
mkTestManager leadSeconds retries = do
  store <- InMemory.new
  cfg <- unwrapConfig (mkTokenManagerConfig leadSeconds retries)
  manager <- new store cfg
  Task.yield (store, manager)


-- | Unwrap a TokenManagerConfig Result or throw on invalid config.
unwrapConfig :: Result Text TokenManagerConfig -> Task Text TokenManagerConfig
unwrapConfig cfgResult =
  case cfgResult of
    Ok c -> Task.yield c
    Err e -> Task.throw e


isOk :: forall error value. Result error value -> Bool
isOk result =
  case result of
    Ok _ -> True
    Err _ -> False


isErr :: forall error value. Result error value -> Bool
isErr result =
  case result of
    Ok _ -> False
    Err _ -> True
