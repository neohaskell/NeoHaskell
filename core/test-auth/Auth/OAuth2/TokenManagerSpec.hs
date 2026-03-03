module Auth.OAuth2.TokenManagerSpec (spec) where

import Auth.OAuth2.TokenManager (mkTokenManagerConfig, new, getToken)
import Auth.OAuth2.TokenRefresh (TokenRefreshError (..))
import Auth.OAuth2.Types (OAuth2Error (..), TokenSet (..), mkAccessToken)
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
        store <- InMemory.new
        let cfgResult = mkTokenManagerConfig 60 3
        cfg <- case cfgResult of
          Ok c -> Task.yield c
          Err e -> Task.throw e
        manager <- new store cfg
        let mockRefresh _rt = Task.throw (TokenRequestFailed "no refresh")
        result <-
          getToken manager "provider" "user1" mockRefresh
            |> Task.asResult
        case result of
          Err (TokenNotFound userId) -> userId |> shouldBe "user1"
          _ -> fail "Expected TokenNotFound"

      it "returns stored token when not near expiry" \_ -> do
        store <- InMemory.new
        let cfgResult = mkTokenManagerConfig 60 3
        cfg <- case cfgResult of
          Ok c -> Task.yield c
          Err e -> Task.throw e
        manager <- new store cfg
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
