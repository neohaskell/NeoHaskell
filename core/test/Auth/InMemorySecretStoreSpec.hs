-- | Tests for the in-memory SecretStore implementation.
--
-- Runs the shared SecretStore test suite against InMemory.new.
module Auth.InMemorySecretStoreSpec where

import Auth.OAuth2.Types (TokenSet (..), mkAccessToken, mkRefreshToken)
import Auth.SecretStore (SecretStore (..), TokenKey (..))
import Auth.SecretStore.InMemory qualified as InMemory
import Core
import DateTime qualified
import Task qualified
import Test
import Test.Auth.SecretStore qualified as SecretStore


spec :: Spec Unit
spec = do
  let newStore = InMemory.new |> Task.mapError toText
  SecretStore.spec newStore

  describe "TTL-aware TokenSet storage" do
    it "preserves expiresAt and ttl fields on retrieval" \_ -> do
      store <- InMemory.new |> Task.mapError toText
      let key = TokenKey "oauth:provider:user-ttl"
      let expectedExpiry = DateTime.fromEpochSeconds (1735689600 :: Int64)
      let tokenSet =
            TokenSet
              { accessToken = mkAccessToken "ttl-access-token"
              , refreshToken = Just (mkRefreshToken "ttl-refresh-token")
              , expiresInSeconds = Just (3600 :: Int)
              , expiresAt = Just expectedExpiry
              , ttl = Just (3600 :: Int)
              }

      store.put key tokenSet
      retrieved <- store.get key

      case retrieved of
        Just storedTokens -> do
          storedTokens.expiresAt |> shouldBe (Just expectedExpiry)
          storedTokens.ttl |> shouldBe (Just (3600 :: Int))
        Nothing -> fail "Expected TokenSet to be present"
