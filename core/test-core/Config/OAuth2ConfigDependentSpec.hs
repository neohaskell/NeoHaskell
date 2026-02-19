-- | Tests for Application.withOAuth2Provider config-dependent factory API.
--
-- These tests verify that withOAuth2Provider supports the same unified
-- factory-based API as withEventStore, withAuth, and withFileUpload,
-- allowing OAuth2 provider credentials to come from the Config system.
module Config.OAuth2ConfigDependentSpec where

import Core
import Array qualified
import Auth.OAuth2.Provider (OAuth2ProviderConfig (..))
import Auth.OAuth2.Types (Provider (..), ClientId (..), mkClientSecret, RedirectUri, mkRedirectUri, Scope (..))
import Service.Application qualified as Application
import Service.EventStore.InMemory qualified as InMemory
import Task qualified
import Test
import Text qualified


-- | Mock config type for testing.
data MockConfig = MockConfig
  { mockClientId :: Text
  , mockClientSecret :: Text
  }


-- | Helper to create a RedirectUri in tests, panics if invalid.
-- Only use with known-valid HTTPS test URLs.
testRedirectUri :: Text -> RedirectUri
testRedirectUri url =
  case mkRedirectUri url of
    Ok uri -> uri
    Err err -> panic [fmt|Test URL should be valid: #{url}, error: #{toText err}|]


-- | Helper to create a static OAuth2ProviderConfig for testing.
staticProviderConfig :: Text -> OAuth2ProviderConfig
staticProviderConfig name = OAuth2ProviderConfig
  { provider = Provider
      { name = name
      , authorizeEndpoint = "https://example.com/authorize"
      , tokenEndpoint = "https://example.com/token"
      }
  , clientId = ClientId "static-client-id"
  , clientSecret = mkClientSecret "static-client-secret"
  , redirectUri = testRedirectUri "https://myapp.com/callback"
  , scopes = [Scope "read"]
  , onSuccess = \_userId _tokenKey -> "success"
  , onFailure = \_userId _err -> "failure"
  , onDisconnect = \_userId -> "disconnect"
  , successRedirectUrl = "https://myapp.com/success"
  , failureRedirectUrl = "https://myapp.com/failure"
  }


-- | Factory function that creates OAuth2ProviderConfig from MockConfig.
makeProviderFactory :: MockConfig -> OAuth2ProviderConfig
makeProviderFactory cfg = OAuth2ProviderConfig
  { provider = Provider
      { name = "test-provider"
      , authorizeEndpoint = "https://example.com/authorize"
      , tokenEndpoint = "https://example.com/token"
      }
  , clientId = ClientId cfg.mockClientId
  , clientSecret = mkClientSecret cfg.mockClientSecret
  , redirectUri = testRedirectUri "https://myapp.com/callback"
  , scopes = [Scope "read"]
  , onSuccess = \_userId _tokenKey -> "success"
  , onFailure = \_userId _err -> "failure"
  , onDisconnect = \_userId -> "disconnect"
  , successRedirectUrl = "https://myapp.com/success"
  , failureRedirectUrl = "https://myapp.com/failure"
  }


spec :: Spec Unit
spec = do
  describe "Application.withOAuth2Provider (config-dependent API)" do
    it "stores factory with config-dependent provider" \_ -> do
      let app = Application.new
            |> Application.withOAuth2StateKey "TEST_KEY"
            |> Application.withOAuth2Provider makeProviderFactory
      case app.oauth2Setup of
        Nothing -> fail "Expected oauth2Setup to be Just"
        Just setup -> do
          let providerCount = Array.length setup.providers
          providerCount |> shouldBe 1

    it "works with static config using @() pattern" \_ -> do
      let app = Application.new
            |> Application.withOAuth2StateKey "TEST_KEY"
            |> Application.withOAuth2Provider @() (\_ -> staticProviderConfig "static-provider")
      case app.oauth2Setup of
        Nothing -> fail "Expected oauth2Setup to be Just"
        Just setup -> do
          let providerCount = Array.length setup.providers
          providerCount |> shouldBe 1

    it "supports multiple providers (additive)" \_ -> do
      let app = Application.new
            |> Application.withOAuth2StateKey "TEST_KEY"
            |> Application.withOAuth2Provider @() (\_ -> staticProviderConfig "provider-1")
            |> Application.withOAuth2Provider @() (\_ -> staticProviderConfig "provider-2")
      case app.oauth2Setup of
        Nothing -> fail "Expected oauth2Setup to be Just"
        Just setup -> do
          let providerCount = Array.length setup.providers
          providerCount |> shouldBe 2

    it "supports mixing static and config-dependent providers" \_ -> do
      let app = Application.new
            |> Application.withOAuth2StateKey "TEST_KEY"
            |> Application.withOAuth2Provider @() (\_ -> staticProviderConfig "static")
            |> Application.withOAuth2Provider makeProviderFactory
      case app.oauth2Setup of
        Nothing -> fail "Expected oauth2Setup to be Just"
        Just setup -> do
          let providerCount = Array.length setup.providers
          providerCount |> shouldBe 2

  describe "runWith rejects deferred OAuth2 providers" do
    it "rejects config-dependent OAuth2 provider in runWith" \_ -> do
      eventStore <- InMemory.new |> Task.mapError toText
      let app = Application.new
            |> Application.withOAuth2StateKey "TEST_KEY"
            |> Application.withOAuth2Provider makeProviderFactory
      result <- Application.runWith eventStore app |> Task.asResult
      case result of
        Ok _ -> fail "Expected error but got Ok"
        Err err -> err |> shouldSatisfy (Text.contains "runWith does not support config-dependent OAuth2")
