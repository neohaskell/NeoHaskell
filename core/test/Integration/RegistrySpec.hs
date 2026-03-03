module Integration.RegistrySpec where

import Array qualified
import Auth.OAuth2.Provider (ValidatedOAuth2ProviderConfig (..))
import Auth.OAuth2.Types
  ( ClientId (..)
  , Provider (..)
  , Scope (..)
  , mkClientSecret
  , mkRedirectUri
  , unsafeValidatedProvider
  )
import Core
import Integration qualified
import Map qualified
import Task qualified
import Test



spec :: Spec Unit
spec = do
  describe "Integration.ImmutableProviderRegistry" do
    it "lookup returns Nothing for empty registry" \_ -> do
      let registry = Integration.fromMap Map.empty
      case Integration.lookup "any-provider" registry of
        Nothing -> Task.yield ()
        Just _ -> Test.fail "expected Nothing for empty registry but got Just"

    it "lookup returns Just config for existing provider" \_ -> do
      config <- makeTestConfig
      let registry = Integration.fromMap (Map.fromArray (Array.fromLinkedList [("test-provider", config)]))
      case Integration.lookup "test-provider" registry of
        Nothing -> Test.fail "expected Just config but got Nothing"
        Just foundConfig -> foundConfig.clientId |> shouldBe (ClientId "test-client-id")

    it "lookup returns Nothing for missing provider" \_ -> do
      config <- makeTestConfig
      let registry = Integration.fromMap (Map.fromArray (Array.fromLinkedList [("test-provider", config)]))
      case Integration.lookup "missing-provider" registry of
        Nothing -> Task.yield ()
        Just _ -> Test.fail "expected Nothing for missing provider but got Just"

    it "fromMap creates registry with all entries from map" \_ -> do
      config <- makeTestConfig
      let providerMap = Map.fromArray (Array.fromLinkedList [("test-provider", config)])
      let registry = Integration.fromMap providerMap
      let entryCount = registry |> Integration.entries |> Array.length
      entryCount |> shouldBe 1


makeTestConfig :: Task Text ValidatedOAuth2ProviderConfig
makeTestConfig = do
  let provider =
        Provider
          { name = "test-provider"
          , authorizeEndpoint = "https://example.com/authorize"
          , tokenEndpoint = "https://example.com/token"
          }
  let validatedProvider = unsafeValidatedProvider provider
  redirectUri <- case mkRedirectUri "https://example.com/callback" of
    Ok uri -> Task.yield uri
    Err _ -> Task.throw ("Failed to create redirect URI" :: Text)
  Task.yield
    ValidatedOAuth2ProviderConfig
      { provider = provider
      , validatedProvider = validatedProvider
      , clientId = ClientId "test-client-id"
      , clientSecret = mkClientSecret "test-client-secret"
      , redirectUri = redirectUri
      , scopes = Array.fromLinkedList [Scope "read"]
      , onSuccess = \_userId _tokenKey -> "{}"
      , onFailure = \_userId _err -> "{}"
      , onDisconnect = \_userId -> "{}"
      , successRedirectUrl = "https://example.com/success"
      , failureRedirectUrl = "https://example.com/failure"
      }
