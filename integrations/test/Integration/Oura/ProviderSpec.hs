-- | Tests for Oura Ring OAuth2 provider configuration.
module Integration.Oura.ProviderSpec (spec) where

import Array qualified
import Auth.OAuth2.Provider (OAuth2ProviderConfig (..))
import Auth.OAuth2.Types (ClientId (..), Provider (..), RedirectUri, Scope (..), mkClientSecret, mkRedirectUri)
import Basics (panic, (|>))
import Integration.Oura.Provider (makeOuraConfig, ouraProvider)
import LinkedList qualified
import Result (Result (..))
import Test.Hspec (Spec, describe, it, shouldBe)
import Text (Text)


-- | Helper to create redirect URI for tests (panics on invalid URL)
testRedirectUri :: Text -> RedirectUri
testRedirectUri url =
  case mkRedirectUri url of
    Ok uri -> uri
    Err _ -> panic "Invalid redirect URI in test"


spec :: Spec
spec = do
  describe "Oura.Provider" do
    describe "ouraProvider" do
      it "has correct name" do
        case ouraProvider of
          Provider name _ _ -> name `shouldBe` ("oura" :: Text)

      it "has correct authorize endpoint" do
        case ouraProvider of
          Provider _ authorizeEndpoint _ -> authorizeEndpoint `shouldBe` ("https://cloud.ouraring.com/oauth/authorize" :: Text)

      it "has correct token endpoint" do
        case ouraProvider of
          Provider _ _ tokenEndpoint -> tokenEndpoint `shouldBe` ("https://api.ouraring.com/oauth/token" :: Text)

    describe "makeOuraConfig" do
      it "creates config with all fields populated" do
        let clientId = ClientId "test-client-id"
        let clientSecret = mkClientSecret "test-secret"
        let redirectUri = testRedirectUri "https://localhost:3000/callback"
        let onSuccess = \_ _ -> ("success" :: Text)
        let onFailure = \_ _ -> ("failure" :: Text)
        let onDisconnect = \_ -> ("disconnect" :: Text)
        let successUrl = ("https://localhost:3000/success" :: Text)
        let failureUrl = ("https://localhost:3000/failure" :: Text)

        let config = makeOuraConfig clientId clientSecret redirectUri onSuccess onFailure onDisconnect successUrl failureUrl

        case config.provider of
          Provider name _ _ -> name `shouldBe` ("oura" :: Text)
        config.clientId `shouldBe` clientId
        config.successRedirectUrl `shouldBe` successUrl
        config.failureRedirectUrl `shouldBe` failureUrl

      it "includes both required scopes" do
        let clientId = ClientId "test-client-id"
        let clientSecret = mkClientSecret "test-secret"
        let redirectUri = testRedirectUri "https://localhost:3000/callback"
        let onSuccess = \_ _ -> ("" :: Text)
        let onFailure = \_ _ -> ("" :: Text)
        let onDisconnect = \_ -> ("" :: Text)

        let config = makeOuraConfig clientId clientSecret redirectUri onSuccess onFailure onDisconnect ("" :: Text) ("" :: Text)

        let scopeNames = config.scopes |> Array.map (\(Scope name) -> name) |> Array.toLinkedList

        scopeNames `shouldBe` (["daily", "heartrate"] :: LinkedList.LinkedList Text)

      it "has correct provider endpoints in config" do
        let clientId = ClientId "test-client-id"
        let clientSecret = mkClientSecret "test-secret"
        let redirectUri = testRedirectUri "https://localhost:3000/callback"

        let config = makeOuraConfig clientId clientSecret redirectUri (\_ _ -> ("" :: Text)) (\_ _ -> ("" :: Text)) (\_ -> ("" :: Text)) ("" :: Text) ("" :: Text)

        case config.provider of
          Provider _ authorizeEndpoint tokenEndpoint -> do
            authorizeEndpoint `shouldBe` ("https://cloud.ouraring.com/oauth/authorize" :: Text)
            tokenEndpoint `shouldBe` ("https://api.ouraring.com/oauth/token" :: Text)
