-- | OAuth2 provider configuration for Oura Ring API integration.
--
-- This module provides the OAuth2 configuration needed to enable users
-- to connect their Oura accounts via the `/connect/oura` endpoint.
--
-- = Oura OAuth2 Scopes
--
-- * @daily@: Sleep, activity, readiness data
-- * @heartrate@: Heart rate data
--
-- See: https://cloud.ouraring.com/docs/authentication#oauth2-scopes
module Integration.Oura.Provider (
  ouraProvider,
  makeOuraConfig,
) where

import Array qualified
import Auth.OAuth2.Provider (OAuth2ProviderConfig (..))
import Auth.OAuth2.Types (ClientId, ClientSecret, OAuth2Error, Provider (..), RedirectUri, Scope (..))
import Auth.SecretStore (TokenKey)
import Text (Text)


-- | Oura Ring OAuth2 provider configuration.
--
-- Contains the endpoints needed for the Authorization Code flow.
ouraProvider :: Provider
ouraProvider =
  Provider
    { name = "oura"
    , authorizeEndpoint = "https://cloud.ouraring.com/oauth/authorize"
    , tokenEndpoint = "https://api.ouraring.com/oauth/token"
    }


-- | Create an OAuth2ProviderConfig for Oura Ring integration.
--
-- This function constructs the complete OAuth2 configuration needed to
-- handle the OAuth2 flow for Oura Ring API connections.
--
-- = Parameters
--
-- * @clientId@: OAuth2 client ID from Oura dashboard
-- * @clientSecret@: OAuth2 client secret from Oura dashboard (keep secure!)
-- * @redirectUri@: Callback URL (must match Oura dashboard configuration)
-- * @onSuccess@: Callback when token exchange succeeds (userId -> tokenKey -> action JSON)
-- * @onFailure@: Callback when OAuth2 flow fails (userId -> error -> action JSON)
-- * @onDisconnect@: Callback when user disconnects the provider (userId -> action JSON)
-- * @successRedirectUrl@: URL to redirect user after successful connection
-- * @failureRedirectUrl@: URL to redirect user after failed connection
makeOuraConfig ::
  ClientId ->
  ClientSecret ->
  RedirectUri ->
  (Text -> TokenKey -> Text) ->
  (Text -> OAuth2Error -> Text) ->
  (Text -> Text) ->
  Text ->
  Text ->
  OAuth2ProviderConfig
makeOuraConfig clientId clientSecret redirectUri onSuccess onFailure onDisconnect successUrl failureUrl =
  OAuth2ProviderConfig
    { provider = ouraProvider
    , clientId = clientId
    , clientSecret = clientSecret
    , redirectUri = redirectUri
    , scopes = Array.fromLinkedList [Scope "daily", Scope "heartrate"]
    , onSuccess = onSuccess
    , onFailure = onFailure
    , onDisconnect = onDisconnect
    , successRedirectUrl = successUrl
    , failureRedirectUrl = failureUrl
    }
