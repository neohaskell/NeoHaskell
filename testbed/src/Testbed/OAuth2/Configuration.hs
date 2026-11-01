-- | Build the OAuth testbed's persistence and provider settings.
-- Pass these factories to its named Event Model.
module Testbed.OAuth2.Configuration (makeOAuthPostgresConfig, providerConfig, oauthApiInfo) where

import Auth.OAuth2.Provider (OAuth2ProviderConfig (..))
import Auth.OAuth2.Types (ClientId (..), Provider (..), Scope (..), mkClientSecret, mkRedirectUri)
import Auth.SecretStore (TokenKey (..))
import Core
import Integration qualified
import Result qualified as Result
import Service.Application (ApiInfo (..))
import Service.EventStore.Postgres (PostgresEventStore (..))
import Testbed.AccountConnection.Commands.RecordAccountConnection (
  RecordAccountConnection (..),
  RecordAccountConnectionAction (..),
 )
import Testbed.Config (TestbedConfig)
import Testbed.Infrastructure (makePostgresConfig)


makeOAuthPostgresConfig :: TestbedConfig -> PostgresEventStore
makeOAuthPostgresConfig config =
  (makePostgresConfig config) {erasableApplicationScope = "testbed-oauth"}


oauthApiInfo :: ApiInfo
oauthApiInfo =
  ApiInfo
    { apiTitle = "OAuth2 Testbed API",
      apiVersion = "1.0.0",
      apiDescription = "Isolated authenticated API for deterministic OAuth2 testbed scenarios"
    }


providerConfig :: Int -> Int -> OAuth2ProviderConfig
providerConfig oauthPort fixturePort =
  OAuth2ProviderConfig
    { provider =
        Provider
          { name = "test-provider",
            authorizeEndpoint = [fmt|http://127.0.0.1:#{fixturePort}/oauth/authorize|],
            tokenEndpoint = [fmt|http://127.0.0.1:#{fixturePort}/oauth/token|]
          },
      clientId = ClientId "testbed-client",
      clientSecret = mkClientSecret "testbed-fake-client-secret",
      redirectUri = case mkRedirectUri [fmt|http://localhost:#{oauthPort}/callback/test-provider|] of
        Result.Ok uri -> uri
        Result.Err _ -> panic "Invalid testbed OAuth2 callback URL",
      scopes = [Scope "account:read"],
      onSuccess = \userId tokenKey ->
        Integration.encodeCommand
          RecordAccountConnection
            { action =
                RecordConnected
                  { userId = userId,
                    tokenKeyMatches = tokenKey == TokenKey [fmt|oauth:test-provider:#{userId}|]
                  }
            },
      onFailure = \userId _error ->
        Integration.encodeCommand
          RecordAccountConnection
            { action = RecordFailed {userId = userId}
            },
      onDisconnect = \userId ->
        Integration.encodeCommand
          RecordAccountConnection
            { action = RecordDisconnected {userId = userId}
            },
      successRedirectUrl = [fmt|http://127.0.0.1:#{oauthPort}/oauth/success|],
      failureRedirectUrl = [fmt|http://127.0.0.1:#{oauthPort}/oauth/failure|]
    }
