module OAuth2App (app) where

import App (makePostgresConfig)
import Auth.OAuth2.Provider (OAuth2ProviderConfig (..))
import Auth.OAuth2.Types (ClientId (..), Provider (..), Scope (..), mkClientSecret, mkRedirectUri)
import Auth.SecretStore (TokenKey (..))
import Core
import Integration qualified
import Result qualified as Result
import Service.Application (ApiInfo (..), Application)
import Service.Application qualified as Application
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Testbed.AccountConnection.Commands.RecordAccountConnection (
  RecordAccountConnection (..),
  RecordAccountConnectionAction (..),
 )
import Testbed.AccountConnection.Outbound (AccountTokenPresenceObserver)
import Testbed.AccountConnection.Query (AccountConnectionSummary)
import Testbed.AccountConnection.Service qualified as AccountConnectionService
import Testbed.Config (TestbedConfig)
import Testbed.Erasable.Owned.Outbound (OwnedErasureOutbound)
import Testbed.Erasable.Owned.Query (OwnedProtectedView)
import Testbed.Erasable.Owned.Service qualified as OwnedErasableService


app :: Application
app =
  Application.new
    |> Application.withConfig @TestbedConfig
    |> Application.withEventStore makeOAuthPostgresConfig
    |> Application.withTransport (WebTransport.server {WebTransport.port = 8081})
    |> Application.withApiInfo @() (\_ -> oauthApiInfo)
    |> Application.withService AccountConnectionService.service
    |> Application.withService OwnedErasableService.service
    |> Application.withQuery @AccountConnectionSummary
    |> Application.withQuery @OwnedProtectedView
    |> Application.withAuth @() (\_ -> "http://127.0.0.1:8181/oidc")
    |> Application.withOAuth2StateKey "OAUTH2_TESTBED_STATE_KEY"
    |> Application.withOAuth2Provider @() (\_ -> providerConfig)
    |> Application.withOutbound @AccountTokenPresenceObserver
    |> Application.withOutbound @OwnedErasureOutbound


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


providerConfig :: OAuth2ProviderConfig
providerConfig =
  OAuth2ProviderConfig
    { provider =
        Provider
          { name = "test-provider",
            authorizeEndpoint = "http://127.0.0.1:8181/oauth/authorize",
            tokenEndpoint = "http://127.0.0.1:8181/oauth/token"
          },
      clientId = ClientId "testbed-client",
      clientSecret = mkClientSecret "testbed-fake-client-secret",
      redirectUri = case mkRedirectUri "http://localhost:8081/callback/test-provider" of
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
      successRedirectUrl = "http://127.0.0.1:8081/oauth/success",
      failureRedirectUrl = "http://127.0.0.1:8081/oauth/failure"
    }
