# External OAuth2 account connections

External OAuth2 connects an authenticated application user to a provider API. Inbound OIDC authentication identifies that user. The provider's OAuth2 flow then stores API credentials separately from application events. See `core/service/Service/Application.hs`, `core/auth/Auth/OAuth2/Routes.hs`, and `core/auth/Auth/SecretStore.hs`.

## Prerequisites and wiring

Configure inbound authentication first. `withAuth` needs an identity provider with `/.well-known/openid-configuration` and a reachable JWKS endpoint. The external API's authorization and token endpoints do not supply those identity requirements by themselves. See `core/auth/Auth/Discovery.hs`, `core/auth/Auth/Jwks.hs`, and [authorization](authorization.md).

This explanatory fragment adds OAuth2 builders to an application that already registers an event store, services, and Web transport. `persistentStore` is a consumer-supplied `SecretStore`. `providerConfig` is an `OAuth2ProviderConfig`.

```haskell ignore
configuredApp =
	baseApp
		|> Application.withAuth @() (\_ -> "https://identity.example.com")
		|> Application.withSecretStore @() (\_ -> persistentStore)
		|> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
		|> Application.withOAuth2Provider @() (\_ -> providerConfig)
```

`withSecretStore` takes a `config -> SecretStore` factory. It does not take a `Task` factory. Initialize effectful store resources in the consumer's owning startup code, then supply the resulting record. The config-dependent form requires the registered config type. See `core/service/Service/Application.hs`.

`withOAuth2StateKey` names the environment variable holding the HMAC secret. The key must contain at least 32 bytes. The builder must precede `withOAuth2Provider`. Provider registration without the state-key setup fails. See `core/service/Service/Application.hs` and `core/auth/Auth/OAuth2/StateToken.hs`.

## Provider configuration

`OAuth2ProviderConfig` contains the provider name and authorization and token endpoints, client ID, client secret, redirect URI, scopes, callback mappings, and browser redirect URLs. The following fragment assumes typed inputs and application-owned callback functions.

```haskell ignore
providerConfig = OAuth2ProviderConfig
	{ provider = externalProvider
	, clientId = externalClientId
	, clientSecret = externalClientSecret
	, redirectUri = callbackUri
	, scopes = requestedScopes
	, onSuccess = connectedAction
	, onFailure = failedAction
	, onDisconnect = disconnectedAction
	, successRedirectUrl = "https://app.example.com/settings/connected"
	, failureRedirectUrl = "https://app.example.com/settings/connection-failed"
	}

connectedAction :: Text -> TokenKey -> Text
failedAction :: Text -> OAuth2Error -> Text
disconnectedAction :: Text -> Text
```

Use `ClientId`, `mkClientSecret`, `Scope`, and the validated result of `mkRedirectUri` for the typed values. Handle a rejected redirect URI during consumer configuration. The provider's registered callback URI must match the application's `/callback/{provider}` URL. See `core/auth/Auth/OAuth2/Types.hs` and `core/auth/Auth/OAuth2/Provider.hs`.

Use the current builders above. The `Application.withOAuth2` setup shown in older Oura examples is absent from the current application API. `integrations/Integration/Oura/Provider.hs` is a maintained example of the provider record shape. `core/service/Service/Application.hs` owns builder signatures.

## Callback commands and token references

The callback mappings return command payload JSON through `Integration.encodeCommand`. A callback command needs `NameOf` and `Json.ToJSON`, plus registration in its service. Define how the callback's subject identifies the application's target entity. The subject is `Text`; do not assume that every identity provider subject parses as a `Uuid`. See `core/auth/Auth/OAuth2/Provider.hs`, `core/auth/Auth/Claims.hs`, and `core/service/Integration.hs`.

The success callback receives the authenticated subject and a `TokenKey`. The framework stores tokens before calling that mapping. The current key is `oauth:{provider}:{subject}`. Preserve the supplied key rather than inventing a different prefix. The `TokenKey` constructor exposes its `Text` for an application-owned serializable reference when required. It has a redacted `Show` instance, so `show tokenKey` is not a serialization format. See `core/auth/Auth/OAuth2/Routes.hs` and `core/auth/Auth/SecretStore.hs`.

This explanatory mapping uses an application-owned `ConnectionRecorded` command with `subject` and `tokenReference` fields. Its routing and decision remain application responsibilities.

```haskell ignore
connectedAction :: Text -> TokenKey -> Text
connectedAction subject tokenKey = case tokenKey of
	TokenKey reference ->
		Integration.encodeCommand ConnectionRecorded
			{ subject = subject
			, tokenReference = reference
			}
```

Store the token reference and connection status in domain events when needed. Keep access tokens, refresh tokens, and client secrets out of events, query rows, logs, and callback command payloads. Token types deliberately have no `ToJSON` instances. See `core/auth/Auth/OAuth2/Types.hs`, `core/auth/Auth/SecretStore.hs`, and `core/service/Integration.hs`.

An integration retrieves credentials through `ActionContext.secretStore` using the token reference. The context also carries validated provider configurations. See `core/service/Integration.hs`. Use the provider integration's current refresh path where applicable. `core/auth/Auth/OAuth2/TokenRefresh.hs` owns refresh behavior.

## Route lifecycle and persistence limits

`GET /connect/{provider}` requires a valid application identity. It creates PKCE values and signed state, then stores the subject and verifier in a server-side transaction. `GET /callback/{provider}?code=...&state=...` validates state and consumes that transaction once. The callback retrieves the subject from the transaction and does not require a fresh inbound JWT. See `core/service/Service/Transport/Web.hs` and `core/auth/Auth/OAuth2/Routes.hs`.

After a successful exchange, the route stores credentials and dispatches the success command. A failed success-command dispatch returns an error instead of redirecting to the success URL. The token write and domain command are separate operations. See `core/auth/Auth/OAuth2/Routes.hs` and `core/service/Service/Transport/Web.hs`.

`POST /disconnect/{provider}` requires authentication. The route requests deletion of the provider's token key, then dispatches the disconnect command. The current route ignores a store deletion error. The disconnect response therefore does not prove credential erasure or provider-side revocation. Build consumer verification around those requirements. See `core/auth/Auth/OAuth2/Routes.hs` and `core/service/Service/Transport/Web.hs`.

The shipped `SecretStore` implementation is in memory. Supply a persistent consumer-owned implementation for durable credentials. Implement all five record operations, including both atomic modification functions, with their stated per-key semantics. The framework does not ship Vault, cloud secret manager, or encrypted-database adapters. See `core/auth/Auth/SecretStore.hs`, `core/auth/Auth/SecretStore/InMemory.hs`, and `core/service/Service/Application.hs`.

A persistent token store does not make pending connections durable. Startup currently creates an `InMemoryTransactionStore` with no application builder for a replacement. Pending callback state disappears on restart and is not shared across instances. A deployment must preserve routing to the initiating process for a pending flow. See `core/service/Service/Application.hs` and `core/auth/Auth/OAuth2/TransactionStore.hs`.

The maintained route specs cover connect generation, invalid or replayed state, and disconnect behavior. They do not complete a successful callback token exchange against a fake provider. No testbed OAuth2 scenario supplies that missing evidence. See `core/test/Auth/OAuth2/RoutesSpec.hs` and `testbed/tests`. Verify a new provider with a deterministic fake endpoint and assert token persistence, the callback command's domain result, and disconnect deletion.
