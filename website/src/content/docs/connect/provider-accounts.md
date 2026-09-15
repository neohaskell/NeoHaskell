---
title: Connect a merchant's provider account
description: Link an external account while keeping consent, credentials, and business outcomes explicit.
sidebar:
  order: 3
---

The merchant wants the shop to send accepted orders to their accounting service. They should authorise that connection without giving the shop their accounting password. OAuth2 provides a consent flow and credentials for subsequent access.

This differs from customer sign-in. The shop's JWT authentication identifies the merchant using your application; the provider's OAuth2 tokens authorise your application to access an external account. Connecting that account still does not implement invoice creation or order export.

Start with [application access control](/build/access-control/) and [integration outcomes](/connect/). Choose the minimum provider permissions needed for the actual feature.

## Establish provider compatibility

The core supplies a configurable authorization-code flow with PKCE, not an accounting-provider preset. A `Provider` contains `name`, `authorizeEndpoint`, and `tokenEndpoint`. Confirm the selected provider supports the client's actual exchange format: form parameters include `client_id`, `client_secret`, and the PKCE verifier. Provider-specific scopes, additional authorization parameters, and API operations need their own compatibility work.

Startup validates provider endpoints for HTTPS and network-address restrictions and rejects duplicate names. Build the callback URI with `OAuth2.mkRedirectUri`, handle its `Result`, and register that same URI with the provider. HTTPS is required except for supported localhost development addresses.

## Wire the account connection

This is a **partial application builder**. The surrounding application must already register its transport, business services, and typed configuration. `identityServerUrl`, `accountProviderConfig`, and `existingSecretStore` are values you supply:

```haskell
    |> Application.withAuth @() (\_ -> identityServerUrl)
    |> Application.withSecretStore @() (\_ -> existingSecretStore)
    |> Application.withOAuth2StateKey "SHOP_OAUTH_STATE_KEY"
    |> Application.withOAuth2Provider @() (\_ -> accountProviderConfig)
```

The state-key setting names an environment variable containing a secret of at least 32 bytes. Set it before startup. `withOAuth2StateKey` must precede provider registration. For configuration-dependent values, replace `@()` factories with functions from the configuration type registered by `Application.withConfig`.

Both `withSecretStore` and `withOAuth2Provider` accept **factory functions**, not a store or provider record directly. A store requiring startup work must be constructed through your startup design before returning its handle from the factory; this API does not accept a `Task` as the factory result.

The provider configuration has this **record-construction fragment**:

```haskell
OAuth2ProviderConfig
  { provider = selectedProvider
  , clientId = registeredClientId
  , clientSecret = registeredClientSecret
  , redirectUri = validatedCallbackUri
  , scopes = requestedScopes
  , onSuccess = encodeConnected
  , onFailure = encodeConnectionFailure
  , onDisconnect = encodeDisconnected
  , successRedirectUrl = connectedPage
  , failureRedirectUrl = failedPage
  }
```

Import `OAuth2ProviderConfig (..)` from `Auth.OAuth2.Provider`. Client IDs, secrets, redirect URIs, and scopes use the types in `Auth.OAuth2.Types`; use its smart constructors for secrets and validated redirect URIs. Keep credentials in [secret configuration](/build/configuration/).

## Follow the three routes

| Intended request | What happens |
| --- | --- |
| `GET /connect/{provider}` | Authenticates the merchant and redirects to provider consent |
| `GET /callback/{provider}?code=…&state=…` | Checks signed state, consumes the saved transaction, and exchanges the code |
| `POST /disconnect/{provider}` | Authenticates the merchant and attempts local token deletion |

The connect route accepts a bearer header and has a query-token fallback for browser redirects. Prefer the header where feasible; prevent token-bearing URLs from entering application or proxy logs. The callback uses the signed state and saved transaction rather than requiring a JWT from the provider's redirect.

State expires after five minutes. Its transaction retains the merchant identity and PKCE verifier server-side and is consumed once. A failed exchange therefore requires starting a fresh connection rather than replaying the same callback.

## Turn consent into a visible business outcome

After successful exchange, tokens are stored before `onSuccess` receives the merchant user ID and a `TokenKey`. Each callback returns JSON text in `Integration.CommandPayload` format; construct that with `Integration.encodeCommand` around a registered application command.

`encodeConnected` handles `Text -> TokenKey -> Text`; `encodeConnectionFailure` handles `Text -> OAuth2Error -> Text`; `encodeDisconnected` handles `Text -> Text`. Their resulting commands can be different types because the callback boundary is encoded text. Keep raw tokens out of command payloads and events. Let the connection command record the application association and a suitable reference, then expose its outcome through a query.

Do not treat arrival at a success URL as proof that order export works. Test callback command dispatch and a real provider API operation separately. Errors before exchange, and consent-denial redirects lacking `code`, do not necessarily call `onFailure`; the current web callback expects both `code` and `state`.

## Plan the credential lifetime

The default secret store is in memory. Implement and supply durable secret storage before promising that connections survive restart. The current application also creates an in-memory transaction store: restarting during consent loses the transaction, and multiple instances need deliberate callback routing or a different transaction-store integration.

`TokenRefresh.withValidToken` is an explicit helper for adapter authors. It reads stored tokens, executes the supplied action, and refreshes on an error identified by the caller's unauthorized predicate. It stores refreshed tokens and retries the action once. It does not proactively schedule refresh from `expiresInSeconds`; its per-key refresh locks are local to the process.

Missing tokens, missing refresh tokens, or a failed refresh need a reconnect outcome. Account disconnection currently attempts local deletion but ignores deletion errors and does not call a provider revocation endpoint. Verify deletion and implement provider revocation where the product requires it; “disconnected” is not evidence of remote revocation.

## Exercise: consent interrupted

Ask your agent to demonstrate connection, a tampered or replayed state, consent denial, restart during consent, refresh failure, and disconnect with a failing secret store. Then explain what the merchant sees in each case.

The public route and refresh tests provide examples with controlled dependencies. They do not certify a particular accounting provider. Record that provider's sandbox verification separately, then build the actual [provider adapter](/connect/custom-integrations/).

## Implementation and examples

- [Application wiring](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs)
- [Provider configuration](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/OAuth2/Provider.hs)
- [OAuth2 types and URI validation](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/OAuth2/Types.hs)
- [Client exchange format](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/OAuth2/Client.hs)
- [Route lifecycle](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/OAuth2/Routes.hs)
- [HTTP route wiring](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Transport/Web.hs)
- [Secret-store interface](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/SecretStore.hs)
- [Refresh helper](https://github.com/neohaskell/NeoHaskell/blob/main/core/auth/Auth/OAuth2/TokenRefresh.hs)
- [Route tests](https://github.com/neohaskell/NeoHaskell/blob/main/core/test/Auth/OAuth2/RoutesSpec.hs)
- [Refresh tests](https://github.com/neohaskell/NeoHaskell/blob/main/core/test/Auth/OAuth2/TokenRefreshSpec.hs)
