# ADR-0033: Config-Dependent OAuth2 Provider Factory

## Status

Proposed

## Context

The Application builder provides config-dependent factories for `withEventStore`, `withAuth`, and `withFileUpload`. These allow users to pass a function `(config -> X)` that is stored at build time and evaluated at runtime after `.env` and config are loaded:

```haskell
app = Application.new
  |> Application.withConfig @AppConfig
  |> Application.withEventStore (\cfg -> makePostgresConfig cfg)
  |> Application.withAuth (\cfg -> cfg.authServerUrl)
  |> Application.withFileUpload (\cfg -> makeFileUploadConfig cfg)
```

However, `withOAuth2Provider` only accepts a static `OAuth2ProviderConfig`:

```haskell
withOAuth2Provider :: OAuth2ProviderConfig -> Application -> Application
```

This means OAuth2 client credentials (client ID, client secret, redirect URIs) cannot come from the Config system or environment variables loaded via `.env`. The provider config is evaluated at app-definition time, before `Application.run` loads `.env` and resolves the config.

Users must resort to `unsafePerformIO` + `System.Environment.getEnv` to read env vars at top level, which is fragile and races with `.env` loading.

### Requirements

- OAuth2 provider credentials must be loadable from the Config system
- The API must be consistent with `withEventStore`, `withAuth`, and `withFileUpload`
- Multiple providers must still be supported (additive, not last-write-wins)
- Static config must still work via the `@()` pattern
- `runWith` must reject deferred OAuth2 providers with a clear error message

## Decision

Add an `OAuth2ProviderFactory` GADT following the established pattern:

```haskell
data OAuth2ProviderFactory where
  EvaluatedOAuth2Provider :: OAuth2ProviderConfig -> OAuth2ProviderFactory
  DeferredOAuth2Provider :: (Typeable cfg) => (cfg -> OAuth2ProviderConfig) -> OAuth2ProviderFactory
```

Change `OAuth2Setup.providers` from `Array OAuth2ProviderConfig` to `Array OAuth2ProviderFactory`.

Change `withOAuth2Provider` signature to accept a factory function:

```haskell
withOAuth2Provider ::
  forall config.
  (Typeable config) =>
  (config -> OAuth2ProviderConfig) ->
  Application ->
  Application
```

Usage:

```haskell
-- Static config (no app config needed):
app = Application.new
  |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
  |> Application.withOAuth2Provider @() (\_ -> ouraConfig)

-- Config-dependent (uses app config):
app = Application.new
  |> Application.withConfig @AppConfig
  |> Application.withOAuth2StateKey "OAUTH2_STATE_KEY"
  |> Application.withOAuth2Provider (\cfg -> makeOuraConfig cfg.ouraClientId cfg.ouraClientSecret)
```

### Runtime Resolution

In `Application.run`, after config is loaded (step 2), resolve all OAuth2 provider factories before passing to `buildProviderMap`:

- `EvaluatedOAuth2Provider config` -> use directly
- `DeferredOAuth2Provider mkConfig` -> call `mkConfig` with loaded config

In `runWith`, reject any `DeferredOAuth2Provider` with a clear error message, matching the pattern for `DeferredFileUpload` and `DeferredWebAuth`.

## Consequences

### Positive

- Consistent API across all Application builder functions
- OAuth2 credentials can come from `.env` files and the Config system
- No need for `unsafePerformIO` workarounds
- Type-safe: config type mismatch is a compile error
- Backward compatible: static configs work via `@()` pattern

### Negative

- Slightly more complex internal implementation (GADT + resolution step)
- Breaking change: existing `withOAuth2Provider ouraConfig` calls must become `withOAuth2Provider @() (\_ -> ouraConfig)`

### Risks and Mitigations

- **Risk**: Users forget `withConfig` when using deferred factories
  - **Mitigation**: Clear error message at runtime, matching existing pattern
- **Risk**: Breaking change for existing users
  - **Mitigation**: The `@()` pattern is well-documented and consistent with other builder functions
