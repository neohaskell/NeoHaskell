# ADR-0035: Config-Dependent Application Builder Functions

## Status

Accepted

## Context

The Application builder provides config-dependent factories for five `withXXX` functions. These allow users to pass a function `(config -> X)` that is stored at build time and evaluated at runtime after `.env` and config are loaded:

```haskell
app = Application.new
  |> Application.withConfig @AppConfig
  |> Application.withEventStore (\cfg -> makePostgresConfig cfg)
  |> Application.withAuth (\cfg -> cfg.authServerUrl)
  |> Application.withFileUpload (\cfg -> makeFileUploadConfig cfg)
  |> Application.withOAuth2Provider (\cfg -> makeOuraConfig cfg.ouraClientId cfg.ouraClientSecret)
```

The five functions that already support this pattern are:

- `withEventStore` (uses `EventStoreFactory` ADT: `EvaluatedEventStore` / `DeferredEventStore`)
- `withFileUpload` (uses `FileUploadFactory` ADT: `EvaluatedFileUpload` / `DeferredFileUpload`)
- `withAuth` (uses `WebAuthFactory` ADT: `EvaluatedWebAuth` / `DeferredWebAuth`)
- `withAuthOverrides` (same factory ADT as `withAuth`)
- `withOAuth2Provider` (uses `OAuth2ProviderFactory` ADT: `EvaluatedOAuth2Provider` / `DeferredOAuth2Provider`)

The remaining builder functions accept only static values:

- `withOutbound` — needs config for external service IDs, API URLs
- `withOutboundLifecycle` — needs config for connection strings, pool sizes
- `withInbound` — needs config for timer intervals, webhook URLs
- `withService` — needs config for feature flags, limits
- `withTransport` — needs config for port, host, TLS settings
- `withCors` — needs config for allowed origins per environment
- `withApiInfo` — needs config for version from build config
- `withDispatcherConfig` — needs config for timeouts per environment
- `withQueryObjectStore` — needs config for connection strings
- `withQueryEndpoint` — needs config for endpoint paths
- `withHealthCheck` — needs config for health check path
- `withSecretStore` — needs config for store backend config

Because these functions are evaluated at app-definition time, before `Application.run` loads `.env` and resolves the config, users cannot access the Config system. The only workarounds are hardcoding values or using `unsafePerformIO` + `System.Environment.getEnv` at top level, both of which are fragile and defeat the purpose of the declarative config DSL introduced in ADR-0021.

### Requirements

- All `withXXX` builder functions must be able to receive values derived from app config
- The API must be consistent with the existing factory pattern used by `withEventStore`, `withAuth`, `withFileUpload`, and `withOAuth2Provider`
- Static config must still work via the `@()` pattern, with no breaking change for that path
  - The `@()` path is stable once adopted; the migration from `withFoo staticValue` to `withFoo @() (\_ -> staticValue)` is a one-time change, not a recurring breaking change
- `runWith` must reject deferred factories with a clear error message, matching the existing pattern
- The config type mismatch must be a compile error, not a runtime failure

## Decision

Apply the same deferred factory pattern to all remaining `withXXX` functions. Each function gets a new factory ADT with `Evaluated` and `Deferred` variants, following the established naming convention.

### Factory ADT Pattern

Each function `withFoo` that currently accepts a static `FooConfig` gets a corresponding factory type:

```haskell
data FooFactory where
  EvaluatedFoo :: FooConfig -> FooFactory
  DeferredFoo :: (Typeable cfg) => (cfg -> FooConfig) -> FooFactory
```

The `withFoo` signature changes from:

```haskell
withFoo :: FooConfig -> Application -> Application
```

to:

```haskell
withFoo ::
  forall config.
  (Typeable config) =>
  (config -> FooConfig) ->
  Application ->
  Application
```

### New Factory Types

The following factory ADTs are introduced, one per affected builder function:

```haskell
data OutboundFactory where
  EvaluatedOutbound :: (entity -> event -> Integration.Outbound) -> OutboundFactory
  DeferredOutbound :: (Typeable cfg) => (cfg -> entity -> event -> Integration.Outbound) -> OutboundFactory

data OutboundLifecycleFactory where
  EvaluatedOutboundLifecycle :: OutboundLifecycleConfig -> OutboundLifecycleFactory
  DeferredOutboundLifecycle :: (Typeable cfg) => (cfg -> OutboundLifecycleConfig) -> OutboundLifecycleFactory

data InboundFactory where
  EvaluatedInbound :: InboundConfig -> InboundFactory
  DeferredInbound :: (Typeable cfg) => (cfg -> InboundConfig) -> InboundFactory

data ServiceFactory where
  EvaluatedService :: ServiceConfig -> ServiceFactory
  DeferredService :: (Typeable cfg) => (cfg -> ServiceConfig) -> ServiceFactory

data TransportFactory where
  EvaluatedTransport :: TransportConfig -> TransportFactory
  DeferredTransport :: (Typeable cfg) => (cfg -> TransportConfig) -> TransportFactory

data CorsFactory where
  EvaluatedCors :: CorsConfig -> CorsFactory
  DeferredCors :: (Typeable cfg) => (cfg -> CorsConfig) -> CorsFactory

data ApiInfoFactory where
  EvaluatedApiInfo :: ApiInfo -> ApiInfoFactory
  DeferredApiInfo :: (Typeable cfg) => (cfg -> ApiInfo) -> ApiInfoFactory

data DispatcherConfigFactory where
  EvaluatedDispatcherConfig :: DispatcherConfig -> DispatcherConfigFactory
  DeferredDispatcherConfig :: (Typeable cfg) => (cfg -> DispatcherConfig) -> DispatcherConfigFactory

data QueryObjectStoreFactory where
  EvaluatedQueryObjectStore :: QueryObjectStoreConfig -> QueryObjectStoreFactory
  DeferredQueryObjectStore :: (Typeable cfg) => (cfg -> QueryObjectStoreConfig) -> QueryObjectStoreFactory

data QueryEndpointFactory where
  EvaluatedQueryEndpoint :: QueryEndpointConfig -> QueryEndpointFactory
  DeferredQueryEndpoint :: (Typeable cfg) => (cfg -> QueryEndpointConfig) -> QueryEndpointFactory

data HealthCheckFactory where
  EvaluatedHealthCheck :: HealthCheckConfig -> HealthCheckFactory
  DeferredHealthCheck :: (Typeable cfg) => (cfg -> HealthCheckConfig) -> HealthCheckFactory

data SecretStoreFactory where
  EvaluatedSecretStore :: SecretStoreConfig -> SecretStoreFactory
  DeferredSecretStore :: (Typeable cfg) => (cfg -> SecretStoreConfig) -> SecretStoreFactory
```

### Phased Delivery

The following factory types are deferred to a future PR due to architectural constraints:

- `ServiceFactory` — Requires type-level rework for service composition
- `TransportFactory` — Requires major refactoring of the Transport typeclass (ADR-0034 in progress)
- `QueryObjectStoreFactory` — Depends on Transport redesign
- `QueryEndpointFactory` — Depends on Transport redesign

These will be addressed in a follow-up PR once the Transport layer is stabilized.

### The `@()` Pattern for Static Config

Functions that don't need app config use the `@()` type application. The `eqT @config @()` trick from `Data.Typeable` causes immediate evaluation:

```haskell
-- Config-dependent (uses app config):
app = Application.new
  |> Application.withConfig @AppConfig
  |> Application.withTransport (\cfg -> TransportConfig { port = cfg.port, host = cfg.host })
  |> Application.withCors (\cfg -> CorsConfig { allowedOrigins = cfg.allowedOrigins })

-- Static config (no app config needed):
app = Application.new
  |> Application.withTransport @() (\_ -> myStaticTransportConfig)
  |> Application.withCors @() (\_ -> myCorsConfig)
```

### Runtime Resolution

In `Application.run`, after config is loaded (step 2), each deferred factory is resolved before the component is initialized:

- `EvaluatedFoo value` -> use directly
- `DeferredFoo mkValue` -> call `mkValue` with the loaded config

In `runWith`, any `DeferredFoo` is rejected with a clear error message, matching the pattern for `DeferredFileUpload`, `DeferredWebAuth`, and `DeferredOAuth2Provider`.

### Usage Example

```haskell
module Main where

import Application qualified
import MyApp.Config (AppConfig)

app :: Application
app = Application.new
  |> Application.withConfig @AppConfig
  |> Application.withEventStore (\cfg -> makePostgresConfig cfg.databaseUrl)
  |> Application.withTransport (\cfg -> TransportConfig { port = cfg.port, host = "0.0.0.0" })
  |> Application.withCors (\cfg -> CorsConfig { allowedOrigins = cfg.allowedOrigins })
  |> Application.withDispatcherConfig (\cfg -> DispatcherConfig { timeout = cfg.dispatcherTimeout })
  |> Application.withOutbound (\cfg entity event -> makeOutbound cfg.serviceUrl entity event)
  |> Application.withService myService

main :: Task Text Unit
main = Application.run app
```

## Consequences

### Positive

- All builder functions can access app config loaded via `Application.withConfig`
- No breaking change for the `@()` usage path
- Consistent API across all builder functions
- Type-safe: config type mismatch is a compile error
- No need for `unsafePerformIO` workarounds or hardcoded values

### Negative

- More factory ADT types to maintain (8 new types to maintain, 4 deferred to future work)
- More resolution logic in `Application.run`
- Breaking change: existing `withFoo staticValue` calls must become `withFoo @() (\_ -> staticValue)`

### Risks and Mitigations

- **Risk**: Increased complexity in `Application.hs`
  - **Mitigation**: The pattern is already established and well-understood from the five existing factory types. New types follow the same structure mechanically.
- **Risk**: Users forget `withConfig` when using deferred factories
  - **Mitigation**: Clear error message at runtime, matching the existing pattern for `DeferredFileUpload` and `DeferredWebAuth`
- **Risk**: Breaking change for existing users
  - **Mitigation**: The `@()` pattern is well-documented and consistent with other builder functions

## References

- [Issue #422](https://github.com/neohaskell/NeoHaskell/issues/422) - Extend all Application.withXXX functions to support config factory pattern
- [ADR-0021: Declarative Config DSL](0021-declarative-config-dsl.md) - The config system these factories integrate with
- [ADR-0033: Config-Dependent OAuth2 Provider Factory](0033-config-dependent-oauth2-provider.md) - Most recent prior art for this pattern
