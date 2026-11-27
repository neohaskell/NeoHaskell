# Configuration and wiring

`Core.eventModel` assembles configuration, stores, services, queries, transports, and integrations from one named model. `EventModel.run` validates it, loads configuration, creates runtime resources, and cleans them up when the application stops. Existing direct `Application` assembly remains available.

## Typed configuration

In the configuration module import `Core` and `Config`. Use `defineConfig` to generate a record, its parser and its `Has...` constraint. Use `field @Type` to declare each field, `doc` for its help text, and either `defaultsTo` or `required` for its value. Field modifiers add defaults, environment names, command line flags, documentation, and secret handling.

Startup loads `.env` before parsing configuration. Values take precedence in this order: command line, process environment, `.env`, then default.

Use `secret` for credential fields so their display is redacted. Secret marking does not choose persistent credential storage. OAuth2 tokens belong in a [SecretStore](advanced-oauth2.md).

## Factories and registration order

This application module imports `Core`, its configuration type, its Counter Feature and the Web transport.

```haskell ignore
appEventModel :: EventModel
appEventModel = eventModel do
  appName "Counter"
  withConfig @StarterConfig
  withPortFromConfig @StarterConfig (.httpPort)
  withEventStore inMemory
  withTransport WebTransport.server
  features do
    counterFlow
```

Import `Core` in the application module. In the launcher import `Service.EventModel.Launcher qualified as EventModel` and run `appEventModel |> EventModel.run |> Task.runOrPanic`. A static store configuration can be supplied directly. For a store selected from configuration, supply a factory whose argument is the registered configuration type.

`withHost` and `withPort` override `WebTransport.host` and `WebTransport.port` on the registered Web transport, regardless of order relative to `withTransport`. Direct `Application` assembly provides the matching `Application.withHost` and `Application.withPort` builders. Omitting either override preserves the transport's own field, and invalid static values fail before startup acquires external resources. Read [transports](transports.md#web) for accepted host and port forms.

`withAuth`, `withAuthOverrides`, `withSecretStore`, `withCors`, `withHealthCheck`, `withOAuth2Provider`, and `withFileUpload` take configuration-dependent factories. `withFileUpload` is also polymorphic over any `BlobStoreConfig` backend such as `LocalBlobStoreConfig` or `S3Config`. `withIntegration @Request` registers a typed adapter. `withoutHealthCheck` disables the default health route, and `useReadinessEndpoint` enables the readiness route. `withQueryObjectStore` takes a concrete configuration value and captures the choice when `withQuery` runs. Use the named Event Model builders for configuration so model validation owns duplicate config registration and Slice declarations own runtime components. Read [storage](storage.md), [authorization](authorization.md), [external OAuth2](advanced-oauth2.md), and [side effects](advanced-side-effects.md) for those features.

Configure stores before query Features. Configure `withOAuth2StateKey` before `withOAuth2Provider`. Validation rejects a second configuration registration before starting the application.

For Postgres, supply the record fields listed in [storage](storage.md). Start from `(def :: PostgresEventStore)` to inherit pool and TLS defaults.
