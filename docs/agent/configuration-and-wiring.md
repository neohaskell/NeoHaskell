# Configuration and wiring

`EventModel.eventModel` assembles configuration, stores, services, queries, transports, and integrations from one named model. `EventModel.run` validates it, loads configuration, creates runtime resources, and cleans them up when the application stops. Existing direct `Application` assembly remains available.

## Typed configuration

`Config.defineConfig` generates a record, its parser, and its `Has...` constraint. `Config.field @Type` declares a field. Field modifiers add defaults, environment names, command line flags, documentation, and secret handling.

Startup loads `.env` before parsing configuration. Values take precedence in this order: command line, process environment, `.env`, then default.

Use `Config.secret` for credential fields so their display is redacted. Secret marking does not choose persistent credential storage. OAuth2 tokens belong in a [SecretStore](advanced-oauth2.md).

## Factories and registration order

This explanatory fragment needs the application's config, factories, domain service, and query imports.

```haskell ignore
appEventModel = EventModel.eventModel do
	EventModel.appName "Counter"
	EventModel.withConfig @AppConfig
	EventModel.withEventStore makeEventStoreConfig
	EventModel.withTransport WebTransport.server
	EventModel.features do
		counterFlow

main = EventModel.run appEventModel |> Task.runOrPanic
```

`withEventStore` takes a `config -> eventStoreConfig` factory. For static configuration, use `withEventStore @() (\_ -> storeConfig)`. For application configuration, register `withConfig @AppConfig` and use a factory whose argument is `AppConfig`. The factory runs after startup loads configuration. The source-local `counterFlow` Feature declares its command and query Slices; the model registers them.

`withAuth`, `withAuthOverrides`, `withSecretStore`, `withCors`, `withHealthCheck`, `withOAuth2Provider`, and `withFileUpload` take configuration-dependent factories. `withFileUpload` is also polymorphic over any `BlobStoreConfig` backend such as `LocalBlobStoreConfig` or `S3Config`. `withIntegration @Request` registers a typed adapter. `withoutHealthCheck` disables the default health route, and `useReadinessEndpoint` enables the readiness route. `withQueryObjectStore` takes a concrete configuration value and captures the choice when `withQuery` runs. Use the named Event Model builders for configuration so model validation owns duplicate config registration and Slice declarations own runtime components. Read [storage](storage.md), [authorization](authorization.md), [external OAuth2](advanced-oauth2.md), and [side effects](advanced-side-effects.md) for those features.

Configure stores before query Features. Configure `withOAuth2StateKey` before `withOAuth2Provider`. A second `EventModel.withConfig` returns an error from `eventModel` before startup.

For Postgres, supply the record fields listed in [storage](storage.md). A generated project's older commented Postgres example may omit current fields.
