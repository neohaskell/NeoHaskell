# Configuration and wiring

`Application` is a declarative assembly of configuration, stores, services, queries, transports, and integrations. `Application.run` loads configuration, creates runtime resources, and cleans them up when the application stops.

## Typed configuration

`Config.defineConfig` generates a record, its parser, and its `Has...` constraint. `Config.field @Type` declares a field. Field modifiers add defaults, environment names, command line flags, documentation, and secret handling.

`Application.run` loads `.env` before parsing configuration. Values take precedence in this order: command line, process environment, `.env`, then default.

Use `Config.secret` for credential fields so their display is redacted. Secret marking does not choose persistent credential storage. OAuth2 tokens belong in a [SecretStore](advanced-oauth2.md).

## Factories and registration order

This explanatory fragment needs the application's config, factories, domain service, and query imports.

```haskell ignore
app :: Application
app =
	Application.new
		|> Application.withConfig @AppConfig
		|> Application.withEventStore makeEventStoreConfig
		|> Application.withTransport WebTransport.server
		|> Application.withService Counter.service
		|> Application.withQuery @CounterView
```

`withEventStore` takes a `config -> eventStoreConfig` factory. For static configuration, use `withEventStore @() (\_ -> storeConfig)`. For application configuration, register `withConfig @AppConfig` and use a factory whose argument is `AppConfig`. The factory runs after startup loads configuration.

`withAuth`, `withSecretStore`, `withOAuth2Provider`, and `withFileUpload` also take configuration-dependent factories. `withQueryObjectStore` takes a concrete configuration value and captures the choice when `withQuery` runs. Read [authorization](authorization.md) and [external OAuth2](advanced-oauth2.md) for those features.

Configure stores before queries. Configure `withOAuth2StateKey` before `withOAuth2Provider`. Only one `withConfig` registration is supported.

For Postgres, supply the record fields listed in [storage](storage.md). A generated project's older commented Postgres example may omit current fields.
