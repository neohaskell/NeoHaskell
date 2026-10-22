# Configuration and wiring

`Application` is a declarative assembly of configuration, stores, services, queries, transports, and integrations. `Application.run` loads configuration and creates the runtime resources. The starter application in `neo/starter/src/App.hs` is the basic assembly. The Application implementation in `core/service/Service/Application.hs` owns startup and cleanup.

## Typed configuration

`Config.defineConfig` generates a record, its parser, and its `Has...` constraint. `Config.field @Type` declares a field. Field modifiers add defaults, environment names, command line flags, documentation, and secret handling. See Config in `core/config/Config.hs`, the generator in `core/config/Config/TH.hs`, and StarterConfig in `neo/starter/src/Starter/Config.hs`.

`Application.run` loads `.env` before parsing configuration. The starter describes precedence as command line, environment, `.env`, then default. Inspect the config parser in `core/config/Config.hs` and environment loader in `core/system/Environment.hs` when changing precedence or overrides.

Use `Config.secret` for credential fields. The configuration implementation in `core/config/Config.hs` owns secret display behavior. Secret marking does not choose persistent credential storage. OAuth2 tokens belong in a [SecretStore](advanced-oauth2.md).

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

`withEventStore` takes a `config -> eventStoreConfig` factory. For static configuration, use `withEventStore @() (\_ -> storeConfig)`. For application configuration, register `withConfig @AppConfig` and use a factory whose argument is `AppConfig`. The builder in `core/service/Service/Application.hs` defers that factory until startup has loaded configuration.

Other factory builders include `withAuth`, `withSecretStore`, `withOAuth2Provider`, and `withFileUpload`. Read their signatures individually. `withQueryObjectStore` takes a concrete configuration value and captures the choice when `withQuery` runs. See Application in `core/service/Service/Application.hs`.

Configure stores before queries. Configure `withOAuth2StateKey` before `withOAuth2Provider`. Only one `withConfig` registration is supported. The builder checks in `core/service/Service/Application.hs` enforce these contracts.

The testbed application in `testbed/src/App.hs` demonstrates config-dependent Postgres and upload factories. Use its current record fields when configuring Postgres. The starter's commented Postgres block is not the complete current Postgres record.
