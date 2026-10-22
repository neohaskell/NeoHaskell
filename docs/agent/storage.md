# Storage

Event storage and query storage have separate configuration. `withEventStore` selects the accepted event history. `withQueryObjectStore` selects stored read models. A Postgres event store does not implicitly make query rows persistent. See Application in `core/service/Service/Application.hs`.

## Event history

`SimpleEventStore` has `basePath :: Path` and `persistent :: Bool`. With persistence enabled, it loads events from disk and writes event files. The starter in `neo/starter/src/App.hs` explicitly sets `persistent = False`, so its events disappear when that process ends. See the Simple implementation in `core/service/Service/EventStore/Simple.hs`.

`PostgresEventStore` is the Postgres configuration. Its current fields include connection settings, `poolSize`, `sslMode`, and `sslRootCert`. The Postgres implementation in `core/service/Service/EventStore/Postgres.hs` owns those fields. The testbed factory in `testbed/src/App.hs` shows the complete current configuration and TLS parsing.

The event store interface in `core/service/Service/EventStore/Core.hs` exposes insertion preconditions, ordered reads, and subscriptions. The command executor in `core/service/Service/CommandExecutor/Core.hs` uses the target stream's history and insertion results. Do not infer a transaction across commands, services, or application instances from one successful append.

## Query rows

`Application.new` defaults to in-memory query storage. `withQuery @Row` captures the configured store factory at registration. Call `withQueryObjectStore` first. See query registration in `core/service/Service/Application.hs`.

`PostgresQueryObjectStoreConfig` has connection fields, `poolSize`, `sslMode`, and `sslRootCert`. The store distinguishes query types through the query name and instance identifier. See Postgres query storage in `core/service/Service/QueryObjectStore/Postgres.hs`.

The testbed app in `testbed/src/App.hs` configures Postgres event storage but does not call `withQueryObjectStore`. Its query rows therefore use memory. The query idempotency scenario in `testbed/tests/scenarios/query-idempotency.hurl` exercises visible query results. It does not prove restart recovery.

## Credentials and caches

OAuth2 credentials belong in the separate SecretStore interface in `core/auth/Auth/SecretStore.hs`. The shipped in-memory implementation in `core/auth/Auth/SecretStore/InMemory.hs` is for development. Applications that need durable tokens supply their own implementation through `withSecretStore`. Read [external OAuth2](advanced-oauth2.md).

A snapshot cache in `core/service/Service/SnapshotCache.hs` supports entity reconstruction. It does not replace the event history or choose the query backend.
