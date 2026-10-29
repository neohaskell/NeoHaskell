# Storage

Event storage and query storage have separate configuration. `withEventStore` selects the accepted event history. `withQueryObjectStore` selects stored read models. A Postgres event store does not implicitly make query rows persistent.

## Event history

`SimpleEventStore` has `basePath :: Path` and `persistent :: Bool`. With persistence enabled, it loads events from disk and writes event files. A generated starter project sets `persistent = False`, so its events disappear when that process ends.

`PostgresEventStore` configures Postgres event storage. Its fields are `host`, `databaseName`, `user`, `password`, `port`, `poolSize`, `sslMode`, `sslRootCert`, and `erasableApplicationScope`. `sslRootCert` is `Maybe Text`; use `Nothing` when no certificate path is needed. Set `erasableApplicationScope` to a stable, nonempty identifier for the application before using protected-value erasure.

If your application constructs a custom `EventStore` record, add its `eraseErasableKey` field. A store without protected-value erasure support must set `eraseErasableKey = \_ -> Task.throw (StorageFailure "Protected-value erasure is unsupported by this event store")`. Add `import Task qualified` and `import Service.EventStore (Error (StorageFailure))` if those imports are absent. The current compiler rejects a constructor that omits this field. Build the application, then call the field in a test and check that an unsupported store returns `StorageFailure`. For the one-reference request and result contract of a supporting store, see [protected-value erasure](advanced-erasure.md).

The event store supports insertion preconditions, ordered reads, and subscriptions. The command executor uses the target stream's history and insertion result. One successful append does not establish a transaction across commands, services, or application instances.

For application-owned key deletion, read [protected-value erasure](advanced-erasure.md).

## Query rows

`Application.new` defaults to in-memory query storage. `withQuery @Row` captures the configured store factory at registration. Call `withQueryObjectStore` first.

`PostgresQueryObjectStoreConfig` has the same eight field names. The store distinguishes query types through the query name and instance identifier.

Configuring Postgres event storage without `withQueryObjectStore` still leaves query rows in memory. A query result observed before restart does not prove that the row will survive restart.

## Credentials and caches

OAuth2 credentials belong in a separate `SecretStore`. The shipped in-memory implementation is for development. Applications that need durable tokens supply their own implementation through `withSecretStore`. Read [external OAuth2](advanced-oauth2.md).

A snapshot cache supports entity reconstruction. It does not replace the event history or choose the query backend.
