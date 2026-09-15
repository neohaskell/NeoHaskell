---
title: Keep the shop's history
description: Choose storage for events, read models, files, and credentials, then prove what survives restart.
sidebar:
  order: 1
---

Restarting the shop should not erase a customer's order. It is less alarming if a temporary dashboard has to be rebuilt. That distinction is the starting point for persistence: decide which information is authoritative and which can be reconstructed.

In an event-sourced application, accepted events preserve business history. Entities and queries interpret that history for different purposes. Keeping events safely is essential, but events are not the only data your application may need to retain.

## Identify each kind of storage

| Information | NeoHaskell surface | Decision for the shop |
| --- | --- | --- |
| Accepted events | `Service.EventStore` | Use durable storage before accepting real orders |
| Query results | `Service.QueryObjectStore` | Choose memory or Postgres independently of event storage |
| Uploaded bytes | Local blob store configured by `blobStoreDir` | Retain and back up the actual files |
| File ownership and lifecycle | File state store | Choose persistent state as well as persistent bytes |
| Connected-provider secrets | `Application.withSecretStore` | Supply storage with the lifetime your deployment needs |

The starter configures `SimpleEventStore` with `persistent = False`. A filesystem-looking path in that configuration does **not** make that setting durable. It is appropriate for the first experiment; restarting that experiment loses its event history.

## Use the public Postgres example carefully

The reference application's `testbed/src/App.hs` supplies a config factory to:

```haskell
    |> Application.withEventStore makePostgresConfig
```

That factory fills the current `PostgresEventStore` fields: `user`, `password`, `host`, `databaseName`, `port`, `poolSize`, `sslMode`, and `sslRootCert`. Copy the current factory together with the typed configuration it reads; an older example with only connection credentials is incomplete.

The testbed maps these settings from `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_NAME`, `DB_POOL_SIZE`, `DB_SSL_MODE`, and `DB_SSL_ROOT_CERT`. These names belong to that application's configuration. A new application must declare and wire its own fields; environment variables do not configure an unwired subsystem automatically.

Read the [current application example](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs) and [its configuration](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Config.hs) together.

## Persistent events and persistent queries are separate

Queries use memory unless you supply a query-store backend with `Application.withQueryObjectStore` (also exposed as `useQueryObjectStore`). `PostgresQueryObjectStoreConfig` has its own connection and pool settings. Stores distinguish queries by name as well as instance identifier, so two views of the same order remain separate.

There is an important operational boundary: lower-level query subscriber APIs provide checkpoint and hash-aware rebuild support, but normal `Application` wiring currently constructs `Subscriber.new`. Choosing a Postgres query store alone is **not proof that startup resumes from a persisted checkpoint**. Test restart and replay with your actual wiring and projection logic, especially if a projection accumulates values rather than replacing them.

## Prove durability with one order

In an isolated shop environment:

1. Submit a valid order and save its identifier and expected contents.
2. Wait until its query shows the expected result.
3. Stop and restart the process using the same database and storage paths.
4. Wait for `/ready`, then fetch the same order.
5. Compare the identifier, contents, and totals. Check that replay has not counted the order twice.

Repeat with an uploaded attachment if your workflow uses one. A surviving database row does not prove the corresponding bytes survived.

Also establish how abandoned uploads are removed. A lower-level cleanup worker exists in `Service.FileUpload.Web`, but current `Application.withFileUpload` startup does not launch it. Setting `cleanupIntervalSeconds` alone therefore does not establish automatic cleanup. Verify your chosen lifecycle wiring and monitor storage growth.

**Try a variation:** move the process to a fresh host while retaining only the resources you intended to persist.

<details>
<summary>What the result should teach you</summary>

The order should remain recoverable from the retained event store. Any missing file or provider connection reveals another persistence dependency. Add that dependency to the deployment and backup plan, then repeat the experiment. Do not infer durability from a successful ordinary restart alone.

</details>

Continue with [deployment](/operate/deployment/) and [recovery](/operate/recovery/).
