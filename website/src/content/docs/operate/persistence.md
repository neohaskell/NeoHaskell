---
title: Persist application data
description: Choose storage for events, read models, files, and credentials, then prove what survives restart.
sidebar:
  order: 1
---

Restarting an application should not erase work it promised to retain. A temporary dashboard, however, may be safe to rebuild. Persistence begins with that distinction: decide which information is authoritative and which can be reconstructed. A cart and its accepted additions in `mug-shop` give us a small example to carry through a restart.

In an event-sourced application, accepted events preserve business history. Entities and queries interpret that history for different purposes. Keeping events safely is essential, but events are not the only data your application may need to retain.

## Identify each kind of storage

| Information | NeoHaskell surface | Application decision |
| --- | --- | --- |
| Accepted events | `Service.EventStore` | Use durable storage before accepting work that must survive restart |
| Query results | `Service.QueryObjectStore` | Choose memory or Postgres independently of event storage |
| Uploaded bytes | Local blob store configured by `blobStoreDir` | Retain and back up the actual files |
| File ownership and lifecycle | File state store | Choose persistent state as well as persistent bytes |
| Connected-provider secrets | `Application.withSecretStore` | Supply storage with the lifetime your deployment needs |

The starter configures `SimpleEventStore` with `persistent = False`. A filesystem-looking path in that configuration does **not** make that setting durable. It is appropriate for the first experiment; restarting that experiment loses its event history.

## Switch your project to Postgres

Continue in your `mug-shop` directory. The commands still operate on your own
`neo.json`, `src/App.hs`, and `src/Shop/` modules. This is the point where the
journey adds a database; Cart and Stock did not require one for the earlier lessons.

### Add the database fields

A password is a useful first example: it is required, supplied by the environment,
and redacted when the configuration record is displayed. This is the one field
we want you to understand before adding the rest:

```haskell
Config.field @Text "dbPassword"
  |> Config.doc "PostgreSQL password"
  |> Config.required
  |> Config.envVar "DB_PASSWORD"
  |> Config.secret
```

The other choices identify the server and database and set the connection pool
and TLS policy. Add them to your existing configuration when ready.

The [persistence checkpoint](/examples/mug-shop-persistence.tar.gz) contains the
complete settings and storage factory. Its three files belong in your existing
project: `src/Shop/Config.hs`, `src/Shop/Storage.hs`, and `src/App.hs`.
Compare and merge these changes if you have added integrations or settings since
Build; keep your existing registrations. The download is a reference for these
changes, not a new project.

The remaining settings follow the same pattern: `DB_HOST`, `DB_PORT`, `DB_USER`,
`DB_NAME`, `DB_POOL_SIZE`, `DB_SSL_MODE`, and `DB_SSL_ROOT_CERT` describe the
connection. Each maps to an explicit field in `ShopConfig`.

The local defaults match the generated project's Docker Compose database. The
password is required so a missing credential produces a configuration error.
Choose the deployed database's actual address, credentials, pool budget, and TLS
requirements when you leave this local exercise.

### Give storage configuration its own file

Keep application composition easy to scan. Put the translation from settings to
storage in `src/Shop/Storage.hs`; `App.hs` only selects that factory.

The relationship is small: your `ShopConfig` supplies the settings and the
factory returns a `PostgresEventStore`. Start by reading that relationship:

```haskell
makePostgresConfig :: ShopConfig -> PostgresEventStore
```

Most fields pass a value through, such as `host = config.dbHost`. TLS mode needs
validation because the environment supplies text. This part of the factory
turns a recognised name into the driver's setting:

```haskell
      sslMode = case ConnectionConfig.textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err message -> panic message,
```

The complete factory in the download also handles an optional certificate path.
It lives in `Shop.Storage`, which the application refers to as `Storage`.

Keep `Application.withConfig @ShopConfig` in the application chain. Replace only
the event-store step with:

```haskell
    |> Application.withEventStore Storage.makePostgresConfig
```

Your services, queries, and outbound handlers remain registered in that chain.
The earlier `persistEvents` setting belonged to `SimpleEventStore`; it no longer
controls persistence after this replacement. Remove that field if no other code
uses it. No extra package is needed in
`neo.json`: the event-store implementation is supplied by the framework.

This factory passes all eight current `PostgresEventStore` fields, including pool
and TLS settings. A setting has an effect because the factory passes it, not
because an environment variable has a recognised-looking name. An unknown TLS
mode fails during startup; `unset` leaves the driver's default negotiation in
place. For remote deployment, select and verify the mode your database requires.

### Start the local database and application

With Docker and its Compose command available, use the generated project's
`docker-compose.yml`:

```sh
docker compose up -d postgres
docker compose exec postgres pg_isready -U neohaskell
neo build
DB_PASSWORD=neohaskell neo run
```

If another local service uses port 5432, change the Compose host mapping to a
free port, such as `55432:5432`, before starting it, and supply `DB_PORT=55432`
when running your application or its tests. Do not stop an unrelated database.

Wait for `pg_isready` to report that the database accepts connections. The password
above is the local Compose example's credential. Supply real credentials through
your deployment's secret mechanism. The configuration loader reads process
environment; creating a `.env` file alone does not prove it reached the application.

Switching stores does not migrate earlier in-memory history or any local event
files you enabled with `persistEvents`. Create the cart for the following experiment after starting with Postgres. For repeatable
HTTP tests, stop the running application and use `DB_PASSWORD=neohaskell neo test`
against this disposable local database. The CLI starts its own server; the tests
write data, so never point that command at a production database.

## Persistent events and persistent queries are separate

Queries use memory unless you supply a query-store backend with `Application.withQueryObjectStore` (also exposed as `useQueryObjectStore`). `PostgresQueryObjectStoreConfig` has its own connection and pool settings. Stores distinguish queries by name as well as instance identifier, so two views of the same entity remain separate.

There is an important operational boundary: lower-level query subscriber APIs provide checkpoint and hash-aware rebuild support, but normal `Application` wiring currently constructs `Subscriber.new`. Choosing a Postgres query store alone is **not proof that startup resumes from a persisted checkpoint**. Test restart and replay with your actual wiring and projection logic, especially if a projection accumulates values rather than replacing them.

## Prove durability with a representative change

Choose an accepted operation from your application and compare its state before and after restart. Use the Cart routes you added in [HTTP and frontend](/build/http-and-frontend/), with the local Postgres configuration above:

1. Create a cart, add a positive quantity, and save its identifier and expected contents.
2. Wait until `CartSummary` shows the expected result, then send a zero quantity and verify the refusal.
3. Stop with Ctrl-C and run `DB_PASSWORD=neohaskell neo run` again from the same project, using the same database.
4. Wait for `/ready`, then fetch the same cart summary.
5. Compare the identifier, item count, and empty/nonempty state. Check that replay has not counted an addition twice, and that the refused request contributed nothing.

Repeat with an uploaded attachment if your workflow uses one. A surviving database row does not prove the corresponding bytes survived.

Also establish how abandoned uploads are removed. A lower-level cleanup worker exists in `Service.FileUpload.Web`, but current `Application.withFileUpload` startup does not launch it. Setting `cleanupIntervalSeconds` alone therefore does not establish automatic cleanup. Verify your chosen lifecycle wiring and monitor storage growth.

**Try a variation:** move the process to a fresh host while retaining only the resources you intended to persist.

<details>
<summary>What the result should teach you</summary>

The cart should remain recoverable from the retained event store. Any missing file or provider connection reveals another persistence dependency. Add that dependency to the deployment and backup plan, then repeat the experiment. Do not infer durability from a successful ordinary restart alone.

</details>

Continue with [deployment](/operate/deployment/) and [recovery](/operate/recovery/).

Implementation evidence: [Postgres event-store fields](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/EventStore/Postgres/Internal.hs), [TLS mode parsing](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Infra/Postgres/SslMode.hs), and [generated local database](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/docker-compose.yml).
