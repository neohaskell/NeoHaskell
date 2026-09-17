---
title: Persist application data
description: Choose storage for events, read models, files, and credentials, then prove what survives restart.
sidebar:
  order: 1
---

Restarting an application should not erase work it promised to retain. A temporary dashboard, however, may be safe to rebuild. Persistence begins with that distinction: decide which information is authoritative and which can be reconstructed. A cart and its accepted additions in `mug-shop` give us a small example to carry through a restart.

In an event-sourced application, accepted events preserve business history. Entities and queries interpret that history for different purposes. Keeping events safely is essential, but events are not the only data your application may need to retain.

All paths below are relative to the `mug-shop` project root created by `neo new`. The lesson first explains the replacement, then gives the complete `ShopConfig`, storage factory, and `App.hs` files needed for the Postgres checkpoint. If you have already added integrations, keep their imports and registrations while merging the complete persistence files.

## Identify each kind of storage

| Information | NeoHaskell surface | Application decision |
| --- | --- | --- |
| Accepted events | `Service.EventStore` | Use durable storage before accepting work that must survive restart |
| Query results | `Service.QueryObjectStore` | Choose memory or Postgres independently of event storage |
| Uploaded bytes | Local blob store configured by `blobStoreDir` | Retain and back up the actual files |
| File ownership and lifecycle | File state store | Choose persistent state as well as persistent bytes |
| Connected-provider secrets | `Application.withSecretStore` | Supply storage with the lifetime your deployment needs |

The starter configures `SimpleEventStore` with `persistent = False`. A filesystem-looking path in that configuration does not make that setting durable. It is appropriate for the first experiment; restarting that experiment loses its event history.

## Replace the configuration with database settings

Continue in your own `mug-shop` directory. The commands still operate on your `neo.json`, `src/App.hs`, and `src/Shop/` modules. This is the point where the journey adds a database; Cart and Stock did not require one for the earlier lessons.

A password is a useful first example: it is required, supplied by the environment, and redacted when the configuration record is displayed:

```haskell
Config.field @Text "dbPassword"
  |> Config.doc "PostgreSQL password"
  |> Config.required
  |> Config.envVar "DB_PASSWORD"
  |> Config.secret
```

The other choices identify the server and database and set the connection pool and TLS policy. Replace `src/Shop/Config.hs` with the complete file below. It retains the earlier `persistEvents` field so this overlay remains a direct extension of the Build checkpoint; after the switch, that field no longer controls the Postgres event store and can be removed when nothing else uses it.

<!-- complete-file -->
```haskell title="src/Shop/Config.hs"
module Shop.Config (ShopConfig (..), HasShopConfig) where

import Config (defineConfig)
import Config qualified
import Core

defineConfig
  "ShopConfig"
  [ Config.field @Bool "persistEvents"
      |> Config.doc "Keep local event files between development runs"
      |> Config.defaultsTo False
      |> Config.envVar "PERSIST_EVENTS"
  , Config.field @Text "dbHost"
      |> Config.doc "PostgreSQL host"
      |> Config.defaultsTo ("localhost" :: Text)
      |> Config.envVar "DB_HOST"
  , Config.field @Int "dbPort"
      |> Config.doc "PostgreSQL port"
      |> Config.defaultsTo (5432 :: Int)
      |> Config.envVar "DB_PORT"
  , Config.field @Text "dbUser"
      |> Config.doc "PostgreSQL user"
      |> Config.defaultsTo ("neohaskell" :: Text)
      |> Config.envVar "DB_USER"
  , Config.field @Text "dbPassword"
      |> Config.doc "PostgreSQL password"
      |> Config.required
      |> Config.envVar "DB_PASSWORD"
      |> Config.secret
  , Config.field @Text "dbName"
      |> Config.doc "PostgreSQL database name"
      |> Config.defaultsTo ("neohaskell" :: Text)
      |> Config.envVar "DB_NAME"
  , Config.field @Int "dbPoolSize"
      |> Config.doc "Event-store connection pool size"
      |> Config.defaultsTo (6 :: Int)
      |> Config.envVar "DB_POOL_SIZE"
  , Config.field @Text "dbSslMode"
      |> Config.doc "PostgreSQL TLS mode"
      |> Config.defaultsTo ("unset" :: Text)
      |> Config.envVar "DB_SSL_MODE"
  , Config.field @Text "dbSslRootCert"
      |> Config.doc "Root CA certificate path, or empty for none"
      |> Config.defaultsTo ("" :: Text)
      |> Config.envVar "DB_SSL_ROOT_CERT"
  ]
```

The settings map directly to `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_NAME`, `DB_POOL_SIZE`, `DB_SSL_MODE`, and `DB_SSL_ROOT_CERT`. The local defaults match the generated project's Docker Compose database. The password is required so a missing credential produces a configuration error. Choose the deployed database's actual address, credentials, pool budget, and TLS requirements when you leave this local exercise.

## Create a storage factory

Create `src/Shop/Storage.hs`. Keep the translation from settings to storage in this file so `App.hs` only selects the factory. Start with its contract:

```haskell
makePostgresConfig :: ShopConfig -> PostgresEventStore
```

Most fields pass a value through, such as `host = config.dbHost`. TLS mode needs validation because the environment supplies text:

```haskell
      sslMode = case ConnectionConfig.textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err message -> panic message,
```

Copy the complete factory below. It passes all eight current `PostgresEventStore` fields, including pool and TLS settings, and treats an empty root-certificate path as absent.

<!-- complete-file -->
```haskell title="src/Shop/Storage.hs"
module Shop.Storage (makePostgresConfig) where

import Core
import Service.EventStore.Postgres (PostgresEventStore (..))
import Service.Infra.Postgres.ConnectionConfig qualified as ConnectionConfig
import Shop.Config (ShopConfig (..))
import Text qualified

makePostgresConfig :: ShopConfig -> PostgresEventStore
makePostgresConfig config =
  PostgresEventStore
    { user = config.dbUser,
      password = config.dbPassword,
      host = config.dbHost,
      databaseName = config.dbName,
      port = config.dbPort,
      poolSize = config.dbPoolSize,
      sslMode = case ConnectionConfig.textToSslMode config.dbSslMode of
        Ok mode -> mode
        Err message -> panic message,
      sslRootCert =
        if Text.isEmpty config.dbSslRootCert
          then Nothing
          else Just config.dbSslRootCert
    }
```

An unknown TLS mode fails during startup; `unset` leaves the driver's default negotiation in place. A setting has an effect because the factory passes it, not because an environment variable has a recognised-looking name.

## Replace the application event-store wiring

In `src/App.hs`, add this import:

```haskell
import Shop.Storage qualified as Storage
```

Replace the `SimpleEventStore` import and `Application.withEventStore` expression with the Postgres factory, retaining your transport, services, queries, and integration registrations:

```haskell
  |> Application.withEventStore Storage.makePostgresConfig
```

The complete result below continues the Cart-to-Stock and upload lessons while replacing the event store. The temporary timer observation has finished, so its registration is absent. If you skipped a feature, omit its import and registration; retain authentication or any other additions you made. `ShopConfig` and `Shop.Storage` are the two complete files just created above.

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Shop.Uploads qualified as Uploads
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Service.Application (Application)
import Service.Application qualified as Application
import Service.Transport.Web qualified as WebTransport
import Shop.Config (ShopConfig)
import Shop.Storage qualified as Storage
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Stock.Queries.StockLevel (StockLevel)
import Shop.Stock.Service qualified as Stock

app :: Application
app = Application.new
  |> Application.withConfig @ShopConfig
  |> Application.withEventStore Storage.makePostgresConfig
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
  |> Application.withOutbound @ReserveStockOnItemAdded
  |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
```

No extra package is needed in `neo.json`: the event-store implementation is supplied by the framework. The persistence overlay's README records the required upcoming framework release and corrected CLI compiler preset; the released 0.10.0 pin lacks the five `Core` derive helpers used by the current examples.

## Start the local database and application

With Docker and its Compose command available, use the generated project's `docker-compose.yml`:

```sh
docker compose up -d postgres
docker compose exec postgres pg_isready -U neohaskell
neo build
DB_PASSWORD=neohaskell neo run
```

Wait for `pg_isready` to report that the database accepts connections. The password above is the local Compose example's credential. Supply real credentials through your deployment's secret mechanism. The configuration loader reads process environment; creating a `.env` file alone does not prove it reached the application.

If another local service uses port 5432, change the Compose host mapping to a free port, such as `55432:5432`, before starting it, and supply `DB_PORT=55432` when running the application or its tests. Do not stop an unrelated database.

Switching stores does not migrate earlier in-memory history or any local event files enabled with `persistEvents`. Create the cart for the following experiment after starting with Postgres. For repeatable HTTP tests, stop the running application and use `DB_PASSWORD=neohaskell neo test` against this disposable local database. The CLI starts its own server; the tests write data, so never point that command at a production database.

## Persistent events and persistent queries are separate

Queries use memory unless you supply a query-store backend with `Application.withQueryObjectStore` (also exposed as `useQueryObjectStore`). `PostgresQueryObjectStoreConfig` has its own connection and pool settings. Stores distinguish queries by name as well as instance identifier, so two views of the same entity remain separate.

There is an important operational boundary: lower-level query subscriber APIs provide checkpoint and hash-aware rebuild support, but normal `Application` wiring currently constructs `Subscriber.new`. Choosing a Postgres query store alone is **not proof that startup resumes from a persisted checkpoint**. Test restart and replay with your actual wiring and projection logic, especially if a projection accumulates values rather than replacing them.

## Prove durability with a representative change

Use the Cart routes from [HTTP and frontend](/build/http-and-frontend/) with the local Postgres configuration:

1. Create a cart, add a positive quantity, and save its identifier and expected contents.
2. Wait until `CartSummary` shows the expected result, then send a zero quantity and verify the refusal.
3. Stop with Ctrl-C and run `DB_PASSWORD=neohaskell neo run` again from the same project, using the same database.
4. Wait for `/ready`, then fetch the same cart summary.
5. Compare the identifier, item count, and empty/nonempty state. Check that replay has not counted an addition twice, and that the refused request contributed nothing.

Repeat with an uploaded attachment if your workflow uses one. A surviving database row does not prove the corresponding bytes survived. Also establish how abandoned uploads are removed: a lower-level cleanup worker exists in `Service.FileUpload.Web`, but current `Application.withFileUpload` startup does not launch it. Setting `cleanupIntervalSeconds` alone therefore does not establish automatic cleanup. Verify your chosen lifecycle wiring and monitor storage growth.

Try moving the process to a fresh host while retaining only the resources you intended to persist. The cart should remain recoverable from the retained event store. Any missing file or provider connection reveals another persistence dependency; add it to the deployment and backup plan, then repeat the experiment. Do not infer durability from a successful ordinary restart alone.

Continue with [deployment](/operate/deployment/) and [recovery](/operate/recovery/).

<details>
<summary>Framework and checkpoint sources</summary>

- [Postgres event-store fields](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/EventStore/Postgres/Internal.hs)
- [TLS mode parsing](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Infra/Postgres/SslMode.hs)
- [Generated local database](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/docker-compose.yml)
- [Complete persistence configuration](https://github.com/neohaskell/NeoHaskell/blob/main/website/examples/mug-shop/persistence/src/Shop/Config.hs)
- [Complete persistence storage factory](https://github.com/neohaskell/NeoHaskell/blob/main/website/examples/mug-shop/persistence/src/Shop/Storage.hs)
- [Complete persistence application](https://github.com/neohaskell/NeoHaskell/blob/main/website/examples/mug-shop/persistence/src/App.hs)

</details>
