---
title: "Configuration"
description: Connect typed settings to the parts of your application that consume them.
sidebar:
  order: 9
---

Applications need different settings across environments while keeping their business rules consistent. Configuration gives those choices names, validates their types, and makes the connection to the running application explicit.

A database address is configuration. An agreed order price belongs in business history. Changing a setting tomorrow should not rewrite yesterday's agreement.

Examples below show the relevant declarations and behaviour, with each destination named. The small snippets explain one choice at a time; the assembled files later in this page include the imports needed by those declarations. The [complete end of Build files](/examples/mug-shop-build.tar.gz) is supplementary and contains the same checkpoint plus tests.

## Add a setting your application will use

So far, your application always starts with empty in-memory history. We will make local persistence an explicit development setting, defaulting to the same behaviour.

Name the choice and its default first:

```haskell
  [ Config.field @Bool "persistEvents"
      |> Config.doc "Keep local event files between development runs"
      |> Config.defaultsTo False
      |> Config.envVar "PERSIST_EVENTS"
  ]
```

Place the field inside `defineConfig "ShopConfig"` in `src/Shop/Config.hs`. The checkpoint contains the complete definition.

`defineConfig` generates a record and its parser. The field has documentation, a Boolean type, a default, and an environment variable. The macro requires each field to have documentation and a deliberate default or required-value policy.

## Connect the setting to the store

After completing the Cart and Stock lessons, connect the setting to your **local-development baseline**. If you already added authentication or other registrations, preserve them: add the `Shop.Config` import, insert `withConfig @ShopConfig`, and replace only the `withEventStore` step. Do not discard your application’s permission setup.

The relevant pipeline steps in `src/App.hs` are:

```haskell
  |> Application.withConfig @ShopConfig
  |> Application.withEventStore (\(config :: ShopConfig) -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = config.persistEvents
    })
```

`withConfig` registers the type to load. The store factory consumes that loaded record. This is the important connection: declaring `persistEvents` alone would not change storage.

## Add required values deliberately

A provider credential can be a required secret field. This is a **field-list fragment** to add when the corresponding provider is implemented, not a requirement for the current application:

```haskell
  , Config.field @Text "providerKey"
      |> Config.doc "Credential for the selected external provider"
      |> Config.required
      |> Config.envVar "SHOP_PROVIDER_KEY"
      |> Config.secret
```

Your integration must then consume `config.providerKey`. `required` establishes presence; it cannot prove the remote provider will accept the credential.

`Config.secret` redacts the field from generated record display and JSON. It does not encrypt the value or prevent code from logging the raw field after extraction. Keep real credentials in your deployment's credential mechanism.

## Verify the consumer, not just the parser

The loader reads process arguments and environment variables. A `.env` file does not automatically enter the process environment; use an explicit loader or your process manager if you choose that format.

A field called `httpPort` also does not automatically change a listener. Your application currently uses `WebTransport.server`, which listens on 8080. For a fixed alternate development port, replace that pipeline step with:

```haskell
  |> Application.withTransport (WebTransport.server {port = 8081})
```

Update clients to match. The current `neo test` HTTP workflow probes port 8080, so keep that port for the tutorial tests; changing Hurl URLs alone does not change its startup probe. See the [CLI reference](/reference/cli/). If you later make the port configurable, trace the parsed value all the way to the actual transport and verify the listening address.

## Assemble the local persistence checkpoint

You have now seen the field and the consumer separately. In the same `mug-shop` project, create or replace `src/Shop/Config.hs` with the complete file below.

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
  ]
```

Then replace the application wiring in `src/App.hs` with this complete local checkpoint. If you have already added the authenticated variant from [access control](/build/access-control/), preserve that policy and insert the `withConfig` and `withEventStore` steps into your existing pipeline instead of replacing the whole file.

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Shop.Config (ShopConfig (..))
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Stock.Queries.StockLevel (StockLevel)
import Shop.Stock.Service qualified as Stock

app :: Application
app = Application.new
  |> Application.withConfig @ShopConfig
  |> Application.withEventStore (\(config :: ShopConfig) -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = config.persistEvents
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
```

Run `neo build` after creating or replacing these files. With the default, run
`neo test` and then remove `.neo/events` if you want a fresh local exercise.
With `PERSIST_EVENTS=True`, keep the server's event directory between restarts
and verify the same cart ID still has its summary. This is a development
checkpoint, not a durable production recovery guarantee.

For the restart exercise, stop other servers and run:

```sh
PERSIST_EVENTS=True neo run
```

Use capitalised `True` and `False`: this Boolean field uses the typed Haskell-value parser. Create a cart and keep its ID. Stop the server and run the same command again. Read the cart summary, allowing time for reconstruction and projection. Existing carts from earlier in-memory runs are not migrated into files by enabling the setting.

The [persistence chapter](/operate/persistence/) explains moving to PostgreSQL and checking durable recovery. Local event files are a useful development option; operating an application also requires backups, restore evidence, retention choices, and appropriate access.

## Exercise: optional or misconfigured?

Your agent gives a required provider key an empty-string default so startup succeeds. What behaviour do you want when the provider is unavailable or unconfigured?

<details>
<summary>Suggested reasoning and checks</summary>

If the feature is required, require its credential and report a clear startup failure when missing. If optional, model the disabled state explicitly. Test valid configuration, an absent required value, and an invalid typed value. Check redaction with harmless test credentials and separately check that the provider receives the configured value.

</details>

Next: [review your application](/build/your-shop/) before connecting it to more systems.

API reference: [configuration](https://github.com/neohaskell/NeoHaskell/blob/main/core/config/Config.hs), [application factories](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application.hs), [simple store](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/EventStore/Simple.hs).
