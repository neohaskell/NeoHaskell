---
title: Configuration
description: Give the shop explicit deployment settings and keep secrets out of ordinary output.
sidebar:
  order: 9
---

Your local shop and your deployed shop need different database addresses and provider credentials. They should still follow the same business rules. Configuration makes those environmental choices explicit and validates them before the application starts serving customers.

Decide which values are settings and which belong in the business history. A database host is configuration. A price accepted for an order is part of the commercial record; changing a setting tomorrow should not rewrite yesterday's agreement.

## Read a configuration definition

NeoHaskell's `defineConfig` builds a typed configuration record and its parser. This is an exact excerpt from the testbed's field list:

```haskell
  [ Config.field @Text "dbHost"
      |> Config.doc "PostgreSQL host"
      |> Config.defaultsTo ("localhost" :: Text)
      |> Config.envVar "DB_HOST"
```

It says that `dbHost` is text, documents its purpose, supplies a development default, and connects it to an environment variable. This is only the beginning of the surrounding `defineConfig` expression; do not paste it as a complete module.

Every field needs documentation and either a default or a requirement to provide it. The macro rejects missing documentation, missing default/required choices, and conflicting choices.

Here is an **adapted complete declaration**, intended for a module in a configured NeoHaskell application. The provider field is illustrative; it does not create an integration:

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Shop.Config (ShopConfig (..), HasShopConfig) where

import Config (defineConfig)
import Config qualified
import Core

defineConfig
  "ShopConfig"
  [ Config.field @Text "authServerUrl"
      |> Config.doc "Identity service used by the shop"
      |> Config.required
      |> Config.envVar "AUTH_SERVER_URL"
  , Config.field @Text "providerKey"
      |> Config.doc "Credential for the selected external provider"
      |> Config.required
      |> Config.envVar "SHOP_PROVIDER_KEY"
      |> Config.secret
  ]
```

Your agent must register the module in the application's build configuration, connect the generated type with `Application.withConfig @ShopConfig`, and wire consumers of those fields. This example is source-grounded but is not a compiled application shipped with these docs.

## Connect settings to their consumers

Application factories such as `withEventStore` and `withAuth` can receive the loaded configuration. The testbed builds its PostgreSQL configuration this way. Configuration loading happens before those deferred factories run.

A field's name does not automatically connect it to a subsystem. The current starter and testbed declare `httpPort`, but their application uses the unmodified `WebTransport.server`, whose port is 8080. Changing `PORT` or `HTTP_PORT` alone therefore does not change that listener.

For a fixed alternate development port, this **partial wiring expression** updates the actual transport record:

```haskell
Application.withTransport (WebTransport.server {port = 8081})
```

If you need configuration-driven selection, have your agent show where the parsed value reaches the transport and verify the listening port after startup. A passing parser test cannot establish that wiring.

## Handle secrets as data with restricted display

`Config.secret` makes the generated configuration's `Show` and JSON output redact the field. It does not encrypt the value or prevent code from logging the raw field after extracting it.

For values that need display protection beyond the configuration record, the `Redacted` wrapper provides explicit wrapping and unwrapping. Keep secrets in your deployment's credential mechanism and pass them to the application environment. Do not paste production credentials into examples, screenshots, or agent conversations.

The current loader reads process arguments and environment variables. Do not assume a `.env` file is loaded automatically merely because a template mentions one; use an explicit loader or your process manager and verify the resulting environment.

## Exercise: a missing credential

Your agent added a provider key field with an empty-string default so startup succeeds. Is that the behaviour you want?

<details>
<summary>Suggested reasoning and checks</summary>

If the provider is required, require the field and fail clearly at startup when it is missing. If the feature is optional, model that choice explicitly rather than treating an empty credential as a usable key. Check valid configuration, a missing required value, and an invalid typed value. Check redaction using harmless test credentials, and separately verify that the configured consumer uses the value. `required` establishes presence, not that a remote provider will accept the credential.

</details>

Next: consult [language essentials](/build/language-essentials/), or move on to [connect the shop](/connect/).

Public sources: [configuration API](https://github.com/neohaskell/NeoHaskell/blob/main/core/config/Config.hs), [configuration generation](https://github.com/neohaskell/NeoHaskell/blob/main/core/config/Config/TH.hs), [testbed settings](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Config.hs), [starter wiring](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/src/App.hs), [Redacted](https://github.com/neohaskell/NeoHaskell/blob/main/core/core/Redacted.hs).

Next: [bring the working slice into your own shop](/build/your-shop/) before extending it with providers.
