# Integration checkpoint for mug-shop

These files continue the reader-created `mug-shop` project after the Build
chapters. They are an overlay, not a second project or a framework checkout.
The human instructions explain the focused declarations and logic in
[workflows](https://neohaskell.org/connect/workflows/),
[files](https://neohaskell.org/connect/files/), and
[timers](https://neohaskell.org/connect/timers/). This checkpoint supplies the
complete files, including their module headers and imports.

Apply this directory's `src/` and `tests/` contents over that project's matching
directories. Keep its `neo.json`, launcher, Cart and Stock services, queries, and
configuration. Built-in integrations are already included by `neo new`.

## Required framework version

Use the upcoming framework release with the five `Core` derive helpers and the
corrected CLI compiler preset described in setup. The released 0.10.0 framework
pin lacks these exports. These revised modules use `deriveCommand` and
`deriveOutboundIntegration` through `Core`. Compilation and runtime checks
were not repeated for this migration; historical Connect runs preceded it.

## Connect Cart to Stock

Add this import to the existing `src/App.hs`:

```haskell
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
```

Add this registration to its application pipeline, retaining both services:

```haskell
  |> Application.withOutbound @ReserveStockOnItemAdded
```

Stop any running application, then run `neo build` and `neo test` from the project
root. `tests/stock-reservation.hurl` creates fresh stock and cart identifiers,
checks the two resulting views, verifies a zero-quantity refusal, and reserves
exactly the remaining stock. Leave the optional timer unregistered for this run.
The local scenario assumes the development transport before authentication is
added; an authenticated variation needs its own test identities.

## Check file uploads

Import `Shop.Uploads qualified as Uploads` in `src/App.hs`, then add:

```haskell
  |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
```

Run `neo build` and `neo run`. Follow the files chapter's local text-file upload
and download requests. The configuration stores file metadata in memory and
bytes in `uploads/`; it does not promise that references survive restart.
The unit factory is evaluated independently of an existing `ShopConfig`.

## Observe the timer separately

The overlay adds `CreateCartInternal` and registers it in `Shop.Cart.Service`.
It declares `InternalTransport` and delegates to the existing cart-creation
decision; the public `CreateCart` keeps its web transport.

Import `Shop.Cart.Timers (periodicCartCreator)` and temporarily add:

```haskell
  |> Application.withInbound @() (\_ -> periodicCartCreator)
```

With `neo run`, observe the cart summary: a new empty cart appears after startup,
then another after the interval. This exercises command submission, not a durable
expiry policy. Stop the application and remove the timer registration after the
observation. Keep the module when compiling the complete checkpoint.

Provider chapters use explicitly partial request builders and application-defined
outcome commands. This checkpoint does not claim live email, payment, OAuth,
document-provider, or AI-provider verification.
