---
title: Assemble a practice application
description: Transfer a working example into a generated project, connect its parts, and verify the result.
sidebar:
  order: 10
---

A feature becomes part of an application when its commands, views, and integrations are wired together and checked in that application's environment. This exercise teaches that assembly process by transferring the public Cart and Stock examples into the `mug-shop` practice project.

The result can collect choices, reserve stock, and show both views. More importantly, you will know how to take a source-grounded example, establish its dependencies, register its components, and verify the connection. Those are steps you can reuse in the application you want to build.

Keep the generated Counter while doing this. Its tests give you a known working starting point. You can remove it deliberately after the new slice is established.

## Establish the two workspaces

You need the practice application from [getting started](/getting-started/) and a checkout of the **public NeoHaskell repository**. The commands below run from the generated application's root. If you have not created it yet, run these from its intended parent directory:

```sh
neo --ci new mug-shop
cd mug-shop
```

Before copying, compare the application's `neo-version` in `neo.json` with the framework revision used by the source checkout. The CLI pins a new project to its embedded starter's framework revision; it does not automatically use the checkout you happen to have nearby.

Choose a public committed revision whose Cart/Stock example and generated-starter compatibility have been checked together. If using that newer revision, set `neo-version` to its full commit SHA in `neo.json`, then run the existing application's build and tests before transferring code. `neo build` regenerates the dependency files from that setting. Do not mix current examples with an older framework and assume identical APIs. A local unpublished commit is not fetchable as an upstream dependency.

Set this variable to the absolute path of the matching public checkout; the quoted placeholder must be replaced:

```sh
NEOHASKELL_CHECKOUT='/absolute/path/to/NeoHaskell'
git -C "$NEOHASKELL_CHECKOUT" rev-parse HEAD
neo --ci build
neo --ci test
```

Stop other servers on port 8080 before testing. If baseline tests fail, resolve that compatibility or environment problem first.

## Copy the slice and its evidence

From `mug-shop`, run this **once**, before `src/Testbed/Cart` and `src/Testbed/Stock` exist:

```sh
mkdir -p src/Testbed tests/scenarios tests/commands
cp -R "$NEOHASKELL_CHECKOUT/testbed/src/Testbed/Cart" src/Testbed/
cp -R "$NEOHASKELL_CHECKOUT/testbed/src/Testbed/Stock" src/Testbed/
cp "$NEOHASKELL_CHECKOUT/testbed/tests/commands/create-cart.hurl" tests/commands/
cp "$NEOHASKELL_CHECKOUT/testbed/tests/scenarios/stock-reservation.hurl" tests/scenarios/
```

Retain the `Testbed` module namespace initially so the existing imports remain consistent. It describes the example's origin; the copied code now belongs to your application. A later namespace rename should change file paths and module/import names together and rerun the checks.

The two directories include the complete local dependency chain:

| Part | Included responsibility |
| --- | --- |
| `Cart/Core.hs` and `Stock/Core.hs` | State, events, reconstruction, and type connections |
| Each `Commands/` directory | Cart creation/addition and stock initialisation/reservation |
| Each `Service.hs` | Registration of that domain's commands |
| `Queries/CartSummary.hs`, `Queries/StockLevel.hs` | Read models and their demo access policies |
| `Cart/Integrations/ReserveStockOnItemAdded.hs` | The cart-to-stock connection |

The Cart service also registers internal cart creation and natural-key registration, whose modules are included. Timer and event-counter example modules are copied too, but do not run unless registered. This milestone registers only the reservation handler. It does not copy the testbed's application, document service, upload configuration, or database setup.

## Wire it into your application

Add these imports to the existing `src/App.hs`:

```haskell
import Testbed.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Testbed.Cart.Queries.CartSummary (CartSummary)
import Testbed.Cart.Service qualified as Cart
import Testbed.Stock.Queries.StockLevel (StockLevel)
import Testbed.Stock.Service qualified as Stock
```

Keep the starter's imports and configuration. Replace its `app` declaration with this **adapted application fragment**, which relies on those existing imports:

```haskell
app :: Application
app =
  Application.new
    |> Application.withConfig @StarterConfig
    |> Application.withEventStore (\(_ :: StarterConfig) -> SimpleEventStore {basePath = Path.fromText ".neo/events" |> Maybe.getOrDie, persistent = False})
    |> Application.withTransport WebTransport.server
    |> Application.withService Counter.service
    |> Application.withQuery @CounterView
    |> Application.withService Cart.service
    |> Application.withService Stock.service
    |> Application.withQuery @CartSummary
    |> Application.withQuery @StockLevel
    |> Application.withOutbound @ReserveStockOnItemAdded
```

This deliberately keeps the starter's nonpersistent store and public demonstration policies. PostgreSQL is not required for this assembled slice. Restarting loses its history. An application serving real users needs appropriate [persistence](/operate/persistence/) and [access control](/build/access-control/), with checks that establish both.

Run:

```sh
neo --ci build
neo --ci test
```

Reconciliation discovers the copied source modules and Hurl files; do not manually maintain generated Cabal module lists. The test command starts its own server for Hurl. A separately running `neo run` would compete for its port.

## Explain the passing result

The retained Counter checks should still pass. Cart creation should return a UUID. The reservation scenario creates 100 units, adds five to a cart, observes 95 available and five reserved, then adds ten and observes 85/15. Its bounded query retries account for asynchronous updates.

Now run `neo run` and repeat the manual requests from [stock and checkout](/build/stock-and-checkout/) against **your application's** port 8080. In another terminal rooted in `mug-shop`, use the [IDE model workflow](/getting-started/visual-ide/) to inspect the copied domains. Do not reuse an IDE still rooted in the framework testbed.

Ask your agent to demonstrate a zero-quantity rejection and an over-reservation. Explain why the second case can leave an accepted cart addition with a rejected stock reservation. That missing return path identifies the next design problem in this exercise; it also illustrates why a connected workflow needs evidence of its final outcome.

## Grow toward order acceptance

The ecommerce thread can continue through these **proposed capstone scenarios**. They provide practice with capabilities covered in the following sections:

| Milestone | Evidence required before accepting it |
| --- | --- |
| Order acceptance | An explicit accepted-order fact, captured prices/currency, and rejection when the chosen prerequisites are unmet |
| Reservation outcome | Visible success/failure, no over-reservation, and an explained cancellation/expiry policy |
| Payment | Provider identity, duplicate protection, refusal handling, and reconciliation after a lost reply |
| Confirmation email | Accepted/failed notification state without erasing the order; a controlled delivery test |
| AI description drafts | Review before publication and protection against late replies replacing newer work |
| Operation | Durable restart, restored history, authorised access, and smoke tests against the deployed revision |

Choose one milestone at a time. Write a happy case, a rejection, and a boundary case before asking the agent to implement it. The transferred Cart/Stock slice is executable evidence for its own behaviour; the later milestones remain design exercises.

Continue with [integrations](/connect/) and eventually [run and evolve](/operate/). When you apply these capabilities to a different project, start from its own requests, accepted facts, and completion criteria. The practice project gives you a method for developing and checking those decisions.

Public sources: [starter application](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/src/App.hs), [Cart service](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Service.hs), [Stock service](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Stock/Service.hs), [reservation scenario](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl), [project reconciliation](https://github.com/neohaskell/NeoHaskell/blob/main/neo/src/reconcile/mod.rs).
