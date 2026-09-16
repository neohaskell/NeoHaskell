---
title: Coordinate work across entities
description: Connect events and commands across services while keeping each rule explicit.
sidebar:
  order: 1
---

One accepted change can require work in another part of an application. Keeping those responsibilities separate gives each rule a clear home, but introduces a period when one side has changed and the other has not. An integration makes that handoff explicit.

Continue in your `mug-shop` directory from [stock and checkout](/build/stock-and-checkout/). Its Cart and Stock services already make separate decisions. Now connect them: adding two mugs records the choice in Cart, then asks Stock to reserve two units. This first policy reserves on addition; reserving at checkout is a later variation.

## Add the handler to your project

The handoff has one job: turn the accepted item addition into a stock request.
The important part of the handler is this command value:

```haskell
ReserveStock
  { stockId = added.stockId
  , quantity = added.quantity
  , cartId = cart.cartId
  }
```

`added` is the payload inside `ItemAdded`; `cart` supplies the cart identifier.
Wrap that value in `Command.Emit` so the integration runtime can deliver it.
Put this handler in `src/Shop/Cart/Integrations/ReserveStockOnItemAdded.hs`.
The excerpt focuses on its declaration and rule; the
[complete integration checkpoint](/examples/mug-shop-connect.tar.gz) supplies
its module header and imports, including `Core` and your Cart and Stock types.

```haskell
data ReserveStockOnItemAdded = ReserveStockOnItemAdded


type instance EntityOf ReserveStockOnItemAdded = CartEntity


handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
handleEvent cart event =
  case event of
    ItemAdded added ->
      Integration.batch
        [ Integration.outbound
            Command.Emit
              { command =
                  ReserveStock
                    { stockId = added.stockId
                    , quantity = added.quantity
                    , cartId = cart.cartId
                    }
              }
        ]
    _ -> Integration.none


deriveOutboundIntegration ''ReserveStockOnItemAdded
```

Read it aloud: when an item is added, ask stock to reserve the quantity for this
cart. Other cart events produce no action. `Command.Emit` submits a command to
another registered service without making an external HTTP call.

The marker at the end connects `handleEvent` to the outbound machinery. The
function still supplies the rule; the marker does not choose when stock should
be reserved.

The typed integration reconstructs Cart state from its recorded history. Its
registration needs a default starting value for `CartEntity`; the entity marker
already supplies it from `initialState`:

```haskell
deriveEntity ''CartEntity ''CartEvent
```

Keep that declaration in `src/Shop/Cart/Entity.hs` after its companions. The
complete Cart checkpoint includes it; no second set of cart rules or manual
`Default` instance is needed.

Bring `ReserveStockOnItemAdded` into scope in `src/App.hs`, then add this line to the existing application pipeline, retaining both service
registrations and their queries:

```haskell
    |> Application.withOutbound @ReserveStockOnItemAdded
```

This handler uses the core integration modules already available to your project.
It needs no provider credentials or additional integration package.

## Build and observe the handoff

From `mug-shop`, run:

```sh
neo build
neo test
neo run
```

Keep the server running and use another terminal for the requests from
[stock and checkout](/build/stock-and-checkout/). Create fresh
stock with three available units and a fresh cart, then add two units using their
returned identifiers. Poll the stock query until it reports one available and
two reserved. A successful cart response does not mean the stock query has
already caught up.

Read the cart summary too. Its `itemCount` is one because it counts entries, even
though the addition requested two units. Stock tracks unit quantities. These
views answer different questions about the same workflow.

## Keep a repeatable integration check

Stop `neo run` before running the project’s test command, which manages its test
server. Add `tests/stock-reservation.hurl` from the integration checkpoint. It captures
fresh identifiers, checks both views, rejects zero without changing those views,
then reserves exactly the final remaining unit. It uses the local
development transport from the first cart chapter; if you have added
authentication, supply the test identities from [access control](/build/access-control/)
as part of the scenario.

The complete scenario is included in the
[integration checkpoint](/examples/mug-shop-connect.tar.gz). Its stock query
makes the asynchronous boundary visible; `stock_id` is captured from the fresh
stock created earlier in the scenario:

```hurl
GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 1
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 2
```

Its invalid-input check sends zero to the same cart and stock:

```hurl
POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"{{stock_id}}","quantity":0}
HTTP/1.1 400
```

The following query checks must still show the original cart entry and stock
reservation. A refusal is useful evidence only when the accepted state also
remains correct.

Run `neo test` from `mug-shop`. The query retries wait for the asynchronous
integration and views; they do not resubmit an accepted addition. The checks
make the entry-versus-unit distinction visible: two accepted additions create
two cart entries while reserving three stock units.

Keep your existing Cart and Stock tests too. This scenario tests their connected
behaviour; the smaller tests help explain which rule failed when it breaks.

Run `neo ide` from `mug-shop` to explore the same connection visually. Identify
the originating command, its event, the integration, and the destination command.
Explain where the stock rule executes without reading every handler line.

## The case the happy path does not settle

> **Jess:** “If stock is unavailable, undo adding the item automatically.”
>
> **Agent:** “The stock command rejects the reservation, so the cart is unchanged.”
>
> **Jess:** “The cart event was already accepted. Show me the return path that updates the cart.”

A rejection on the stock side cannot erase an already recorded cart event. The displayed handler supplies one direction of communication. A complete workflow needs an explicit outcome path, such as recording reservation failure and changing what checkout permits. Implementing that return path is a useful extension to the practice project.

This is a **process manager** problem: coordinating steps across entities, tracking progress, and handling incomplete work. The general lesson is to represent pending work and its recovery explicitly. In the example, that means checkout cannot be considered complete merely because the cart accepted an item.

## Exercise: choose when stock is reserved

Change the practice project’s reservation policy from “on add” to “on checkout request.” Write the event sequence before changing code.

<details>
<summary>Suggested reasoning and checks</summary>

Adding to the cart should no longer reserve stock. Checkout should request a reservation once for a stable business operation. Check sufficient stock, insufficient stock, duplicate requests, and cancellation while reservation is pending. Define what the customer sees in each case. Do not count a logged integration rejection as a customer-visible failure status.

</details>

Continue with [provider calls](/connect/http-and-payments/) when the next step leaves your own application.

<details>
<summary>Framework source notes</summary>

- [testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs)
- [core/service/Service/OutboundIntegration/TH.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/OutboundIntegration/TH.hs)
- [core/service/Integration/Command.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Command.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [testbed/tests/scenarios/stock-reservation.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl)

</details>
