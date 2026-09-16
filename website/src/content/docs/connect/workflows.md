---
title: Coordinate work across entities
description: Connect events and commands across services while keeping each rule explicit.
sidebar:
  order: 1
---

One accepted change can require work in another part of an application. Keeping those responsibilities separate gives each rule a clear home, but introduces a period when one side has changed and the other has not. An integration makes that handoff explicit.

The public testbed demonstrates the pattern with cart and stock entities. A customer adds two mugs: the cart records what they want, then stock decides whether it can reserve them. The example reserves on adding an item; try reserving at checkout or expiring reservations to explore different rules.

## Trace a working connection

The testbed has a `ReserveStockOnItemAdded` handler. This is an exact excerpt from its `handleEvent` function:

```haskell
handleEvent :: CartEntity -> CartEvent -> Integration.Outbound
handleEvent cart event =
  case event of
    ItemAdded {stockId, quantity} ->
      Integration.batch
        [ Integration.outbound
            Command.Emit
              { command =
                  ReserveStock
                    { stockId = stockId
                    , quantity = quantity
                    , cartId = cart.cartId
                    }
              }
        ]
    _ -> Integration.none
```

Read it aloud: when an item is added, ask stock to reserve the quantity for this cart. Other cart events produce no action. `Command.Emit` submits a command to another registered service without making an external HTTP call.

The handler module associates its handler type with `CartEntity`, then ends with the canonical marker:

```haskell
type instance EntityOf ReserveStockOnItemAdded = CartEntity
```

```haskell
outboundIntegration ''ReserveStockOnItemAdded
```

These excerpts belong to the same module. Keep its `Core`, integration, entity, command, and marker imports when studying the complete source. The marker connects `handleEvent` to the outbound machinery; it does not invent the business rule.

The application registers the cart service, stock service, their queries, and this handler. The registration fragment is:

```haskell
    |> Application.withOutbound @ReserveStockOnItemAdded
```

## Observe the result

With the public testbed running, its `tests/scenarios/stock-reservation.hurl` scenario:

1. Initialises 100 units of stock.
2. Creates a cart.
3. Adds five units to that cart.
4. Polls the stock query until it reports 95 available and five reserved.
5. Adds ten more and checks 85 available and 15 reserved.

Use [testing](/build/testing/) for the testbed environment and execution commands. The polling is significant: a successful cart response does not mean the stock query has already caught up.

Explore the same connection in the Neo IDE graph. Identify the originating command, its event, the integration, and the destination command. Then explain where the stock rule executes without reading the handler line by line.

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

## Implementation and examples

- [testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs)
- [core/service/Service/OutboundIntegration/TH.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/OutboundIntegration/TH.hs)
- [core/service/Integration/Command.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Command.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [testbed/tests/scenarios/stock-reservation.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl)

To inspect this example in the IDE, stop any IDE using port 2323, then run
`cd testbed` and `neo ide` from the NeoHaskell checkout in a separate terminal.
The workspace must be the directory containing the testbed `src/`. Create and save
a model if needed; run `neo inspect sync` from that workspace, then click **Open**
to read it. See [first-cart workspace setup](/build/first-cart/) for the full transition.
