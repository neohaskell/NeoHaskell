---
title: Stock and checkout
description: Coordinate a customer's choices with stock, and design an honest checkout promise.
sidebar:
  order: 5
---

A mug in a cart is a customer's intention. A mug reserved in the warehouse is a commitment of limited stock. If two customers want the last mug, the shop needs a rule for who receives that commitment—and a way to tell the other customer what happened.

This is where the application begins coordinating separate responsibilities. NeoHaskell provides commands, events, and integrations for that coordination. You still decide when the shop is entitled to say “your order is confirmed.”

## Follow the existing reservation

The public testbed demonstrates this flow:

1. `InitializeStock` establishes available units for a product.
2. `AddItem` records a customer's choice in a cart.
3. The `ReserveStockOnItemAdded` integration reacts to `ItemAdded`.
4. It emits an internal `ReserveStock` command.
5. Stock accepts the reservation only when the quantity is positive and enough units remain.

The Stock entity tracks `available` and `reserved`. Applying `StockReserved` subtracts the quantity from the first and adds it to the second. `ReserveStock` uses `InternalTransport`; the public example does not expose it as a customer HTTP endpoint.

## Run a reservation

Use the testbed started in [your first cart](/build/first-cart/). Create a stock record for the fictional mug:

```sh
curl http://localhost:8080/commands/initialize-stock \
  -H 'Content-Type: application/json' \
  --data '{"productId":"11111111-1111-1111-1111-111111111111","available":3}'
```

Keep its returned `entityId` as your stock ID. Create a cart with the earlier `create-cart` request and keep that ID too. Replace both placeholders below:

```sh
curl http://localhost:8080/commands/add-item \
  -H 'Content-Type: application/json' \
  --data '{"cartId":"YOUR-CART-UUID","stockId":"YOUR-STOCK-UUID","quantity":2}'
```

Then read the stock projection, using your actual stock ID:

```sh
curl --get http://localhost:8080/queries/stock-level \
  --data-urlencode 'q=.stockLevelId == "YOUR-STOCK-UUID"'
```

After the integration and projection have caught up, expect `available = 1` and `reserved = 2`. The public acceptance scenario exercises the same sequence with 100 units and checks two cumulative reservations. Run it separately with:

```sh
./dev exec hurl --test testbed/tests/scenarios/stock-reservation.hurl
```

## An accepted addition is not a confirmed reservation

This distinction is essential. Cart and Stock receive separate commands. The cart addition can succeed even if a later reservation is rejected. The example does not compensate by removing the cart entry or recording a customer-visible reservation failure.

Try adding two more units to your three-unit stock after reserving the first two. The cart's positive-quantity rule can accept the addition, but Stock should reject reserving more than its remaining one. Verify both views rather than treating the initial HTTP 200 as the final outcome.

That is useful demonstration behaviour, but insufficient checkout policy for a live shop.

## Design the next promise explicitly

Before adding payments, sketch these **proposed shop states**, which are not implemented order types in the testbed:

| Situation | What the shop can honestly say |
| --- | --- |
| Cart selection recorded | “We saved your choices.” |
| Reservation pending | “We are checking availability.” |
| Reservation confirmed | “These units are reserved under our stated expiry policy.” |
| Payment result pending | “Payment is being confirmed.” |
| Order accepted | “The shop has accepted this order.” |

Decide how reservation expiry, failed payment, cancellation, and shipment affect stock. Define how to recognise a repeated request before retrying it. A payment provider's result needs its own reconciliation; no built-in payment adapter is assumed here.

This work belongs in the event model before the agent chooses an implementation. Continue in [connect the shop](/connect/) for outbound actions and follow-up commands.

## Exercise: reserve on checkout instead

The merchant wants customers to browse freely and reserve stock only at checkout. Identify which existing connection must change, then describe the evidence needed before accepting the change.

<details>
<summary>Suggested reasoning and checks</summary>

Stop treating every `ItemAdded` as a reservation trigger. Introduce a checkout request and a visible reservation outcome. Test enough stock, insufficient stock, exactly the remaining stock, two competing requests for the last unit, and repeated delivery of the same request. Also test release or expiry according to the chosen policy. The public happy-path scenario is an anchor, not evidence that this proposed workflow already handles those cases.

</details>

Public sources: [reservation integration](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs), [ReserveStock](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Stock/Commands/ReserveStock.hs), [Stock state](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Stock/Core.hs), [acceptance scenario](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl).
