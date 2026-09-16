---
title: "Coordinating changes: stock and checkout"
description: Add a second domain and define where its decisions need coordination.
sidebar:
  order: 5
---

One accepted action can lead to another decision. A scheduling app may accept a request before a room is reserved; a document workflow may save a draft before a reviewer accepts it. A useful application makes that distinction visible.

Your practice project now gains **Stock**. A cart records selections; stock tracks available and reserved units. We will implement and test the stock decision here, then connect it to cart additions in [the integration lesson](/connect/workflows/).

Examples below show the relevant declarations and behaviour, with each destination named. Module headers and imports are omitted so you can focus on the idea. The [complete end of Build files](/examples/mug-shop-build.tar.gz) include that setup and the tests, including the configuration explained in a later lesson. Use them as a reference, or add them to this same project when you want the complete checkpoint.

## State the promises

`InitializeStock` creates a record with a nonnegative available quantity. `ReserveStock` reserves a positive quantity only when enough remains. A reservation moves units from `available` to `reserved`.

Product and stock IDs have different jobs. The product identifies the mug design; the stock ID identifies the availability record. In this exercise, initialise one stock record per product yourself; the command does not enforce product uniqueness.

From your project root, create the module directories:

```sh
mkdir -p src/Shop/Stock/Commands src/Shop/Stock/Events src/Shop/Stock/Queries
```

## Give each fact a focused file

Initialisation records the product and starting quantity. Reservation records a quantity committed to a cart. Together they form the stock domain's event type in `src/Shop/Stock/Event.hs`:

```haskell
data StockEvent
  = StockInitialized StockInitialized.Event
  | StockReserved StockReserved.Event
```

The event marker handles the standard instances:

```haskell
EventTH.event ''StockEvent
```

Each payload lives separately in `Events/`. `Event.hs` lists the possible facts and identifies which stock stream each affects.

## Apply accepted history

The entity holds the current availability. Applying a reservation moves its quantity between the two counts:

```haskell
  StockReserved reservation ->
    stock
      { available = stock.available - reservation.quantity
      , reserved = stock.reserved + reservation.quantity
      }
```

That update belongs in `Entity.hs`. It does not ask today's warehouse whether yesterday's accepted reservation was reasonable. The command validates the request before it becomes a fact.

As in Cart, `Core.hs` only re-exports the domain types and their operations. Adding a command does not turn it into a large implementation file.

## Establish stock, including zero

In `src/Shop/Stock/Commands/InitializeStock.hs`, `InitializeStock` has two input fields:

```haskell
data InitializeStock = InitializeStock
  { productId :: Uuid
  , available :: Int
  }
```

The command generates a stock ID and refuses a negative initial quantity. Zero is allowed: a product can have a stock record while none remain available. Once its decision and entity/transport declarations are in place, its marker connects them:

```haskell
command ''InitializeStock
```

## Protect a reservation

`ReserveStock` checks existence, positive quantity, and availability. Its final decision in `src/Shop/Stock/Commands/ReserveStock.hs` compares the request with the current state:

```haskell
  if request.quantity > stock.available
    then Decider.reject "Insufficient stock available!"
    else Decider.acceptExisting
      [StockReserved (StockReserved.Event {entityId = stock.stockId, quantity = request.quantity, cartId = request.cartId})]
```

This command uses `InternalTransport`. It is intended for application work; we are not exposing it as a customer HTTP endpoint. The integration lesson will supply its trigger.

The [testing lesson](/build/testing/) calls this decision directly. You can establish the last-unit rule before any automation invokes it.

## Present the result and register the domain

`StockLevel` provides a view of the product, availability, and reserved quantity. Its current public policy suits this local practice; reconsider what a real catalogue should reveal.

Append these steps to the existing application pipeline, preserving Cart and any other registrations:

```haskell
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
```

Run `neo build`, then `neo run`.

## Create and inspect stock

```sh
curl -i http://localhost:8080/commands/initialize-stock \
  -H 'Content-Type: application/json' \
  --data '{"productId":"11111111-1111-1111-1111-111111111111","available":3}'
```

Keep the returned `entityId` as your stock ID. Read its view:

```sh
curl --get http://localhost:8080/queries/stock-level \
  --data-urlencode 'q=.stockLevelId == "YOUR-STOCK-UUID"'
```

Once the projection catches up, expect three available and zero reserved. Create a cart and submit `AddItem` with this stock ID and quantity two. The cart should have one entry. **Stock still has three available and zero reserved**: we have implemented both decisions, but have not connected them.

That observation is evidence. Two registered services do not imply that one calls the other. In [connecting application steps](/connect/workflows/) you will add the connection and check the change to one available and two reserved.

## Keep a repeatable stock check

Save the HTTP scenario below. It creates its own record, waits for its view, and checks refusal of a negative initial quantity. Stop `neo run` before executing `neo test`.

<details>
<summary>Complete file: tests/scenarios/stock-flow.hurl</summary>

Save as `tests/scenarios/stock-flow.hurl`:

```hurl
POST http://localhost:8080/commands/initialize-stock
Content-Type: application/json
{"productId":"11111111-1111-1111-1111-111111111111","available":3}

HTTP 200
[Captures]
stock_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200

HTTP 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 3
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 0

POST http://localhost:8080/commands/initialize-stock
Content-Type: application/json
{"productId":"22222222-2222-2222-2222-222222222222","available":-1}

HTTP 400
[Asserts]
jsonpath "$.reason" == "Available stock cannot be negative"
```

</details>

## Decide what checkout will promise

Even after connecting the domains, a cart addition can be accepted while its later reservation is refused. A checkout needs an observable reservation outcome and a response to partial failure. Design the next promises as further slices:

| Promise | Decision still needed |
| --- | --- |
| Stock was reserved | How does Cart learn whether reservation succeeded? |
| An order was accepted | Which prices, quantities, currency, and delivery details become fixed? |
| Payment was confirmed | Which provider evidence establishes payment, including late or duplicate replies? |
| A reservation expired | Which fact releases it, and how does expiry interact with payment? |

These are application policies, not consequences of naming a domain Stock or Cart.

## Exercise: the last mug

Your agent says a successful cart request proves that the last mug belongs to the customer. Identify the missing evidence.

<details>
<summary>Suggested reasoning and checks</summary>

The cart request establishes a selection. Check the reservation decision and its recorded outcome. Reserve two from three, refuse four from three, and accept exactly three. Competing requests for the last unit need a concurrent application-level check. Repeated requests need a deliberate duplicate policy; the current command can reserve again when enough stock remains.

</details>

Next: [HTTP and frontends](/build/http-and-frontend/) turns these outcomes into an honest interface.
