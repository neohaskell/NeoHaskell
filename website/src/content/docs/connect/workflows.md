---
title: Coordinate work across entities
description: Connect events and commands across services while keeping each rule explicit.
sidebar:
  order: 1
---

One accepted change can require work in another part of an application. Keeping those responsibilities separate gives each rule a clear home, but introduces a period when one side has changed and the other has not. An integration makes that handoff explicit.

Continue in your own `mug-shop` directory from [stock and checkout](/build/stock-and-checkout/). Its Cart and Stock services already make separate decisions. Now connect them: adding two mugs records the choice in Cart, then asks Stock to reserve two units. This policy reserves on addition; reserving at checkout is a later variation.

All paths below are relative to the `mug-shop` project root. The examples grow the same project. The small declarations explain the decision first; the complete files later in each section are the checkpoint you can copy.

## Decide what crosses the boundary

The handoff has one job: turn an accepted `ItemAdded` event into a stock request. The important value is the command sent to the Stock service:

```haskell
ReserveStock
  { stockId = added.stockId
  , quantity = added.quantity
  , cartId = cart.cartId
  }
```

`added` is the payload inside `ItemAdded`; `cart` supplies the cart identifier. Wrap that value in `Command.Emit` so the integration runtime can deliver it. This is an application command, so it preserves the Stock decision and its refusal rules rather than bypassing them.

## Create the outbound integration

From the project root, create the integration directory:

```sh
mkdir -p src/Shop/Cart/Integrations
```

Create the directory and file from the project root: `mkdir -p src/Shop/Cart/Integrations`, then create `src/Shop/Cart/Integrations/ReserveStockOnItemAdded.hs`. Start with the declaration and the rule below, then copy the complete file. `CartEntity` is the state reconstructed for the event; `CartEvent` is the event family already defined by the Cart slice.

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

When an item is added, ask Stock to reserve the requested quantity. Other Cart events produce no action. `Command.Emit` submits a command to another registered service; it does not make an external HTTP call.

The marker connects `handleEvent` to the outbound machinery. The function still supplies the business rule; the marker does not choose when stock should be reserved.

### Complete integration file

Copy the whole contents to the path named by the fence below, including the imports needed by the declarations above.

<!-- complete-file -->
```haskell title="src/Shop/Cart/Integrations/ReserveStockOnItemAdded.hs"
module Shop.Cart.Integrations.ReserveStockOnItemAdded (
  ReserveStockOnItemAdded (..),
  handleEvent,
) where

import Core
import Integration qualified
import Integration.Command qualified as Command
import Shop.Cart.Core (CartEntity (..), CartEvent (..))
import Shop.Cart.Events.ItemAdded qualified as ItemAdded
import Shop.Stock.Commands.ReserveStock (ReserveStock (..))


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

## Register the Stock command and integration

The new handler can only emit `ReserveStock` if the Stock service registers that command with `InternalTransport`, as the stock lesson's source already does. Keep the public Cart commands too.

Keep the Cart service’s `CreateCart` and `AddItem` registrations. The integration registration belongs in `App.hs`; it is not another Cart command.

If your Cart service still matches that Build checkpoint, the complete file below is the resulting file. If you already added the timer chapter, keep its additional `CreateCartInternal` import and registration.

<!-- complete-file -->
```haskell title="src/Shop/Cart/Service.hs"
module Shop.Cart.Service (service) where

import Core
import Service qualified
import Shop.Cart.Commands.AddItem (AddItem)
import Shop.Cart.Commands.CreateCart (CreateCart)

service :: Service _ _
service = Service.new
  |> Service.command @CreateCart
  |> Service.command @AddItem
```

Do not add `CreateCartInternal` just for this workflow; it is introduced in [the timer lesson](/connect/timers/).

## Append the integration to `App.hs`

In `src/App.hs`, append this import with the other `Shop.Cart` imports:

```haskell
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
```

Append the registration after the existing services and queries, retaining all of them:

```haskell
  |> Application.withOutbound @ReserveStockOnItemAdded
```

If your file still matches the Build checkpoint, replacing it with the complete result below is the shortest route. If you have already added uploads, timers, authentication, or other integrations, keep those imports and registrations and add these two lines in the corresponding places.

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
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
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
  |> Application.withOutbound @ReserveStockOnItemAdded
```

The typed integration reconstructs Cart state from its recorded history. Its registration needs a default starting value for `CartEntity`; the entity marker already supplies it from `initialState`:

```haskell
deriveEntity ''CartEntity ''CartEvent
```

Keep that declaration in `src/Shop/Cart/Entity.hs` after `initialState`, `update`, and `getEventEntityId`. The marker supplies the entity machinery; do not add a second manual `Default` instance.

## Run the connected behaviour

From the project root, stop any existing server before changing files, then run:

```sh
neo build
neo run
```

Use another terminal for the requests from [stock and checkout](/build/stock-and-checkout/). Create fresh stock with three available units and a fresh cart, then add two units using their returned identifiers. Poll the stock query until it reports one available and two reserved. A successful Cart response does not mean the Stock query has already caught up.

Read the Cart summary too. Its `itemCount` is one because it counts entries, even though the addition requested two units. Stock tracks unit quantities. These views answer different questions about the same workflow.

## Add the repeatable integration test

Create `tests/stock-reservation.hurl` from the project root. Stop `neo run` before `neo test`; the CLI starts the test server itself. The complete file below captures fresh identifiers, checks both views, rejects zero without changing those views, then reserves the final remaining unit.

```hurl
POST http://localhost:8080/commands/initialize-stock
Content-Type: application/json
{"productId":"11111111-1111-1111-1111-111111111111","available":3}
HTTP/1.1 200
[Captures]
stock_id: jsonpath "$.entityId"

POST http://localhost:8080/commands/create-cart
Content-Type: application/json
[]
HTTP/1.1 200
[Captures]
cart_id: jsonpath "$.entityId"

POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"{{stock_id}}","quantity":2}
HTTP/1.1 200

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 1
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].isEmpty" nth 0 == false

GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 1
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 2

POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"{{stock_id}}","quantity":0}
HTTP/1.1 400

GET http://localhost:8080/queries/cart-summary
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 1

GET http://localhost:8080/queries/stock-level
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 1
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 2

POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"{{stock_id}}","quantity":1}
HTTP/1.1 200

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 2

GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200
HTTP/1.1 200
[Asserts]
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 0
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 3
```

Run `neo test` from `mug-shop`. The query retries wait for the asynchronous integration and projections; they do not resubmit an accepted addition. Keep the smaller Cart and Stock tests too: this scenario tests connected behaviour, while the smaller tests show which local rule failed.

## The case the happy path does not settle

> **Jess:** “If stock is unavailable, undo adding the item automatically.”
>
> **Agent:** “The stock command rejects the reservation, so the cart is unchanged.”
>
> **Jess:** “The cart event was already accepted. Show me the return path that updates the cart.”

A rejection on the Stock side cannot erase an already recorded Cart event. The handler above supplies one direction of communication. A complete workflow needs an explicit outcome path, such as recording reservation failure and changing what checkout permits. Implementing that return path is a useful extension to the practice project.

This is a **process manager** problem: coordinating steps across entities, tracking progress, and handling incomplete work. Represent pending work and its recovery explicitly. In this example, checkout cannot be considered complete merely because the Cart accepted an item.

## Exercise: choose when stock is reserved

Change the practice project's policy from “on add” to “on checkout request.” Write the event sequence before changing code. Adding to the cart should no longer reserve stock. Checkout should request a reservation once for a stable business operation. Check sufficient stock, insufficient stock, duplicate requests, and cancellation while reservation is pending. Define what the customer sees in each case.

Continue with [provider calls](/connect/http-and-payments/) when the next step leaves your own application.

<details>
<summary>Framework source notes</summary>

- [testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations/ReserveStockOnItemAdded.hs)
- [core/service/Service/OutboundIntegration/TH.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/OutboundIntegration/TH.hs)
- [core/service/Integration/Command.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Command.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [testbed/tests/scenarios/stock-reservation.hurl](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl)

</details>
