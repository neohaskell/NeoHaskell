---
title: "Coordinating changes: stock and checkout"
description: Add a second domain and define where its decisions need coordination.
sidebar:
  order: 5
---

One accepted action can lead to another decision. A scheduling app may accept a request before a room is reserved; a document workflow may save a draft before a reviewer accepts it. A useful application makes that distinction visible.

Your practice project now gains **Stock**. A cart records selections; stock tracks available and reserved units. We will implement and test the stock decision here, then connect it to cart additions in [the integration lesson](/connect/workflows/).

Examples below show the relevant declarations and behaviour, with each destination named. The small snippets teach one decision at a time. The assembled Stock files later in this page include the complete modules needed for this checkpoint. The [complete end of Build files](/examples/mug-shop-build.tar.gz) is supplementary; you can build the checkpoint by creating the files here in the same project.

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
deriveEvent ''StockEvent
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
deriveCommand ''InitializeStock
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

## Decide what checkout will promise

Even after connecting the domains, a cart addition can be accepted while its later reservation is refused. A checkout needs an observable reservation outcome and a response to partial failure. Design the next promises as further slices:

| Promise | Decision still needed |
| --- | --- |
| Stock was reserved | How does Cart learn whether reservation succeeded? |
| An order was accepted | Which prices, quantities, currency, and delivery details become fixed? |
| Payment was confirmed | Which provider evidence establishes payment, including late or duplicate replies? |
| A reservation expired | Which fact releases it, and how does expiry interact with payment? |

These are application policies, not consequences of naming a domain Stock or Cart.

## Assemble the Stock checkpoint

Once the decisions make sense, create the directories from the earlier command and add or replace the files below in the same `mug-shop` project. Keep the Cart files and `tests/Spec.hs` that you already have. This checkpoint keeps the nonpersistent local store from the first cart lesson; if you have added authentication or another transport policy, merge the Stock service and query steps into your existing `app` pipeline instead.

<!-- complete-file -->
```haskell title="src/Shop/Stock/Events/StockInitialized.hs"
module Shop.Stock.Events.StockInitialized (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , productId :: Uuid
  , available :: Int
  }
  deriving (Eq)

deriveEvent ''Event
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Events/StockReserved.hs"
module Shop.Stock.Events.StockReserved (Event (..)) where

import Core

data Event = Event
  { entityId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }
  deriving (Eq)

deriveEvent ''Event
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Event.hs"
module Shop.Stock.Event (StockEvent (..), getEventEntityId) where

import Core
import Shop.Stock.Events.StockInitialized qualified as StockInitialized
import Shop.Stock.Events.StockReserved qualified as StockReserved

data StockEvent
  = StockInitialized StockInitialized.Event
  | StockReserved StockReserved.Event
  deriving (Eq)

getEventEntityId :: StockEvent -> Uuid
getEventEntityId change = case change of
  StockInitialized fact -> fact.entityId
  StockReserved fact -> fact.entityId

deriveEvent ''StockEvent
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Entity.hs"
module Shop.Stock.Entity (StockEntity (..), initialState, update) where

import Core
import Shop.Stock.Event (StockEvent (..), getEventEntityId)
import Shop.Stock.Events.StockInitialized qualified as StockInitialized
import Shop.Stock.Events.StockReserved qualified as StockReserved
import Uuid qualified

data StockEntity = StockEntity
  { stockId :: Uuid
  , productId :: Uuid
  , available :: Int
  , reserved :: Int
  }

initialState :: StockEntity
initialState = StockEntity {stockId = Uuid.nil, productId = Uuid.nil, available = 0, reserved = 0}

update :: StockEvent -> StockEntity -> StockEntity
update change stock = case change of
  StockInitialized initialized ->
    StockEntity
      { stockId = initialized.entityId
      , productId = initialized.productId
      , available = initialized.available
      , reserved = 0
      }
  StockReserved reservation ->
    stock
      { available = stock.available - reservation.quantity
      , reserved = stock.reserved + reservation.quantity
      }

deriveEntity ''StockEntity ''StockEvent
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Core.hs"
module Shop.Stock.Core (
  module Shop.Stock.Entity,
  module Shop.Stock.Event,
) where

import Shop.Stock.Entity
import Shop.Stock.Event
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Commands/InitializeStock.hs"
module Shop.Stock.Commands.InitializeStock (
  InitializeStock (..),
  getEntityId,
  decide,
) where

import Core
import Shop.Stock.Events.StockInitialized qualified as StockInitialized
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.Transport.Web (WebTransport)
import Shop.Stock.Core

data InitializeStock = InitializeStock
  { productId :: Uuid
  , available :: Int
  }

getEntityId :: InitializeStock -> Maybe Uuid
getEntityId _ = Nothing

decide :: InitializeStock -> Maybe StockEntity -> RequestContext -> Decision StockEvent
decide request existing _context = case existing of
  Just _ -> Decider.reject "Stock already initialized for this product!"
  Nothing -> initialize request

initialize :: InitializeStock -> Decision StockEvent
initialize request =
  if request.available < 0
    then Decider.reject "Available stock cannot be negative"
    else do
      stockId <- Decider.generateUuid
      Decider.acceptNew
        [StockInitialized (StockInitialized.Event {entityId = stockId, productId = request.productId, available = request.available})]

type instance EntityOf InitializeStock = StockEntity

type instance TransportsOf InitializeStock = '[WebTransport]

deriveCommand ''InitializeStock
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Commands/ReserveStock.hs"
module Shop.Stock.Commands.ReserveStock (
  ReserveStock (..),
  getEntityId,
  decide,
) where

import Core
import Shop.Stock.Events.StockReserved qualified as StockReserved
import Decider qualified
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.Transport.Internal (InternalTransport)
import Shop.Stock.Core

-- | Command to reserve stock for a cart.
-- Keep reservation internal; the integration lesson supplies its trigger.
data ReserveStock = ReserveStock
  { stockId :: Uuid
  , quantity :: Int
  , cartId :: Uuid
  }

getEntityId :: ReserveStock -> Maybe Uuid
getEntityId cmd = Just cmd.stockId

decide :: ReserveStock -> Maybe StockEntity -> RequestContext -> Decision StockEvent
decide request existing _context = case existing of
  Nothing -> Decider.reject "Stock not found!"
  Just stock -> reservePositiveQuantity request stock

reservePositiveQuantity :: ReserveStock -> StockEntity -> Decision StockEvent
reservePositiveQuantity request stock =
  if request.quantity <= 0
    then Decider.reject "Quantity must be positive"
    else reserveAvailableStock request stock

reserveAvailableStock :: ReserveStock -> StockEntity -> Decision StockEvent
reserveAvailableStock request stock =
  if request.quantity > stock.available
    then Decider.reject "Insufficient stock available!"
    else Decider.acceptExisting
      [StockReserved (StockReserved.Event {entityId = stock.stockId, quantity = request.quantity, cartId = request.cartId})]

type instance EntityOf ReserveStock = StockEntity

type instance TransportsOf ReserveStock = '[InternalTransport]

deriveCommand ''ReserveStock
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Queries/StockLevel.hs"
module Shop.Stock.Queries.StockLevel (
  StockLevel (..),
  canAccess,
  canView,
) where

import Core
import Service.AccessControl (AccessError, UserClaims)
import Service.AccessControl qualified as AccessControl
import Shop.Stock.Core (StockEntity (..))

data StockLevel = StockLevel
  { stockLevelId :: Uuid
  , productId :: Uuid
  , available :: Int
  , reserved :: Int
  }

-- | Authorization: Anyone can access stock levels (public catalog data)
canAccess :: Maybe UserClaims -> Maybe AccessError
canAccess claims = AccessControl.publicAccess claims

-- | Authorization: Anyone can view any stock level
canView :: Maybe UserClaims -> StockLevel -> Maybe AccessError
canView claims stockLevel = AccessControl.publicView claims stockLevel

-- | Use TH to derive Query instances.
-- Wires canAccess -> canAccessImpl, canView -> canViewImpl
deriveQuery ''StockLevel [''StockEntity]

instance QueryOf StockEntity StockLevel where
  queryId stock = stock.stockId

  combine stock _maybeExisting =
    Update
      StockLevel
        { stockLevelId = stock.stockId
        , productId = stock.productId
        , available = stock.available
        , reserved = stock.reserved
        }
```

<!-- complete-file -->
```haskell title="src/Shop/Stock/Service.hs"
module Shop.Stock.Service (
  service,
) where

import Core
import Service qualified
import Shop.Stock.Commands.InitializeStock (InitializeStock)
import Shop.Stock.Commands.ReserveStock (ReserveStock)
import Shop.Stock.Core ()

service :: Service _ _
service =
  Service.new
    |> Service.command @InitializeStock
    |> Service.command @ReserveStock
```

If you have completed the Cart and configuration lessons, `src/App.hs` should
contain the Stock service and query registrations shown here. Create or replace
only the pipeline if your application has no additional policies yet; otherwise
append the final two steps while keeping your existing store, transport, and
Cart registrations.

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
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Stock.Queries.StockLevel (StockLevel)
import Shop.Stock.Service qualified as Stock

app :: Application
app = Application.new
  |> Application.withEventStore @() (\_ -> SimpleEventStore
    { basePath = Path.fromText ".neo/events" |> Maybe.getOrDie
    , persistent = False
    })
  |> Application.withTransport WebTransport.server
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
  |> Application.withService Stock.service
  |> Application.withQuery @StockLevel
```

## Create and inspect stock

Now run the checkpoint from the project root:

```sh
neo build
neo run
```

Create a stock record:

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

Save the HTTP scenario below as `tests/scenarios/stock-flow.hurl`. It creates its own record, waits for its view, and checks refusal of a negative initial quantity. Stop `neo run` before executing `neo test`.

<details>
<summary>Complete file: tests/scenarios/stock-flow.hurl</summary>

<!-- complete-file -->
```hurl title="tests/scenarios/stock-flow.hurl"
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

Run `neo test` from the project root. The scenario's captured stock ID keeps
the check independent from earlier runs, and its query retry allows the
projection to catch up. This page does not yet connect `AddItem` to
`ReserveStock`; that trigger is an internal integration taught in [Connect](/connect/workflows/).

## Exercise: the last mug

Your agent says a successful cart request proves that the last mug belongs to the customer. Identify the missing evidence.

<details>
<summary>Suggested reasoning and checks</summary>

The cart request establishes a selection. Check the reservation decision and its recorded outcome. Reserve two from three, refuse four from three, and accept exactly three. Competing requests for the last unit need a concurrent application-level check. Repeated requests need a deliberate duplicate policy; the current command can reserve again when enough stock remains.

</details>

Next: [HTTP and frontends](/build/http-and-frontend/) turns these outcomes into an honest interface.
