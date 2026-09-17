---
title: Schedule periodic work
description: Use timer ticks to request work without mistaking them for durable schedules.
sidebar:
  order: 9
---

Some work needs to happen periodically: checking for expired records, polling a service, or refreshing a summary. A timer can request that work while the application's rules decide what is actually due. Keeping those responsibilities separate makes restart behaviour easier to understand.

NeoHaskell provides a simple in-process timer integration. It is useful for periodic requests while the application is running. It is not a persistent job scheduler that remembers every missed run.

First observe a timer using your existing cart-creation rule. Then design expiry for stock reservations: their deadlines must survive a restart even though the timer itself does not.

All paths below are relative to your `mug-shop` project root. The lesson creates three Cart files, replaces one service file, and appends one application registration. The focused declarations come first; complete resulting files follow each change.

## Give the timer an internal command

A timer's commands use the integration dispatcher, which only registers commands declared with `InternalTransport`. The existing `CreateCart` belongs to `WebTransport`. Keep that public action and give the timer a separate entry point that delegates to the same decision.

The delegation is the business choice: change how the request arrives while keeping cart creation consistent.

```haskell
decide _ entity context =
  CreateCart.decide CreateCart.CreateCart entity context
```

Create `src/Shop/Cart/Commands/CreateCartInternal.hs`. The command has no fields and creates a new cart, so `getEntityId` returns `Nothing`. Its transport is internal:

```haskell
data CreateCartInternal = CreateCartInternal

getEntityId :: CreateCartInternal -> Maybe Uuid
getEntityId _ = Nothing

type instance EntityOf CreateCartInternal = CartEntity
type instance TransportsOf CreateCartInternal = '[InternalTransport]

deriveCommand ''CreateCartInternal
```

### Complete internal command file

Create the file at the path named by the fence and copy the whole file, including its module header and imports.

<!-- complete-file -->
```haskell title="src/Shop/Cart/Commands/CreateCartInternal.hs"
module Shop.Cart.Commands.CreateCartInternal (
  CreateCartInternal (..),
  getEntityId,
  decide,
) where

import Core
import Service.Auth (RequestContext)
import Service.Command.Core (TransportsOf)
import Service.Transport.Internal (InternalTransport)
import Shop.Cart.Commands.CreateCart qualified as CreateCart
import Shop.Cart.Core (CartEntity, CartEvent)


data CreateCartInternal = CreateCartInternal


getEntityId :: CreateCartInternal -> Maybe Uuid
getEntityId _ = Nothing


decide :: CreateCartInternal -> Maybe CartEntity -> RequestContext -> Decision CartEvent
decide _ entity context =
  CreateCart.decide CreateCart.CreateCart entity context


type instance EntityOf CreateCartInternal = CartEntity
type instance TransportsOf CreateCartInternal = '[InternalTransport]


deriveCommand ''CreateCartInternal
```

Do not add both transport types to `CreateCart`; the framework rejects mixing internal and public transports on a single command. This exercise creates empty carts for observation. Remove the timer registration after observing it.

## Replace the Cart service registration

Replace `src/Shop/Cart/Service.hs` with the file below, or append the final `Service.command` line to your existing Cart service if its first two registrations are unchanged:

```haskell
  |> Service.command @CreateCartInternal
```

The command must be registered before the timer can dispatch it. This complete file is the exact Connect overlay.

<!-- complete-file -->
```haskell title="src/Shop/Cart/Service.hs"
module Shop.Cart.Service (service) where

import Core
import Service qualified
import Shop.Cart.Commands.AddItem (AddItem)
import Shop.Cart.Commands.CreateCart (CreateCart)
import Shop.Cart.Commands.CreateCartInternal (CreateCartInternal)

service :: Service _ _
service = Service.new
  |> Service.command @CreateCart
  |> Service.command @AddItem
  |> Service.command @CreateCartInternal
```

## Create the timer integration

Create `src/Shop/Cart/Timers.hs`. The timer converts every tick into the internal command. The tick value is deliberately ignored: the command is the work request, not a durable schedule identifier.

```haskell
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.Every
    { interval = Timer.seconds 30
    , toCommand = \_ -> CreateCartInternal
    }
    |> Timer.every
```

### Complete timer file

Copy this whole file to the path in the title.

<!-- complete-file -->
```haskell title="src/Shop/Cart/Timers.hs"
module Shop.Cart.Timers (periodicCartCreator) where

import Core
import Integration qualified
import Integration.Timer qualified as Timer
import Shop.Cart.Commands.CreateCartInternal (CreateCartInternal (..))


periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.Every
    { interval = Timer.seconds 30
    , toCommand = \_ -> CreateCartInternal
    }
    |> Timer.every
```

## Append the timer to `App.hs`

In `src/App.hs`, add this import with the other Cart imports:

```haskell
import Shop.Cart.Timers (periodicCartCreator)
```

Append the registration after the existing service and query registrations:

```haskell
  |> Application.withInbound @() (\_ -> periodicCartCreator)
```

The `@()` factory needs no application configuration. This complete file continues the workflow and upload lessons, retaining their registrations while adding the timer. If you skipped either optional feature, omit its import and registration; preserve any authentication settings you added.

<!-- complete-file -->
```haskell title="src/App.hs"
module App (app) where

import Core
import Shop.Uploads qualified as Uploads
import Shop.Cart.Integrations.ReserveStockOnItemAdded (ReserveStockOnItemAdded)
import Maybe qualified
import Path qualified
import Service.Application (Application)
import Service.Application qualified as Application
import Service.EventStore.Simple (SimpleEventStore (..))
import Service.Transport.Web qualified as WebTransport
import Shop.Config (ShopConfig (..))
import Shop.Cart.Queries.CartSummary (CartSummary)
import Shop.Cart.Service qualified as Cart
import Shop.Cart.Timers (periodicCartCreator)
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
  |> Application.withFileUpload @() (\_ -> Uploads.uploadConfig)
  |> Application.withInbound @() (\_ -> periodicCartCreator)
```

## Run it and observe the first tick

Stop any running server before editing, then run from `mug-shop`:

```sh
neo build
neo test
neo run
```

In another terminal, query `/queries/cart-summary`. An empty cart should appear after startup and more should appear while the timer runs. Each reports zero entries. Stop the server and remove the `withInbound` line and the `Shop.Cart.Timers` import after observing the behaviour. Keep the command and timer modules if you want the complete checkpoint to build; an unregistered timer does not run.

`Timer.every` calls `toCommand` with tick count **1 immediately when the worker starts**, emits that command, and then sleeps. Later ticks increment the count. The interval helpers convert seconds, minutes, and hours to milliseconds.

The tick count restarts with the worker. It is not a durable identifier, a persisted sequence, or evidence of elapsed wall-clock time. Work and dispatch take time too, so this loop is not a calendar-aligned scheduler. The application restarts inbound workers after reported failures with increasing backoff; that does not recover a durable queue of missed ticks. Multiple application instances can also create multiple timer workers.

## Adapt the pattern to reservations

In the practice project, design a command that requests an expiry check using durable state. Decide how it finds pending reservations, how much work it performs per run, and how a reservation's own command verifies that it is still eligible to expire.

The timer should initiate that process; it should not encode “tick 20 means this reservation expires.” Store the actual deadline with the reservation or its associated workflow. Use the application's clock and persisted facts in the layer responsible for deciding eligibility.

Make repeated expiry checks harmless. For example, an already released reservation should not restore stock again. That rule belongs to your domain and its tests, not to the timer's sleep interval.

## Exercise: restart halfway to expiry

Suppose a reservation expires after ten minutes and the application restarts after six. Explain what happens on the first timer tick after startup. The timer immediately requests a check, but the reservation still uses its original deadline. Check before expiry, exactly at your chosen boundary, and after expiry. Then repeat the command, restart the worker, and run two workers against the same reservation. The stock result should match your duplicate-handling policy.

A startup test should expect an immediate first command. A clock-controlled business test should prove expiry without sleeping for ten minutes.

When your application requires a durable schedule, select or build that capability explicitly and connect it through the [inbound integration abstraction](/connect/custom-integrations/). Use [deployment](/operate/deployment/) to reason about worker count and restarts.

<details>
<summary>Framework source notes</summary>

- [core/service/Integration/Timer.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Timer.hs)
- [testbed/src/Testbed/Cart/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)

</details>
