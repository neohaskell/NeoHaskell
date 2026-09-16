---
title: Schedule periodic work
description: Use timer ticks to request work without mistaking them for durable schedules.
sidebar:
  order: 9
---

Some work needs to happen periodically: checking for expired records, polling a service, or refreshing a summary. A timer can request that work while the application’s rules decide what is actually due. Keeping those responsibilities separate makes restart behaviour easier to understand.

NeoHaskell provides a simple in-process timer integration. It is useful for periodic requests while the application is running. It is not a persistent job scheduler that remembers every missed run.

First observe a timer using your existing cart-creation rule. Then design expiry for
stock reservations: their deadlines must survive a restart even though the timer
itself does not.

## Add a small timer to mug-shop

First reuse your cart-creation rule through an internal command. A timer's
commands use the integration dispatcher, which only registers commands declared
with `InternalTransport`. Your existing `CreateCart` belongs to `WebTransport`.
Keep that public action and give the timer a separate entry point.

The internal entry point delegates its decision to the rule you already wrote:

```haskell
decide _ entity context =
  CreateCart.decide CreateCart.CreateCart entity context
```

That delegation is the key idea: change how the request arrives while keeping
cart creation consistent. In `src/Shop/Cart/Commands/CreateCartInternal.hs`,
name the internal request and declare its entity and transport:

```haskell
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

`getEntityId` returns `Nothing`, as the command creates a new cart. The marker
connects the declaration and delegated decision to NeoHaskell's command machinery,
including its generated instances. Use the
[complete integration checkpoint](/examples/mug-shop-connect.tar.gz) for the
surrounding module wiring.

Bring `CreateCartInternal` into scope in `src/Shop/Cart/Service.hs` and add it to
the existing service pipeline:

```haskell
  |> Service.command @CreateCartInternal
```

Do not add both transport types to `CreateCart`; the framework rejects mixing
internal and public transports on a single command. This exercise deliberately
creates empty carts for observation. Remove the timer registration afterwards.

Create `src/Shop/Cart/Timers.hs`:

```haskell
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.Every
    { interval = Timer.seconds 30
    , toCommand = \_ -> CreateCartInternal
    }
    |> Timer.every
```

Bring `periodicCartCreator` into scope in `src/App.hs` and add this registration to the application pipeline:

```haskell
    |> Application.withInbound @() (\_ -> periodicCartCreator)
```

The `@()` factory needs no application configuration. The Cart service now registers
`CreateCartInternal`; the timer submits that command through the integration
runtime. `Integration.Timer` is part of the core package.

Run `neo build`, `neo test`, and `neo run` from `mug-shop`. In another terminal,
query `/queries/cart-summary`: an empty cart should appear after startup and more
should appear while the timer runs. Each reports zero entries. Stop the server
and remove the `withInbound` line after observing the behaviour. A passing build
checks the types; a startup check also verifies command registration and timing.

## Know when it fires

`Timer.every` calls `toCommand` with tick count **1 immediately when the worker starts**, emits that command, and then sleeps. Later ticks increment the count. The interval helpers convert seconds, minutes, and hours to milliseconds.

The tick count restarts with the worker. It is not a durable identifier, a persisted sequence, or evidence of elapsed wall-clock time. Work and dispatch take time too, so this loop is not a calendar-aligned scheduler.

The application restarts inbound workers after reported failures with increasing backoff. That does not recover a durable queue of missed ticks. Running multiple application instances can also create multiple timer workers.

## Adapt the pattern to reservations

In the practice project, design a command that requests an expiry check using durable state. Decide how it finds pending reservations, how much work it performs per run, and how a reservation's own command verifies it is still eligible to expire.

The timer should initiate that process; it should not encode “tick 20 means this reservation expires.” Store the actual deadline with the reservation or its associated workflow. Use the application's clock and persisted facts in the layer responsible for deciding eligibility.

Make repeated expiry checks harmless. For example, an already released reservation should not restore stock again. That rule belongs to your domain and its tests, not to the timer's sleep interval.

## Exercise: restart halfway to expiry

Suppose a reservation expires after ten minutes and the application restarts after six. Explain what happens on the first timer tick after startup.

<details>
<summary>Suggested reasoning and checks</summary>

The timer immediately requests a check, but the reservation still uses its original deadline. Check before expiry, exactly at your chosen boundary, and after expiry. Then repeat the command, restart the worker, and run two workers against the same reservation. The stock result should match your duplicate-handling policy.

A startup test should expect an immediate first command. A clock-controlled business test should prove expiry without sleeping for ten minutes.

</details>

When your application requires a durable schedule, select or build that capability explicitly and connect it through the [inbound integration abstraction](/connect/custom-integrations/). Use [deployment](/operate/deployment/) to reason about worker count and restarts.

<details>
<summary>Framework source notes</summary>

- [core/service/Integration/Timer.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Timer.hs)
- [testbed/src/Testbed/Cart/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)

</details>
