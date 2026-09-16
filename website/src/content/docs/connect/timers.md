---
title: Schedule periodic work
description: Use timer ticks to request work without mistaking them for durable schedules.
sidebar:
  order: 9
---

Some work needs to happen periodically: checking for expired records, polling a service, or refreshing a summary. A timer can request that work while the application’s rules decide what is actually due. Keeping those responsibilities separate makes restart behaviour easier to understand.

NeoHaskell provides a simple in-process timer integration. It is useful for periodic requests while the application is running. It is not a persistent job scheduler that remembers every missed run.

For practice, we will use expiring stock reservations in the ecommerce project. Their deadlines must survive a restart even though the timer itself does not.

## Read a complete public timer

The public testbed defines this inbound integration:

```haskell
periodicCartCreator :: Integration.Inbound
periodicCartCreator =
  Timer.every Timer.Every
    { interval = Timer.seconds 3
    , toCommand = \_ -> CreateCartInternal
    }
```

Its module imports `Integration`, `Integration.Timer` as `Timer`, and `CreateCartInternal`. The application registers it with this fragment:

```haskell
    |> Application.withInbound @() (\_ -> periodicCartCreator)
```

The `@()` factory means this registration needs no application configuration value. The emitted command must belong to a registered service. Study this in the testbed environment from [testing](/build/testing/); creating carts repeatedly demonstrates command submission rather than a useful expiry policy.

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

## Implementation and examples

- [core/service/Integration/Timer.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Timer.hs)
- [testbed/src/Testbed/Cart/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)
