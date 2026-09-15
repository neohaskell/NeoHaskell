---
title: Connect the shop
description: Turn business events into useful work across services and providers.
sidebar:
  order: 0
---

An order may be recorded in your shop while its confirmation email has not yet been accepted by the email provider. Both facts matter. A customer should not lose their order because an email service is unavailable, and your team needs to see which work remains unfinished.

An **integration** connects a fact in your application to another piece of work. NeoHaskell lets you describe the request and turn its result back into a command. You still decide what success means, how failures appear to people, and which operations may safely be repeated.

Before implementing integrations, be comfortable with [commands and events](/build/commands-and-events/) and [queries](/build/queries/). Evaluators can read the opening and decision sections throughout this chapter without following the code.

If you have followed the public examples, first [assemble the cart and stock slice in your own project](/build/your-shop/). Keep its working tests as you add external effects.

## Follow the business conversation

Consider an order confirmation. This is an original shop design, not a built-in commerce workflow:

1. A command accepts the order and records an order-placed event.
2. An outbound handler recognises that event and describes an email request.
3. The integration runs the request outside the order's decision function.
4. Its callback produces a command recording either provider acceptance or failure.
5. A query gives staff the resulting status.

The fourth step is deliberately a **command**: new information still enters through the application's rules. Calling a provider does not directly edit an entity or a query.

An **outbound** integration reacts to application events. An **inbound** integration starts from something outside that event flow, such as a timer, and submits a command.

## Read the small vocabulary

These signatures are excerpts from `core/service/Integration.hs`:

```haskell
batch :: Array Action -> Outbound
none :: Outbound
```

`Integration.outbound` turns a supported request record into one action. `Integration.batch` collects actions to perform for an event. `Integration.none` says that this handler has no work for that event. A batch is a collection, not a transaction spanning external systems.

The concrete request type determines available fields. Most provider integrations offer `onSuccess` and `onError`. Both callbacks must produce the **same command type**. A command with success/failure alternatives is one option; two unrelated command types will not satisfy a single `Request command`.

## Register both ends

Defining a handler does not register it. The application must register the outbound handler and the service containing each command it can emit. Follow the complete public cart-to-stock example in [workflows](/connect/workflows/).

A missing callback handler is logged by the dispatcher. It is not proof that the original external operation failed: the provider might already have accepted it.

## Where confidence comes from

Test three boundaries separately:

- **Selection:** does the intended event produce an action, and an unrelated event produce none?
- **Translation:** does a provider result produce the right command, including malformed results and refusals?
- **Completion:** does that command reach the registered service and produce the expected query state?

The integration runtime uses per-entity workers and bounded queues. It includes error handling and configurable timeouts. Those mechanisms do not establish durable delivery or exactly-once execution across a process crash and an external provider. Important unfinished work needs an application-level recovery design.

Typed handlers reconstruct the entity from its stream when processing; they do not receive a guaranteed snapshot taken at the triggering event. Put information that must describe that particular occurrence in the event itself.

## Choose the next need

- [Coordinate cart and stock](/connect/workflows/).
- [Call a provider and design payment boundaries](/connect/http-and-payments/).
- [Connect a merchant’s provider account](/connect/provider-accounts/) through explicit OAuth consent.
- [Send email](/connect/email/).
- [Attach files](/connect/files/) and [extract document or audio content](/connect/documents/).
- [Add AI assistance](/connect/ai/) and [constrain AI tools](/connect/ai-tools/).
- [Schedule periodic work](/connect/timers/).
- [Build a reusable integration](/connect/custom-integrations/).

**Try this:** describe what staff should see when an order exists but its email request times out. Include a state for an unknown provider outcome. Check your explanation with a colleague before choosing event names.

## Implementation and examples

- [core/service/Integration.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)
- [core/service/Service/Integration/Dispatcher.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Integration/Dispatcher.hs)
