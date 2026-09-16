---
title: Integrations
description: Connect application events, external services, and follow-up commands.
sidebar:
  order: 0
---

Saving a change and completing its follow-up work are separate things. A document can be saved before its preview is generated; a message can be recorded before its notification is sent. Your application needs to represent both the accepted change and the work still to do.

An **integration** connects a fact in your application to another piece of work. NeoHaskell lets you describe the request and turn its result back into a command. You still decide what success means, how failures appear to people, and which operations may safely be repeated.

Before implementing integrations, be comfortable with [commands and events](/build/commands-and-events/) and [queries](/build/queries/). The examples extend the ecommerce practice project, while the same integration lifecycle applies to other applications. Evaluators can follow the opening and decision sections without implementing the examples.

To practise with the continuing example, [assemble the cart and stock slice](/build/your-shop/) and keep its working tests as you add external effects.

## Follow an event through an integration

For the practice project, consider an order confirmation. The workflow is an application design built from these integration primitives:

1. A command accepts the order and records an order-placed event.
2. An outbound handler recognises that event and describes an email request.
3. The integration runs the request outside the order's decision function.
4. Its callback produces a command recording either provider acceptance or failure.
5. A query makes the resulting status visible.

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

- [Coordinate work across entities](/connect/workflows/).
- [Call an external HTTP API](/connect/http-and-payments/).
- [Connect an external account](/connect/provider-accounts/) through explicit OAuth consent.
- [Send email](/connect/email/).
- [Attach files](/connect/files/) and [extract document or audio content](/connect/documents/).
- [Add AI assistance](/connect/ai/) and [constrain AI tools](/connect/ai-tools/).
- [Schedule periodic work](/connect/timers/).
- [Build a reusable integration](/connect/custom-integrations/).

**Try this:** describe what the practice application should show when an order exists but its email request times out. Include a state for an unknown provider outcome. Then identify a similar handoff in an application you would like to build, and explain which parts of the model carry over.

## Implementation and examples

- [core/service/Integration.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)
- [core/service/Service/Integration/Dispatcher.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Integration/Dispatcher.hs)
