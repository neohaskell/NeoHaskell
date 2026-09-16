---
title: Build a reusable integration
description: Package external work behind a small request and an explicit outcome.
sidebar:
  order: 10
---

When an application needs an external API or local tool that NeoHaskell does not wrap, you can package that work as a reusable integration. Callers describe a request in a small record; protocol details, credentials, and response parsing stay in one implementation.

This is a deeper branch of the journey. You are taking responsibility for networking or subprocess behaviour as well as the application's rules. Begin with [generic HTTP](/connect/http-and-payments/) if that already fits the provider.

## Give the adapter a home in mug-shop

Keep working from your `mug-shop` project. Start with a module such as
`src/Shop/Integrations/Parcel.hs`; split it into request, response, and internal
modules when those responsibilities need separate homes. The resulting module
imports remain `Shop.Integrations.Parcel` and its children. Keep provider-specific
dependencies in your project’s `neo.json`, following [integration setup](/connect/#prepare-your-project).

A useful structure separates:

- A facade module for the API callers import.
- A request type containing input, configuration, and outcome callbacks.
- A response type containing the useful provider result.
- An internal module that implements or composes execution.

For an imaginary parcel-label provider, the application request might include a shipment reference and package details. This is a design example, not a supplied shipping API. Decide which exact provider status establishes label creation and what to do if a reply is lost.

Keep both callbacks returning a single command type declared with `InternalTransport`. Let the caller capture its own workflow identifier in those callbacks. Avoid coupling the reusable integration to a particular application entity.

## Understand the execution contract

`Integration.ToAction` converts a request into an `Action`. Its essential method is:

```haskell
class ToAction config where
  toAction :: config -> Action
```

An action receives `ActionContext` and returns a `Task IntegrationError (Maybe CommandPayload)`. Successful work can use `Integration.emitCommand`; work with no follow-up command can use `Integration.noCommand`.

You already used `Integration.Command.Emit` in [cart-to-stock coordination](/connect/workflows/). It performs no external operation: it emits the configured command. Your parcel adapter adds protocol work before choosing its outcome command.

For HTTP providers, compose `Integration.Http.Request` rather than duplicating request machinery. OpenRouter's `toHttpRequest` is a concrete example: it builds the endpoint, body, headers, authentication, callbacks, and timeout, then delegates execution. Keep the [current retry behaviour](/connect/http-and-payments/#understand-the-current-retry-boundary) in the provider's compatibility tests.

## Choose errors that help the caller recover

The runtime error vocabulary includes `NetworkError`, `AuthenticationError`, `ValidationError`, `RateLimited`, `PermanentFailure`, and `UnexpectedError`.

Decide which failures become an outcome command and which fail preparation before any request can run. Explain that boundary in your integration's docs. A caller waiting for a callback needs an operational way to detect failures that bypass that callback.

Do not return arbitrary provider bodies as error strings. They can contain customer data or credentials. Preserve a safe explanation and a correlation identifier where the provider supports one.

## Keep long-lived resources separate from durable state

If an integration needs an expensive per-entity resource, `Integration.Lifecycle.OutboundConfig state` offers:

```haskell
initialize :: StreamId -> Task Text state
processEvent :: state -> Event Json.Value -> Task Text (Array Integration.CommandPayload)
cleanup :: state -> Task Text Unit
```

These are **field signatures**, excerpted from the lifecycle type. Workers initialise resources, process events, and clean up when stopped or reaped. A later event can create a fresh worker, so this state is not durable workflow history.

A worker can hold a `ConcurrentVar`, connection handle, or other temporary resource. Its values can reset when the worker is recreated. Keep unfinished work in durable application state rather than only in that variable. In `mug-shop`, “label purchase still pending” is one such state.

After you implement `shipmentLifecycle`, make it and `CartEntity` available in
`src/App.hs`. This **registration fragment** connects it to cart events:

```haskell
    |> Application.withOutboundLifecycle @() @CartEntity (\_ -> shipmentLifecycle)
```

`shipmentLifecycle` must be your `OutboundConfig state` value with all three
functions above; it is not a supplied parcel implementation. Use this layer only
when its resource lifecycle is useful. Most provider requests can remain stateless.

## Receive external work

`Integration.inbound` wraps an `InboundConfig` whose `run` function receives an emit callback. The worker translates incoming information into commands. Application startup launches registered inbound workers.

A webhook integration still needs a real listener, provider authentication/signature verification, input limits, and an acknowledgement strategy. The abstraction is not an automatically generated webhook server. Similarly, a queue consumer needs a deliberate policy for acknowledgement, redelivery, and durable progress.

## Prove the adapter before shipping it

Use `Integration.getActions` to inspect the actions selected by a handler and `Integration.runAction` with a controlled `ActionContext` to exercise execution. Pure request-building functions are especially useful for testing protocol mappings without sending traffic.

Put adapter tests alongside the other tests in your `tests/` directory. Run
`neo build` after adding the module, then `neo test` for its request mapping and
error cases. Check valid input, provider refusal, malformed success data, missing
credentials, timeout, duplicate invocation, and a lost reply after remote success.
Count requests against a controlled server. Finally run `neo run` with sandbox
credentials and verify the provider contract through your application’s commands
and outcome query.

**Exercise:** package a shipment-status lookup against a controlled test provider for the practice project. Have another reader configure it using only the public request API. If they must understand internal HTTP parsing to choose ordinary options, revise the API and documentation together.

Return to [running the application](/operate/deployment/) for runtime dependencies, configuration, and recovery planning.

<details>
<summary>Framework source notes</summary>

- [core/service/Integration.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration.hs)
- [core/service/Integration/Command.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Command.hs)
- [core/service/Integration/Lifecycle.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Integration/Lifecycle.hs)
- [integrations/Integration/OpenRouter/Internal.hs](https://github.com/neohaskell/NeoHaskell/blob/main/integrations/Integration/OpenRouter/Internal.hs)
- [testbed/src/Testbed/Cart/Integrations/EventCounter.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Integrations/EventCounter.hs)
- [testbed/src/App.hs](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs)
- [core/service/Service/Application/Integrations.hs](https://github.com/neohaskell/NeoHaskell/blob/main/core/service/Service/Application/Integrations.hs)

</details>
