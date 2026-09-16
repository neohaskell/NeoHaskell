---
title: "Your first working slice"
description: Give your own project one request, one recorded fact, and a useful answer.
sidebar:
  order: 1
---

The smallest useful application slice connects a person's request to something they can observe. Here you will build that slice in **your own `mug-shop` project**: accept “create a cart,” remember that it happened, and show an empty-cart summary.

The cart is our practice example. The same shape can start a booking or a document review. You decide what the action means; NeoHaskell connects the request, history, state, and view.

We will assemble the slice one responsibility at a time. Each section explains the idea before showing its implementation. Read the focused pieces with your agent and discuss what each one promises.

Examples below show the relevant declarations and behaviour, with each destination named. Module headers and imports are omitted so you can focus on the idea. The [complete first slice files](/examples/mug-shop-first-cart.tar.gz) include that setup and the tests; add them to the same project when you want the runnable checkpoint.

## Start in your own project

Complete [getting started](/getting-started/) first. All paths below are relative to the `mug-shop` directory created by `neo new mug-shop`. Keep its `neo.json`, launcher, and generated build setup. `neo` supplies the project's compiler configuration.

We are replacing the scaffold's Counter example with our Cart domain. Remove these supplied example files from your newly generated project:

```sh
rm -r src/Starter tests/Decider/Counter
rm tests/Property/CounterReplaySpec.hs
rm tests/scenarios/counter-flow.hurl tests/integration/smoke.hurl
mkdir -p src/Shop/Cart/Commands src/Shop/Cart/Events src/Shop/Cart/Queries
mkdir -p tests/scenarios tests/Decider/Cart
```

Keep `tests/Spec.hs`; it will discover your test modules. Extract the first-slice download into this project root, retaining its `src/` and `tests/` paths; it replaces `src/App.hs` and adds the Cart files. You can instead assemble those files with your agent as you work through the concepts below. You do not need to maintain a separate module list: `neo build` discovers your source files.

## 1. Name the fact you want to remember

The fact is **a cart was created**. It needs the cart's identifier and an owner identifier. In `src/Shop/Cart/Events/CartCreated.hs`, the payload is small:

```haskell
data Event = Event
  { entityId :: Uuid
  , ownerId :: Text
  }
```

This marker tells NeoHaskell to treat the declaration as an event and supply its routine supporting code:

```haskell
deriveEvent ''Event
```

We keep this payload in its own file. A separate `CartEvent` type lists the facts this domain understands; today its only possibility is `CartCreated`.

Your meaningful contribution is the name and information in the fact. Later, another event gets another focused file rather than turning this one into a catalogue of unrelated concerns.

## 2. Turn the fact into current state

After creation, the cart has an identifier and an owner. The entity's update function applies the accepted fact:

```haskell
  CartCreated created ->
    CartEntity {cartId = created.entityId, ownerId = created.ownerId}
```

This belongs in `Entity.hs`. `Core.hs` is only a small convenience module that re-exports the domain's entity and event types; it contains no decisions or state-update logic.

After `initialState` and `update`, the entity marker connects this state to its events:

```haskell
deriveEntity ''CartEntity ''CartEvent
```

The complete file imports `getEventEntityId` from the event module before this
marker. You supply those three pieces of behaviour; NeoHaskell generates the
routine entity, JSON, default-state, and event-routing instances. Like the event
and command helpers, `deriveEntity` comes from `Core`.

The initial nil identifier is a starting value for reconstruction. It is not evidence that a real cart exists. A real cart begins with an accepted creation event.

## 3. Accept the person's request

`CreateCart` is a request with no input fields. The application generates its identity. In `src/Shop/Cart/Commands/CreateCart.hs`, its decision has two outcomes:

```haskell
decide _ existing context = case existing of
  Just _ -> Decider.reject "Cart already exists!"
  Nothing -> createCart context
```

After the decision and its entity/transport declarations, the command marker connects this behaviour to the framework:

```haskell
deriveCommand ''CreateCart
```

The creation helper generates the cart ID and records `CartCreated`. For this local exercise it generates an anonymous owner when no signed-in identity exists.

An anonymous owner ID is a label in history. It does not establish a browser session or prove that a future caller owns the cart. We will make that policy explicit in [access control](/build/access-control/).

## 4. Answer the screen's question

A screen needs a useful answer, not the whole event history. Our `CartSummary` answers “which cart is this, and is it empty?” Every cart is empty at this first milestone, so the projection in `src/Shop/Cart/Queries/CartSummary.hs` sets `count` to zero.

```haskell
      , itemCount = count
      , isEmpty = count == 0
```

The complete query also supplies the cart ID and owner. Its public access policy is deliberate for this local practice.

The query marker connects this view to the cart state it reads:

```haskell
deriveQuery ''CartSummary [''CartEntity]
```

The complete file puts the declarations in the order NeoHaskell needs. The
[queries lesson](/build/queries/) explains how to grow the view when the screen
needs more information.

## 5. Make the pieces reachable

A service registers a domain's commands. The application registers that service and its views:

```haskell
  |> Application.withService Cart.service
  |> Application.withQuery @CartSummary
```

These steps belong in `src/App.hs`; `src/Shop/Cart/Service.hs` registers `CreateCart`. Both are included in the first-slice download.

The store initially uses `persistent = False`. Restarting clears this exercise's history, so you can reach the first result without setting up a database. [Configuration](/build/configuration/) and [persistence](/operate/persistence/) later make storage an explicit choice.

## Build it and make a request

From your project root:

```sh
neo build
neo run
```

In another terminal, request a cart:

```sh
curl -i http://localhost:8080/commands/create-cart \
  -H 'Content-Type: application/json' \
  --data '[]'
```

Expect HTTP 200 and a JSON object containing `entityId`. Keep that UUID. The `[]` body is the encoding of this fieldless command.

Read the view:

```sh
curl http://localhost:8080/queries/cart-summary
```

Find the row whose `cartSummaryId` matches your `entityId`. It should have `itemCount: 0` and `isEmpty: true`. The response is a page containing `items`, `total`, `hasMore`, and `effectiveLimit`.

The read model updates asynchronously. Repeat the read briefly if your row has not appeared. Resubmitting the creation command would create another cart, not refresh the original one.

## Keep evidence you can run again

The first-slice download includes `tests/scenarios/create-cart.hurl`. It creates its own cart and waits for its empty summary. Its final assertions check the visible outcome:

```hurl
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 0
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].isEmpty" nth 0 == true
```

Stop `neo run` with Ctrl-C, then run `neo test`. The CLI starts a server for the complete HTTP scenario.

The download also includes `tests/Decider/Cart/CreateCartSpec.hs`. It checks the accepted event and refusal of an existing cart without running HTTP. The [testing lesson](/build/testing/) explains this style when you are ready for the next layer.

You have created a cart, not an accepted order. No price, payment, or fulfilment promise appears in the model. Ask your agent to point to the fact behind each proposed claim.

## Try a variation

Create two carts and identify both summaries. Then send malformed JSON, such as a body containing only `{`. What should remain unchanged after that refused request?

<details>
<summary>Suggested reasoning and checks</summary>

Two successful requests should return different IDs and acquire separate empty summaries. Malformed JSON should produce a client error without an accepted creation response. An empty cart is a valid created entity, distinct from a missing cart. Restarting this nonpersistent application starts a fresh exercise.

</details>

Next: [explore your cart in the visual IDE](/getting-started/visual-ide/), running `neo ide` from this same project. Then [add a new command](/build/commands-and-events/).
