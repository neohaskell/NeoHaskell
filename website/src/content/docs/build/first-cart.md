---
title: Your first cart
description: Create a cart, inspect its read model, and catch a small misunderstanding with your agent.
sidebar:
  order: 1
---

Before the shop takes money, it needs a place to collect a customer's choices. Our first useful result is smaller still: create that empty cart and see it represented in the application.

You will make one request, keep its identifier, and find the corresponding summary. This establishes a habit for everything that follows: identify the business action, observe its result, and check what the application now says.

## Open the public example

The generated starter from [getting started](/getting-started/) demonstrates a Counter. This page uses the **Cart in the NeoHaskell repository testbed**, not a cart endpoint in the generated starter.

From a local checkout of the public NeoHaskell repository, start its development PostgreSQL and the testbed:

```sh
docker compose up -d postgres
./dev exec cabal run nhtestbed
```

The command stays running. PostgreSQL must be ready and port 8080 available; stop a starter server using that port first. The repository's `./dev exec` selects its pinned toolchain. The testbed uses the local database settings in its config; these are development defaults.

If you are only reading, follow the request and expected observations below. You can learn the model without running it yet.

## Ask for a cart

In another terminal, send this request to the running testbed:

```sh
curl -i http://localhost:8080/commands/create-cart \
  -H 'Content-Type: application/json' \
  --data '[]'
```

Expect HTTP 200 and a JSON object containing `entityId`, a newly generated UUID. Keep that value. The `[]` body is intentional: it is the current encoding of the example's fieldless `CreateCart` command.

Read the summaries:

```sh
curl http://localhost:8080/queries/cart-summary
```

The response is a page object containing `items`, `total`, `hasMore`, and `effectiveLimit`. Find the item whose `cartSummaryId` equals your returned `entityId`. It should have `itemCount` of zero and `isEmpty` of `true`.

The read model updates asynchronously. If the row is not visible immediately, repeat the read briefly. The testbed also creates demonstration carts periodically, so “the first row” and “the only row” are unreliable ways to identify yours. With many rows, use the [query filter](/build/queries/).

## A useful agent misunderstanding

**Jess:** “We have created the customer's order.”

**Agent:** “Yes; next I will mark it paid.”

**Jess:** “Show me the event that means the shop accepted an order.”

The source only records `CartCreated`. Nothing here establishes prices, payment, stock availability, or an agreement to fulfil. The correction is small and important: **we created a cart**. Order placement needs its own policy and implementation.

Use the [IDE graph](/getting-started/visual-ide/) to locate the command, its entity, and the summary. Launch a separate IDE for the testbed, because the IDE inspects the `src/` directory beneath its working directory. In another terminal, starting from the NeoHaskell repository root:

```sh
cd testbed
neo ide
```

Stop the earlier starter IDE first if it occupies port 2323, and ensure this terminal has the `neo` executable on its path. Confirm the browser's workspace points to `testbed`. If it has no `event-model.json`, create a new model, make a small canvas edit, and wait for autosave. Then run `neo inspect sync` from another terminal in that same `testbed` directory and reopen the workspace model with **Open**. See the [visual IDE walkthrough](/getting-started/visual-ide/) for the save and synchronization steps.

Ask the agent to open the relevant source. The graph gives you a route through the application; the definitions establish what happened. Keep the server and acceptance-test commands in their original repository-root terminals.

## Read the evidence

The public acceptance test checks the real HTTP boundary. This is an exact excerpt from `testbed/tests/commands/create-cart.hurl`:

```hurl
POST http://localhost:8080/commands/create-cart
[]

HTTP/1.1 200
Content-Type: application/json

[Asserts]
# Response must have entityId field
jsonpath "$.entityId" exists
```

Run that file against your running testbed, from the repository root:

```sh
./dev exec hurl --test testbed/tests/commands/create-cart.hurl
```

This proves the response contract when it passes. It does not prove your future checkout policy or a payment integration.

## Try a variation

Create two carts and identify both summaries. Then send malformed JSON, such as a body containing only `{`, and compare the outcome. What should remain unchanged after that rejected request?

<details>
<summary>Suggested reasoning and checks</summary>

Each successful creation should return its own UUID. A malformed body should produce a client error rather than an accepted command response. Follow the two known IDs rather than asserting the global cart count, because the demonstration timer can create other carts. The boundary case is an empty cart: it is a valid created entity with zero entries, not a missing entity.

</details>

Next: [commands and events](/build/commands-and-events/) explains how a request becomes an accepted fact.

Public sources: [CreateCart](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/Testbed/Cart/Commands/CreateCart.hs), [creation test](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/commands/create-cart.hurl), [testbed wiring](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/src/App.hs).
