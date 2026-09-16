---
title: "Testing behaviour"
description: Write checks for the decisions, reconstructed state, and HTTP behaviour of your own project.
sidebar:
  order: 7
---

“The code compiles” and “this request obeys the intended rule” are different claims. Confidence grows when you check each promise at the place it could fail. A fast decision test explains a refusal; an HTTP test checks that the running application actually exposes the promised behaviour.

You own the intended outcomes. Your agent can help implement checks, run them, and explain a failure. Keep examples you can recognise: two units accepted, zero refused, and one unit accepted at the boundary.

All files below belong to the `mug-shop` project you have been building. Keep its generated `tests/Spec.hs`; the CLI discovers tests and runs them with `neo test`.

Examples below show the relevant declarations and behaviour, with each destination named. Module headers and imports are omitted so you can focus on the idea. The [complete end of Build files](/examples/mug-shop-build.tar.gz) include that setup and the tests; add them to the same project when you want the runnable checkpoint.

## Match the check to the promise

| Question | Useful boundary |
| --- | --- |
| Does zero quantity get rejected? | The command decision. |
| Does replay preserve separate additions? | The entity update. |
| Does a request return the promised response and view? | The running HTTP application. |
| Does adding to a cart reserve stock? | The integration and both domains; added in [Connect](/connect/workflows/). |
| Does the provider accept the real request? | Its sandbox or a controlled live check. |

A fake provider reply gives a deterministic local test. It cannot establish that your account, credentials, or live request are accepted.

## Test a decision directly

In `tests/Decider/Cart/AddItemSpec.hs`, one test checks the complete accepted fact. Its body uses distinct, fixed cart and stock UUIDs:

```haskell
    let cart = CartEntity {cartId = cartIdFixture, ownerId = "owner", items = Array.empty}
    let request = AddItem {cartId = cartIdFixture, stockId = stockIdFixture, quantity = 2}
    result <- runDecision (decide request (Just cart) Auth.emptyContext)
    result |> shouldBe (AcceptCommand ExistingStream
      [ItemAdded (ItemAdded.Event {entityId = cartIdFixture, stockId = stockIdFixture, quantity = 2})])
```

The complete event files include equality support for these full-payload assertions. That is separate from the event marker's generated serialization and display instances; the concept lessons omit this testing detail.

The helper runs a `Decision` with a context that can generate IDs. There is no database or server. The accepted result is checked for its insertion type and complete event payload, so a wrong stock ID or quantity is observable.

The two fixed UUIDs are deliberately different, so swapping cart and stock IDs is observable. These tests exercise decision rules, not UUID generation or stream lookup. Supplying a state directly deliberately tests the decision in isolation. The application executor establishes whether an entity actually exists.

## Check reconstruction

In `tests/Decider/Cart/ReplaySpec.hs`, feed accepted facts through the same update function used by the application:

```haskell
    let created = CartCreated (CartCreated.Event {entityId = Uuid.nil, ownerId = "owner"})
    let added = ItemAdded (ItemAdded.Event {entityId = Uuid.nil, stockId = Uuid.nil, quantity = 2})
    let cart = initialState |> update created |> update added |> update added
    cart.items |> Array.length |> shouldBe 2
```

This check applies two separate additions. It protects our chosen meaning of an entry. You could also check the quantities stored in each entry; a later change to merge repeated products needs a new explicit policy and corresponding evidence.

## Test an internal command

You do not need to expose `ReserveStock` over HTTP to test its rule. In `tests/Decider/Stock/ReserveStockSpec.hs`, start with one unit and request two:

```haskell
    let stock = StockEntity {stockId = Uuid.nil, productId = Uuid.nil, available = 1, reserved = 0}
    result <- runDecision (decide (request 2) (Just stock) Auth.emptyContext)
    result |> shouldBe (RejectCommand "Insufficient stock available!")
```

Accepting the last unit and rejecting too many are distinct checks. These sequential tests do not establish how two simultaneous requests compete for the same final unit. Add an application-level concurrency scenario before making that stronger promise.

## Exercise the running application

Create `tests/scenarios/cart-flow.hurl`:

<details>
<summary>Complete HTTP scenario</summary>

```hurl
POST http://localhost:8080/commands/create-cart
Content-Type: application/json
[]

HTTP 200
[Captures]
cart_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200

HTTP 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 0
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].isEmpty" nth 0 == true

POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"11111111-1111-1111-1111-111111111111","quantity":2}

HTTP 200

POST http://localhost:8080/commands/add-item
Content-Type: application/json
{"cartId":"{{cart_id}}","stockId":"11111111-1111-1111-1111-111111111111","quantity":0}

HTTP 400
[Asserts]
jsonpath "$.reason" == "Quantity must be positive"

GET http://localhost:8080/queries/cart-summary
[Options]
retry: 10
retry-interval: 200

HTTP 200
[Asserts]
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].itemCount" nth 0 == 1
jsonpath "$.items[?(@.cartSummaryId == '{{cart_id}}')].isEmpty" nth 0 == false
```

</details>

This test creates its own cart, so it does not depend on yesterday's IDs. It checks that the refused zero request leaves the view at one accepted entry. Retries belong to the read: retrying an accepted addition could add it again.

Stop any `neo run` server, then execute from the project root:

```sh
neo test
```

The CLI runs your Haskell tests and starts the application for Hurl scenarios. A passing decision test with a failing HTTP scenario often points to registration, serialization, configuration, or integration rather than the rule alone. Inspect the failing boundary before changing business logic.

## Keep a regression that explains the mistake

Suppose your agent implements a six-unit **cart** limit by checking each request against six. Add four, then request four more. The second request should be refused under that policy. Run the failing check before correcting the implementation and retain it afterward.

Do not change an expected outcome merely to make the test pass. If the policy changes, describe that change explicitly, then update the evidence to match the new agreement.

## Exercise: a lost response

The client times out after requesting two mugs. Your agent proposes automatically resubmitting the command. What test would reveal the risk?

<details>
<summary>Suggested reasoning and checks</summary>

Arrange for the server to accept the first request while the response is lost. Submit the same request again and inspect history and state. The current command can accept a second addition. Decide what identifier or other policy should distinguish a retry from another intentional request. Test the first submission, a retry, and an intentionally different request. Disabling a button is helpful interface behaviour but does not establish server-side duplicate handling.

</details>

Next: [access control](/build/access-control/) applies the same evidence-based approach to permissions.
