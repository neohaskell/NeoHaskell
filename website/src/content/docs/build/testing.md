---
title: Testing behaviour
description: Build confidence in an agent's changes with examples, boundary cases, and real application checks.
sidebar:
  order: 7
---

“The code compiles” and “this request obeys the intended rule” are different claims. Your confidence grows when each important claim has a check at the place where it could fail. NeoHaskell applications need evidence about decisions, stored state, interfaces, and external effects.

You own the intended behaviour; your agent can implement and run the checks. Ask it to show what a passing result establishes, what a failing result looks like, and what the test never exercises.

The examples below use the Counter starter and the Cart/Stock practice project. For instance, “a customer cannot reserve more mugs than remain” is a specific rule whose evidence should go beyond successful compilation.

## Match the check to the promise

| Question | Useful test boundary |
| --- | --- |
| Does zero quantity get rejected? | The command's decision function. |
| Does applying accepted history produce the right state? | Entity replay. |
| Does an accepted command persist the intended event? | Command executor plus event store. |
| Does a browser request get the documented response? | HTTP acceptance test. |
| Does an addition eventually reserve stock? | Cart, integration dispatcher, Stock, and their views. |
| Does a real provider accept our request? | Provider sandbox or controlled live integration test. |

A fake provider response can make a fast test deterministic. It cannot establish that your provider credentials, account settings, and live payload are accepted.

## Read a small decision test

The starter has a Counter example you can run before building Cart. This exact excerpt tests the zero boundary in `IncrementCounterSpec`:

```haskell
  it "rejects a non-positive amount" \_ctx -> do
    let existing = CounterEntity {counterId = Uuid.nil, label = "downloads", value = 0}
    let cmd = IncrementCounter {entityId = Uuid.nil, amount = 0}
    result <- runTestDecision (decide cmd (Just existing) Auth.emptyContext)
    case result of
      RejectCommand msg ->
        msg |> shouldBe "Amount must be positive"
      AcceptCommand _ _ ->
        fail "expected a rejection for a non-positive amount"
```

The helper runs the `Decision` using a `DecisionContext`. No HTTP server or database is involved. Adapt that pattern to an `AddItem` decision test, keeping the Cart rule and expected message explicit.

For an accepted command, inspect the event's payload as well as its count. A test that only proves “one event happened” can miss the wrong quantity, product, or entity ID. The starter's existing example demonstrates the test structure; your application's acceptance criteria should inspect the values its rules depend on.

## Exercise the full path

With the repository testbed and PostgreSQL running, use:

```sh
./dev exec hurl --test testbed/tests/scenarios/stock-reservation.hurl
```

The file creates stock, creates a cart, adds quantities, and checks the stock read model. This exact excerpt shows a bounded wait for a projection:

```hurl
GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200

HTTP/1.1 200
[Asserts]
# Stock should now show 95 available, 5 reserved
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].available" nth 0 == 95
jsonpath "$.items[?(@.stockLevelId == '{{stock_id}}')].reserved" nth 0 == 5
```

The capture `stock_id` comes from an earlier request in that file. Copying this fragment alone is not a complete test.

Repository-wide acceptance checks are available through `./dev testbed`. For a generated application, use its own `neo test` workflow and test layout; the monorepo's `./dev` commands belong to framework development. See [getting started](/getting-started/) for the application workflow.

## Turn a misunderstanding into lasting evidence

Return to the “six mugs per cart” exercise. First write a scenario that adds four and then attempts another four. If the agent's proposed implementation checks only each request, that scenario should fail. Keep it as a regression test after correcting the rule.

Useful evidence includes the failure before the correction and the pass afterwards. Changing an expected result to match a bug erases that evidence.

## Exercise: accept a reservation change

For the practice project, your agent says it has fixed over-reservation. Ask it for three checks you can understand without reading its implementation.

<details>
<summary>Suggested reasoning and checks</summary>

Reserve two from three: one remains. Reserve four from three: reject and keep stock unchanged. Reserve exactly three: zero remains. Then add the concurrent last-unit case because sequential tests cannot establish competing-request behaviour. A repeated request is another distinct case: decide whether it should reserve again or be recognised as the same operation, and test that explicit policy.

</details>

Next: [access control](/build/access-control/) applies the same evidence-based approach to permissions.

Public sources: [starter decision tests](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/tests/Decider/Counter/IncrementCounterSpec.hs), [replay property](https://github.com/neohaskell/NeoHaskell/blob/main/neo/starter/tests/Property/CounterReplaySpec.hs), [reservation acceptance test](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/tests/scenarios/stock-reservation.hurl).
