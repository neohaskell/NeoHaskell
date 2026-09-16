---
title: Measure application performance
description: Investigate latency, replay, contention, and database budgets with representative application workloads.
sidebar:
  order: 7
---

An application that feels fast with a small dataset may behave differently as history and concurrent activity grow. Performance work begins by choosing which experience must remain acceptable: submitting a change, reading its result, or restarting after a deployment. A busy sale in the ecommerce practice project gives us a concrete workload to examine.

NeoHaskell's architecture gives these operations different work to do. Measure them separately before changing pool sizes or adding parallelism.

## Choose an observable budget

Write a target and a workload together. For the ecommerce example, “Stock availability appears within our agreed time while several customers reserve the last mugs” is testable. “The framework is fast” is not.

Measure at least:

- Command response time, including rejections.
- Time until the relevant query reflects an accepted event.
- Startup time to `/health` and separately to `/ready`.
- External-provider duration and pending-work age.
- Database connections, resource use, and failure rate during the test.

Keep the application revision, dataset size, machine size, and workload with the result. A cart-only measurement does not predict the latency of a later payment integration.

## Understand contention on one entity

Concurrent commands can compete to change the same entity. In the ecommerce example, two customers may try to reserve the final mug from the same stock entity. Optimistic concurrency detects conflicting writes, and the command executor can refetch state and retry its decision.

The current executor has a maximum of 10 conflict retries, using exponential backoff with jitter and a capped delay. That is a bounded conflict mechanism, not a guarantee that all requests succeed under unlimited contention. Keep business decisions deterministic and keep external effects out of a decision that may run again.

Ask whether one entity contains unnecessarily unrelated activity. Separating independent activity—for example, stock for different products—can reduce contention, but splitting one indivisible business invariant may make correctness harder. Preserve the rule you are trying to enforce.

## Budget database connections across the deployment

Postgres event stores, query stores, file state stores, and listeners contribute to connection demand. Default pool sizes are 6 for `PostgresEventStore` and 4 for `PostgresQueryObjectStoreConfig`; those numbers are not a universal capacity recommendation.

Inventory the actual pools your wiring creates, their limits, listener connections, and the number of processes. Include old and new processes alive during rollout, plus operator and maintenance headroom. Per-stream subscriptions may add demand beyond a simple fixed-pool total.

Increasing a pool can move the bottleneck into Postgres. Measure queueing, query duration, and failures before and after a change.

## Test replay as history grows

Use a disposable Postgres database configured through [persistence](/operate/persistence/). Create representative carts and accepted additions through your application's HTTP routes, record the expected cart and stock results, then stop the application. From the same `mug-shop` directory and against the same database, start it again:

```sh
LOG_LEVEL=info neo --ci run
```

In another terminal, check the two signals separately:

```sh
curl -i http://127.0.0.1:8080/health
curl -i http://127.0.0.1:8080/ready
```

Record when each becomes successful and compare the query results with your expected values. Repeat with a larger known history. Use controlled substitutes for any external effects, so a replay experiment cannot send real notifications or repeat provider actions. The initial in-memory store cannot measure recovery of history across restarts.

Keep reusable request scenarios under `tests/` and run `neo test` for correctness checks. Measure the application's request and readiness timings separately from CLI compilation time. Fast incorrect results are a failed test.

## Exercise: the final two mugs

In the ecommerce practice project, run simultaneous reservation attempts for the last two mugs under the policy you implemented. Decide the expected successful count before running the test.

<details>
<summary>What to compare</summary>

Check the count of accepted reservations, explicit rejections, final stock, and visible stock states. Then compare response times with a workload spread across many products. The difference helps isolate contention from general server capacity.

</details>

Use [observability](/operate/observability/) to turn a measured bottleneck into evidence your agent can act on.
