---
title: Measure application performance
description: Investigate latency, replay, contention, and database budgets with representative application workloads.
sidebar:
  order: 7
---

An application that feels fast with a small dataset may behave differently as history and concurrent activity grow. Performance work begins by choosing which experience must remain acceptable: submitting a change, reading its result, or restarting after a deployment. A busy sale in the ecommerce practice project gives us a concrete workload to examine.

NeoHaskell's architecture gives these operations different work to do. Measure them separately before changing pool sizes or adding parallelism.

## Choose an observable budget

Write a target and a workload together. For the ecommerce example, “Order status appears within our agreed time while several customers reserve the last mugs” is testable. “The framework is fast” is not.

Measure at least:

- Command response time, including rejections.
- Time until the relevant query reflects an accepted event.
- Startup time to `/health` and separately to `/ready`.
- External-provider duration and pending-work age.
- Database connections, resource use, and failure rate during the test.

Keep the application revision, dataset size, machine size, and workload with the result. A local counter benchmark does not predict checkout latency through a payment provider.

## Understand contention on one entity

Concurrent commands can compete to change the same entity. In the ecommerce example, two customers may try to reserve the final mug from the same stock entity. Optimistic concurrency detects conflicting writes, and the command executor can refetch state and retry its decision.

The current executor has a maximum of 10 conflict retries, using exponential backoff with jitter and a capped delay. That is a bounded conflict mechanism, not a guarantee that all requests succeed under unlimited contention. Keep business decisions deterministic and keep external effects out of a decision that may run again.

Ask whether one entity contains unnecessarily unrelated activity. Separating independent activity—for example, stock for different products—can reduce contention, but splitting one indivisible business invariant may make correctness harder. Preserve the rule you are trying to enforce.

## Budget database connections across the deployment

Postgres event stores, query stores, file state stores, and listeners contribute to connection demand. Default pool sizes are 6 for `PostgresEventStore` and 4 for `PostgresQueryObjectStoreConfig`; those numbers are not a universal capacity recommendation.

Inventory the actual pools your wiring creates, their limits, listener connections, and the number of processes. Include old and new processes alive during rollout, plus operator and maintenance headroom. Per-stream subscriptions may add demand beyond a simple fixed-pool total.

Increasing a pool can move the bottleneck into Postgres. Measure queueing, query duration, and failures before and after a change.

## Test replay as history grows

The repository's cold-start test uses histories of 1,000, 10,000, and 100,000 events to check that health binding is decoupled from replay, while readiness waits for catch-up. Its dataset is a controlled regression fixture, not a published production throughput promise.

That script intentionally truncates and drops test tables. Run it only through the documented contributor test environment with a disposable database; do not aim it at a database containing application data you need to keep. Read [the public test](https://github.com/neohaskell/NeoHaskell/blob/main/testbed/scripts/cold-start-readiness.sh) to understand what it proves.

For your own load test, use representative histories and controlled substitutes for external effects. Check final state as well as latency; for the ecommerce example, that includes stock and order totals. Fast incorrect results are a failed test.

## Exercise: the final two mugs

In the ecommerce practice project, run simultaneous attempts to buy the last two mugs under the reservation policy you implemented. Decide the expected successful count before running the test.

<details>
<summary>What to compare</summary>

Check the count of accepted reservations, explicit rejections, final stock, and visible order states. Then compare response times with a workload spread across many products. The difference helps isolate contention from general server capacity.

</details>

Use [observability](/operate/observability/) to turn a measured bottleneck into evidence your agent can act on.
