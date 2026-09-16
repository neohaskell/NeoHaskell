---
title: Run and evolve an application
description: Move from a working demonstration to an application you can operate confidently.
sidebar:
  order: 0
---

An application should preserve accepted work across restarts and serve the intended behaviour after a release. Operating it means turning those expectations into checks you can repeat. The right checks depend on its purpose, whether it manages bookings, documents, orders, or another kind of work.

NeoHaskell provides event storage, read models, health endpoints, logging, and tests. You still choose the hosting environment, protect the data, and decide what acceptable service looks like. A successful build establishes a different kind of confidence from a successful restore rehearsal.

## Make five promises

For an event-sourced service, begin with these promises and adapt them to your application before selecting infrastructure:

| Promise | Evidence you will need |
| --- | --- |
| Accepted changes survive restart | A durable event store and a restart test |
| Users see accurate information | Readiness checks and representative query results |
| A release serves the intended revision | Build identity and a smoke test against that revision |
| Failures can be diagnosed | Useful logs, identifiers, and a recovery procedure |
| Changes preserve existing business history | Old-event fixtures and compatibility tests |

The recurring ecommerce practice project makes these concerns concrete. The public starter runs a counter; the reference testbed demonstrates carts, stock, queries, and integrations. These are examples of reusable framework capabilities, not requirements for your own domain or a finished commerce platform. Apply the checks to the behaviour you implement.

## Follow the operational journey

1. [Choose what survives restart](/operate/persistence/): events, read models, uploaded files, and provider credentials have different storage needs.
2. [Deploy a revision](/operate/deployment/): build an executable, supply configuration, and admit traffic only when ready.
3. [Observe the running application](/operate/observability/): distinguish a responding process from completed business work.
4. [Practise recovery](/operate/recovery/): restore into an isolated environment and reconcile external effects.
5. [Evolve safely](/operate/evolution/): preserve the meaning of historical events as requirements change.
6. Review [security](/operate/security/) and [performance](/operate/performance/) before increasing exposure or traffic.

These are connected subjects, not a certification checklist. A single-host pilot and a widely used public service have different availability needs; both need an honest account of their assumptions.

## Give your agent an outcome to demonstrate

For the ecommerce practice project:

> “Show me that an accepted order survives a process restart. Identify where its events live, show the rebuilt query, and explain what would happen if the upload disk disappeared.”

Use the [visual IDE](/getting-started/visual-ide/) to locate the relevant entity and its consumers—in this example, the order model. The graph helps explain the application; deployment logs and tests establish what the running revision actually did.

Once you can operate and change your own application, [contribution](/operate/contributing/) offers a separate path into improving NeoHaskell.
