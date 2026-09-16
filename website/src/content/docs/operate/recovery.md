---
title: Recover with evidence
description: Rehearse restores, distinguish projection failures from lost history, and reconcile external effects.
sidebar:
  order: 4
---

Restoring a database recovers one part of an application. External systems may still remember work that your restored copy does not. Recovery must account for both histories without repeating completed operations or silently forgetting unfinished ones. In an ecommerce example, this could mean reconciling a payment without charging the customer again.

Event history helps reconstruct application state. It cannot recreate a lost attachment from metadata or reverse an external payment by itself. Recovery includes the resources and organisations around the application.

## First identify what failed

| Observation | Investigate before changing data |
| --- | --- |
| No `/health` response | Process startup, configuration, database connection, port binding |
| `/health` works, `/ready` stays `503` | Query rebuild progress or failure |
| Ready, but one view is wrong | Projection logic, stored events, authorisation, running revision |
| File metadata exists, download fails | Blob volume and file lifecycle state |
| Provider succeeded, application outcome is uncertain | Provider transaction identity and local follow-up history |

Preserve useful logs and the failing revision identity. A blind database reset can destroy the evidence needed to distinguish these cases.

## Rehearse a restore into an isolated environment

Before people depend on retained data, rehearse with representative operations:

1. Create a small known history: an accepted change, a rejected change, and any attachment or integration outcome you support. An order provides a concrete example in the practice project.
2. Take a backup using the database and file-storage procedures for your hosting environment.
3. Restore to an isolated environment with production outbound effects disabled or replaced by controlled test endpoints.
4. Start the same application revision and wait for readiness.
5. Compare the reconstructed entities and views with the known history.
6. Verify attachment bytes, authorisation boundaries, and the handling of unfinished external work.
7. Record the recovery duration and the latest accepted operation included in the backup.

Include abandoned uploads in that rehearsal. The file-upload module has a cleanup worker, but normal application startup currently does not launch it. Do not assume a configured expiry or cleanup interval proves expired bytes have been removed.

The last two measurements answer business questions: how long could the application be unavailable, and how much recent work could need reconciliation? NeoHaskell does not choose those tolerances for you.

## Understand the rebuild boundary

A query subscriber can rebuild views from events and exposes readiness states. There are lower-level `rebuildAllAsync`, `rebuildFrom`, and checkpoint APIs; these are application/framework APIs, not a `neo rebuild` command.

Normal `Application` wiring currently creates `Subscriber.new`, without automatically connecting the checkpoint store. Persistent query rows do not alone prove checkpoint-based resume. Test the exact store, startup path, and projection logic you use; see [persistence](/operate/persistence/).

The Postgres query store creates its table if absent. That is not a general schema migration service. Existing installations with an incompatible table schema require an explicit migration plan.

## Recover external work by identity

Keep enough information to relate an application operation to its external counterpart. For a future payment workflow in the ecommerce practice project, that means connecting the order to the provider's operation. Define what happens when the provider accepts a request but the connection fails before the reply arrives.

This is a design requirement for your payment adapter, not a claim that NeoHaskell ships a complete payment/reconciliation system. Provider-supported idempotency and status lookup can inform the design; their exact guarantees must be checked against the chosen provider.

## Exercise: interrupt the handoff

For the payment design above, arrange a controlled test in which the external operation succeeds but local confirmation is interrupted. Restart and inspect the result. Apply the same method to whichever external side effect your own application performs.

<details>
<summary>What to verify</summary>

The customer should not receive a second charge merely because the first response was lost. The application should reach a correct final state or expose a clear pending state that can be reconciled. A timeout is evidence of an uncertain response, not proof that the provider did nothing.

</details>

Next, preserve those guarantees while [evolving the application](/operate/evolution/).
