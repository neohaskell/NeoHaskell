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

First complete [persistence](/operate/persistence/); the initial in-memory store has no retained history to restore. Before people depend on retained data, rehearse with representative operations:

1. Create a small known history: an accepted change, a rejected change, and any attachment or integration outcome you support. In `mug-shop`, create a cart, add a positive quantity, and confirm that zero is refused.
2. Take a backup using the database and file-storage procedures for your hosting environment.
3. Restore to an isolated environment with production outbound effects disabled or replaced by controlled test endpoints.
4. Run `neo run` from the restored application project with its isolated storage configuration, and wait for `/ready`.
5. Compare the reconstructed entities and views with the known history.
6. Verify attachment bytes, authorisation boundaries, and the handling of unfinished external work.
7. Record the recovery duration and the latest accepted operation included in the backup.

Include abandoned uploads in that rehearsal. The file-upload module has a cleanup worker, but normal application startup currently does not launch it. Do not assume a configured expiry or cleanup interval proves expired bytes have been removed.

The last two measurements answer business questions: how long could the application be unavailable, and how much recent work could need reconciliation? NeoHaskell does not choose those tolerances for you.

## Rehearse with your local Postgres database

For the Docker Compose database from [persistence](/operate/persistence/), you can
practise a database-only restore without replacing the original. Stop `neo run`
after creating a known cart and noting its summary. From `mug-shop`, export the
local database to a protected backup file:

```sh
docker compose exec -T postgres pg_dump -U neohaskell -d neohaskell --format=custom > mug-shop.backup
```

Create a new database inside that same local Postgres service and restore into it:

```sh
docker compose exec -T postgres createdb -U neohaskell mug_shop_restore
docker compose exec -T postgres pg_restore -U neohaskell --dbname=mug_shop_restore < mug-shop.backup
```

`createdb` should fail if that restore database already exists. Choose a fresh
restore name for a later rehearsal rather than replacing data you have not
inspected. Check that both commands succeed before starting the application.

If you added real outbound integrations, first use controlled provider endpoints
or remove their registrations in an isolated application revision. Then select
the restored database through the configuration you added:

```sh
DB_NAME=mug_shop_restore DB_PASSWORD=neohaskell neo run
```

Keep any custom `DB_PORT` you used for the local database. In another terminal,
check `/ready` and fetch the original cart summary with the same identifier using
the requests from [HTTP and frontend](/build/http-and-frontend/). Compare it before
running tests or creating more data. The backup contains database data, including
potentially sensitive event history; store it with appropriate access protection.

This procedure restores the event database. Upload bytes and separate provider
credential stores require their own backups. A successful `pg_restore` is the
start of the application checks, not their replacement.

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
