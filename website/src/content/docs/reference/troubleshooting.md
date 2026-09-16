---
title: Troubleshoot from the failed promise
description: Find the layer responsible for build, startup, model, query, and integration problems.
sidebar:
  order: 3
---

Start with what you expected to happen: a project builds, a process starts, an accepted change becomes visible, or a provider confirms an action. Then locate the first point where the evidence diverges. This gives you and your agent a smaller problem than “the application is broken.”

## Keep a useful diagnostic record

Record the command or request, revision, expected result, actual result, and the smallest safe reproduction. Include the complete relevant error, but remove secrets and private data. Run the commands here from your application directory containing `neo.json`; in this journey that is `mug-shop`.

| Symptom | First check | Next action |
| --- | --- | --- |
| `neo` cannot launch Nix | Nix is installed and available in the current shell | Reopen the shell after installation; follow [getting started](/getting-started/) |
| Build reports missing `neo.json` | Current directory | Run from the generated project root |
| A generated Cabal/Nix edit disappears | Reconciliation source | Express the supported configuration in `neo.json` |
| Build refuses a locked file | `.locked-files` and working-tree changes | Review the intentional domain change; see [evolution](/operate/evolution/) |
| Application fails before HTTP binds | First startup/configuration/database error | Fix that error before changing probe timings |
| `/health` succeeds, `/ready` returns `503` | Readiness body and replay logs | Distinguish rebuilding from failure |
| Accepted command is not visible | Query catch-up, identity, and correct revision | Follow the event-to-query path |
| Integration times out | Provider result and local correlation identity | Resolve uncertainty before resubmitting an external action |

## A new module or dependency is missing

Keep application modules beneath `src/`, with the path matching the module name: `Shop.Cart.Core` belongs in `src/Shop/Cart/Core.hs`. Check the import spelling and that you are building the intended project. Save the file and run:

```sh
neo --ci build
```

The CLI discovers source modules and regenerates managed project files. For an external package, edit `dependencies` in `neo.json`; do not add it only to the generated build files. A bare dependency name is looked up in the NeoPackages registry. Use the explicit `hackage:` key prefix when you intend a Hackage package. See the [dependency syntax](/reference/cli/#manage-project-dependencies).

If a lesson uses an API unavailable in your project's pinned framework revision, compare `neo --version` and `neo-version` in `neo.json` with the lesson's version context. Upgrade deliberately and rerun `neo build` and `neo test`; a local framework checkout is not part of the application workflow.

## A missing model is different from an invalid model

Run this in the project root:

```sh
neo validate --json
```

Exit `4` means the file is absent, `3` means JSON could not be parsed, and `2` means the parsed model violates its schema or references. Inspect the reported location before editing. Open `neo ide` to work with the visual model; use `neo inspect sync` only when you intend to update the saved model from source.

A valid graph does not prove the application implements the intended cancellation policy. Verify that with [behavioural tests](/build/testing/).

## Hurl reports connection refused

`neo test` starts the application when Hurl tests are present, and probes port 8080 before running them. Check whether the process crashed, whether another process owns the port, and whether you changed the application's port. In the current implementation the startup probe is fixed at 8080; changing Hurl URLs alone does not update that probe.

Stop any existing application process first. Run `neo run` locally and read the startup error, then stop that diagnostic run before retrying `neo test`. The test command starts its own process; a separately running server can hide which revision you tested. If the server responds but query-dependent tests race replay, check `/ready` as well: the CLI startup wait accepts any HTTP response.

A cold Nix startup can also exhaust the current 60-second wait before the
application begins serving. Let the diagnostic `neo run` finish starting, check
its response, stop it, and retry the test with the build environment warmed.
Treat an actual configuration or application error separately from that delay.

## Query catch-up fails

Use the [readiness and log guide](/operate/observability/). Preserve the query name, position, revision, and sanitised failure. Check database connectivity and event decoding before considering a data change.

Postgres event storage does not imply persisted query state. Persisted query state does not imply checkpoint resume in normal application wiring. [Persistence](/operate/persistence/) explains these boundaries. Do not delete event history to make a view appear empty and healthy.

## Database connections fail intermittently

Inventory pools and listeners across all running revisions. Check the configured pool sizes and the database's available capacity. Listener connections need a session-preserving direct endpoint; transaction-mode pooling cannot supply the required `LISTEN/NOTIFY` behaviour.

Check TLS configuration in each wired store. The `DB_SSL_MODE` field added in [persistence](/operate/persistence/) affects only the stores to which you pass it. It is not a universal switch for every Postgres client.

## Return a focused task to your agent

For example, a diagnostic task in the ecommerce practice project might be:

> “The command is accepted on revision A. Readiness becomes ready, but this user's query omits the cart. Find the query and its access policy, preserve that policy, and give me a test that distinguishes a projection error from an ownership error.”

This states the evidence and preserves the business constraint. Once the agent proposes a fix, repeat the original reproduction and a nearby rejection case.
