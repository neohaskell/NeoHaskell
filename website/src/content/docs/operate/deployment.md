---
title: Deploy a revision you can identify
description: Build and supervise a NeoHaskell executable, configure probes, and verify the new revision.
sidebar:
  order: 2
---

A release is successful when the intended revision serves correct behaviour, not merely when a deployment command finishes. For an event-sourced service, keep traffic away from a fresh process while its read models catch up with history. The same rule applies to a booking view, a document queue, or the practice project’s order summary.

NeoHaskell supplies an executable application and HTTP probe endpoints. Your hosting environment supplies the process supervisor, traffic routing, secrets, persistent storage, and restart policy. There is no `neo deploy` command in the current CLI.

## Produce the executable

Start from a tested application checkout containing `neo.json` and the generated Nix/Cabal files. In this example, the application package and executable are both named `mug-shop`; substitute the name declared by your project.

```sh
neo --ci build
neo --ci test
nix develop --command cabal list-bin exe:mug-shop
```

`neo build` reconciles project files and runs `cabal build all` inside `nix develop`. `cabal list-bin` prints the executable path; it does not package or deploy the application.

One concrete deployment path is a Nix-enabled Linux host built from the tested revision. Build there (or use a matching build host), resolve that path, and configure the host's process supervisor to run the executable directly with the intended working directory and environment. Retain its Nix runtime dependencies. Copying a binary built on macOS to Linux, or copying a Nix-linked binary without its store dependencies, is not a deployment strategy.

The generated flake also exposes the package outputs from `hixProject.flake`. Inspect the outputs with `nix flake show` before choosing a package for a Nix-native deployment. The template does not provide a ready-made application container, cloud environment, or universal default application package.

## Supply the runtime resources

Before starting the revision, establish:

- The actual Postgres settings wired by your application and a reachable database.
- A durable upload volume if you use the local blob store.
- Provider credentials and authentication configuration supplied through your deployment's secret mechanism.
- The HTTP port actually passed into the transport.
- A revision identifier recorded by your release system alongside logs and smoke-test results.

Inspect the wiring as well as the config declaration. A declared port field has no effect unless the application uses it to configure the server.

## Separate startup, liveness, and readiness

With standard application/web wiring:

| Request | Meaning | Typical response |
| --- | --- | --- |
| `GET /health` | The HTTP process is responding | `200` |
| `GET /ready` | Registered query projections have caught up | `200` when ready, `503` while rebuilding or failed |

Health does not prove that a payment provider works. Readiness does not certify every business workflow. Custom wiring can change or omit these routes; verify the actual revision.

For Kubernetes, this is an **illustrative probe fragment**, not a complete deployment manifest:

```yaml
startupProbe:
  httpGet:
    path: /health
    port: 8080
  periodSeconds: 5
  failureThreshold: 12
  timeoutSeconds: 2
livenessProbe:
  httpGet:
    path: /health
    port: 8080
  periodSeconds: 10
  failureThreshold: 3
  timeoutSeconds: 2
readinessProbe:
  httpGet:
    path: /ready
    port: 8080
  periodSeconds: 5
  failureThreshold: 3
  timeoutSeconds: 2
```

The example startup budget is `5 × 12 = 60` seconds. Size it for bounded process/database initialisation. Historical query replay runs after live subscription registration and must not delay the HTTP bind; readiness remains `503` until replay and overlapping live events drain. A startup probe keeps the steady-state liveness policy from repeatedly killing initialisation.

## Admit traffic deliberately

1. Start the revision without sending user traffic to it.
2. Observe `/health`, then wait for `/ready` to return `200`.
3. Run representative smoke tests against the **new revision identity**.
4. Admit traffic and monitor failures, latency, and business outcomes.

A shared ingress can still reach an old revision. Its successful response alone cannot prove the new revision is working.

A complete application smoke test should follow an implemented user journey: submit an allowed action, observe its query result, verify a rejection, and check the relevant external outcome against a controlled provider environment. The built-in probes establish narrower facts. This guide does not supply or certify an end-to-end cloud, payment, or AI deployment.

For a local instance on port 8080:

```sh
curl -i http://127.0.0.1:8080/health
curl -i http://127.0.0.1:8080/ready
```

## Account for current operational limits

Postgres clients use bounded pools. Listener connections for `LISTEN/NOTIFY` require a direct, session-preserving connection; do not route them through transaction-mode PgBouncer. Split pooled/direct endpoint support and Neon scale-to-zero handling are tracked in [issue #857](https://github.com/neohaskell/NeoHaskell/issues/857).

SIGTERM cancellation and checkpoint flushing during an active rebuild remain tracked in [issue #662](https://github.com/neohaskell/NeoHaskell/issues/662). Do not extend termination grace indefinitely to wait for replay. Practise interruption and restart in staging, and use the readiness contract to control traffic.

Next, learn [what to observe](/operate/observability/) and rehearse [recovery](/operate/recovery/).
