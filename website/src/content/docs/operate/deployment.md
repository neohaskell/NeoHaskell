---
title: Deploy a revision you can identify
description: Build and supervise a NeoHaskell executable, configure probes, and verify the new revision.
sidebar:
  order: 2
---

A release is successful when the intended revision serves correct behaviour, not merely when a deployment command finishes. For an event-sourced service, keep traffic away from a fresh process while its read models catch up with history. The same rule applies to a booking view, a document queue, or the practice project’s cart summary.

NeoHaskell supplies an executable application and HTTP probe endpoints. Your hosting environment supplies the process supervisor, traffic routing, secrets, persistent storage, and restart policy. There is no `neo deploy` command in the current CLI.

## Prepare the same application for a host

Continue with your `mug-shop` project, including `neo.json`, `src/Shop/Cart/`,
`src/Shop/Stock/`, and its tests. Complete [persistence](/operate/persistence/)
before promising that accepted changes survive a restart. The initial
`SimpleEventStore` configuration is intentionally in memory.

One concrete path is a Linux host with Neo CLI, Nix, and Git installed for the
account that runs the application. Place a tested revision of **your application**
there, retain its framework pin and lock files, and build on that host or a
matching build host. Supply the staging `DB_*` environment values from
[persistence](/operate/persistence/) before running these checks against an
isolated staging database:

```sh
neo --ci build
neo --ci test
```

The test command creates real application state and starts its own server. Do not
run it against the production database, or while another process occupies port
8080. Configure the production database only after the staging checks pass.

From the application directory, this starts the server without interactive output:

```sh
neo --ci run
```

For a small hosted deployment, configure your process supervisor with that launch
command and the project directory as its working directory. Here is a **systemd
unit template** for an application installed at `/opt/mug-shop`. Replace the Neo
path with the output of `command -v neo` for the service account, and ensure its
`PATH` contains that account's Nix and Git executables:

```ini
[Unit]
Description=Mug shop application
After=network-online.target
Wants=network-online.target

[Service]
Type=simple
User=mug-shop
WorkingDirectory=/opt/mug-shop
EnvironmentFile=/etc/mug-shop.env
Environment=PATH=/home/mug-shop/.nix-profile/bin:/nix/var/nix/profiles/default/bin:/usr/local/bin:/usr/bin:/bin
ExecStart=/usr/local/bin/neo --ci run
Restart=on-failure
RestartSec=5
KillMode=control-group

[Install]
WantedBy=multi-user.target
```

The account needs access to the project and generated build directories. Create
`/etc/mug-shop.env` through the host's protected configuration mechanism using the
fields below. Save the adapted unit as `/etc/systemd/system/mug-shop.service`, then
use the host's administrative account:

```sh
sudo systemctl daemon-reload
sudo systemctl enable --now mug-shop
sudo journalctl -u mug-shop -f
```

This launch path still reconciles and builds through the CLI on restart; it needs
the toolchain and may need network access. It is not a prebuilt minimal runtime
image. Keep the revision and dependencies fixed, prebuild before admitting traffic,
and test stop/restart behavior. More specialised packaging is a deployment choice;
the CLI does not produce a ready-made application container or cloud environment.

## Supply the runtime resources

Before starting the revision, establish:

- The `DB_HOST`, `DB_PORT`, `DB_USER`, `DB_PASSWORD`, `DB_NAME`, `DB_POOL_SIZE`, `DB_SSL_MODE`, and `DB_SSL_ROOT_CERT` fields wired in [persistence](/operate/persistence/), and a reachable database.
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

For `mug-shop`, create a cart, add an allowed quantity, observe its cart and stock query results, then verify that zero is refused. Reuse the request shapes from [HTTP and frontend](/build/http-and-frontend/) against the new revision. Check any external outcome only if you have implemented that integration, using a controlled provider environment. The built-in probes establish narrower facts. This guide does not supply or certify an end-to-end cloud, payment, or AI deployment.

For a local instance on port 8080:

```sh
curl -i http://127.0.0.1:8080/health
curl -i http://127.0.0.1:8080/ready
```

## Account for current operational limits

Postgres clients use bounded pools. Listener connections for `LISTEN/NOTIFY` require a direct, session-preserving connection; do not route them through transaction-mode PgBouncer. Split pooled/direct endpoint support and Neon scale-to-zero handling are tracked in [issue #857](https://github.com/neohaskell/NeoHaskell/issues/857).

SIGTERM cancellation and checkpoint flushing during an active rebuild remain tracked in [issue #662](https://github.com/neohaskell/NeoHaskell/issues/662). Do not extend termination grace indefinitely to wait for replay. Practise interruption and restart in staging, and use the readiness contract to control traffic.

Next, learn [what to observe](/operate/observability/) and rehearse [recovery](/operate/recovery/).
