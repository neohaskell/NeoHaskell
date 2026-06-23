# ADR-0060: Explicit Postgres Connection-Pool Budget for Flexible Server B1ms

> Issue: [#680 — WI-1: Pin `hasql-pool` and set explicit pool sizes on all three Postgres pools](https://github.com/neohaskell/NeoHaskell/issues/680)

## Status

Proposed

## Context

### Current State

nhcore opens Postgres connections from **three independent
`hasql-pool` pools** plus a set of **unpooled** connections:

| Source | Where | Pooled? | Default size today |
|--------|-------|---------|--------------------|
| EventStore | `toConnectionPoolSettings` (`core/service/Service/EventStore/Postgres/Internal.hs:79-86`) | yes | library default (`3`) |
| QueryObjectStore | `acquirePool` (`core/service/Service/QueryObjectStore/Postgres.hs:102-107`) | yes | library default (`3`) |
| FileUpload | `createPool` (`core/service/Service/FileUpload/FileStateStore/Postgres.hs:174-180`) | yes | library default (`3`) |
| Listener pair | `connectTo` (`core/service/Service/EventStore/Postgres/Notifications.hs:29-65`) | no | 2 persistent |
| Init-listen | subscription initialisation | no | 1 transient |
| Per-stream subscription | `subscribeToStreamEventsImpl` (`Internal.hs:771-790`) | no | 1 per active stream |

None of the three pools call `HasqlPoolConfig.size`, so each inherits
`hasql-pool`'s library default — **3 connections per pool** in
`hasql-pool 1.3.0.4` (the version currently resolved by the nix
package set). Two facts make this fragile:

1. **The size is implicit.** The aggregate pool budget today is
   `3 + 3 + 3 = 9` pooled connections — but only because the library
   default happens to be `3`. Nothing in nhcore says so.
2. **`hasql-pool` is unpinned** in `core/nhcore.cabal:56` (it appears
   with no version bound). A dependency bump that changed the default
   size would silently move the connection budget with no code change
   and no review signal.

The deploy target for the per-client baseline (epic #679) is **Azure
Database for PostgreSQL — Flexible Server, Burstable `B1ms`**.
`B1ms` allows **50 `max_connections` total, of which ≈15 are reserved
(superuser + replication + maintenance), leaving ≈35 usable**. The
aggregate worst-case connection demand of an nhcore app must therefore
be **intentional and bounded**, and shown to fit comfortably under 35
— not left to a transitive default.

### Use Cases

- **First production client on `B1ms`** — a single nhcore app instance
  must run EventStore + QueryObjectStore + FileUpload + the
  LISTEN/NOTIFY listener + active stream subscriptions without
  exhausting the 35-usable ceiling, with headroom for an operator
  `psql` session and a maintenance task.
- **Dependency bump safety** — a routine `hasql-pool` upgrade must not
  silently change how many connections each pool opens. The budget is
  reviewed when it changes, not discovered in production.
- **Tier upgrade (B1ms → General Purpose)** — a client whose load
  outgrows `B1ms` should be able to raise the connection budget by
  changing a single, visible number per pool, without hunting through
  three call sites or guessing the library default.

### Design Goals

1. **Explicit per-pool size** — every pool calls
   `HasqlPoolConfig.size` with a named constant, so the budget is
   readable at the call site and reviewed when it changes.
2. **Pinned `hasql-pool`** — a version bound in `core/nhcore.cabal` so
   the default-size behaviour cannot drift under a dependency bump.
3. **Documented aggregate budget that fits `B1ms`** — the worst-case
   sum (3 pools + listener pair + init-listen + per-stream
   subscriptions + admin headroom) is written down and shown to be
   `< 35`.
4. **Tier-tunable without a rebuild** — the per-pool sizes are
   configurable, defaulted fields on the config records, so moving to a
   larger tier is changing a field value (or an env var, if the app
   threads one through), not editing library source. The safe `B1ms`
   defaults mean an unset field is still correct out of the box.
5. **No behaviour change for Jess** — local/dev apps and the in-memory
   default path are unaffected; the new fields are defaulted, so
   hello-world never has to mention them. This is invisible to anyone
   not deploying to a constrained Postgres tier.

### GitHub Issue

- [#680: WI-1 — Pin `hasql-pool` and set explicit pool sizes on all three Postgres pools](https://github.com/neohaskell/NeoHaskell/issues/680)
- Parent epic: [#679 — Deploy-readiness on Azure Database for PostgreSQL Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)

## Decision drivers

- **The 35-usable `B1ms` ceiling is a hard deploy blocker.** Exceeding
  it surfaces as `FATAL: remaining connection slots are reserved` —
  a production outage, not a degraded mode. The budget must have margin.
- **Implicit defaults are unreviewable.** A connection budget that
  exists only because a transitive library defaults to `3` cannot be
  reasoned about during a deploy review. Making each size explicit is
  the load-bearing change; the exact numbers are secondary and tunable.
- **The pin and the sizes are a pair.** Pinning without explicit sizes
  still leaves the budget defined by the (now-frozen) library default
  — readable only by reading `hasql-pool` source. Setting sizes without
  pinning leaves the *other* pool knobs (`agingTimeout`,
  `idlenessTimeout` defaults, future settings) free to drift. Both are
  needed for the budget to be both explicit and stable.
- **Keep the surface Jess-invisible.** This is deploy-tier hardening.
  It must not add a config knob Jess has to understand to run
  hello-world. The default constants must be safe for `B1ms` out of the
  box; a tier upgrade is an advanced, documented edit.
- **Follow the WI-2 trajectory.** Epic #679 WI-2 (#681) will unify the
  three connection-settings builders. The explicit size set here must
  be expressed so WI-2 can lift it into the shared builder without
  re-litigating the numbers (see Consequences → Negative).

## Considered options

### Option 1 — Configurable, defaulted `size` field per config + cabal version pin (chosen)

Add `HasqlPoolConfig.size <n>` to all three pool configs, sourced from a
configurable, defaulted `poolSize :: Int` field on each Postgres config
record (defaulted via a `Default` instance), and pin `hasql-pool` with a
`>= 1.3 && < 1.4` bound in `core/nhcore.cabal`. Document the aggregate
budget in this ADR and (per WI-6) in the deployment guide.

Per-pool split (defaults sized for `B1ms`):

| Pool | Field | Default | Why |
|------|-------|---------|-----|
| EventStore | `PostgresEventStore.poolSize` | `6` | Busiest pool: command appends, entity fetches, query catch-up reads all route through it. |
| QueryObjectStore | `PostgresQueryObjectStoreConfig.poolSize` | `4` | Read-model writes during rebuild + read-model reads; bursty during cold-start replay, quiet at steady state. |
| FileUpload | shares `PostgresEventStore.poolSize` | `6` | FileUpload is configured from the same `PostgresEventStore` record, so it shares the EventStore pool size rather than owning an independent knob. Metadata writes only (bytes are out-of-band). |

Aggregate worst-case demand on `B1ms` (≈35 usable), with all sizes at
their defaults:

```text
  EventStore pool        6
  QueryObjectStore pool  4
  FileUpload pool        6   (shares PostgresEventStore.poolSize)
  ----------------------- 
  pooled subtotal       16

  Listener pair          2   (persistent, unpooled)
  Init-listen            1   (transient, unpooled)
  ----------------------- 
  fixed unpooled         3

  Per-stream subs        N   (1 each; bounded by deployment, see WI-4 #683)
  ----------------------- 
  steady-state total    19 + N
```

With `N = 0` the app uses **19** of 35. That leaves **16** for active
stream subscriptions plus an operator `psql` session and a maintenance
task. A deployment can safely run a healthy number of concurrent stream
subscriptions before approaching the ceiling, and WI-4 (#683) will both
release per-stream connections on unsubscribe and document this cap. If
FileUpload's shared size proves too generous (it is often unused), WI-2
(#681) can give it an independent, smaller size.

### Option 2 — Pin `hasql-pool` only; leave sizes at the library default

Add the cabal bound but call no `size`. The budget becomes "whatever
`hasql-pool 1.3.x` defaults to" — frozen at `3` per pool by the pin.

- Rejected: the budget is stable but still **unreadable at the call
  site** — a deploy reviewer must read `hasql-pool` source to learn it
  is `9` total. Fails Design Goal 1. Also leaves no obvious knob for a
  tier upgrade (Design Goal 4).

### Option 3 — Set explicit sizes; do not pin `hasql-pool`

Call `size` everywhere but leave the cabal dependency unbounded.

- Rejected: the *size* is now explicit, but every *other* pool default
  (and any future `hasql-pool` setting nhcore relies on) is still free
  to drift on a bump. The pin is cheap insurance and the issue's
  acceptance criteria require it. Fails Design Goal 2.

### Option 4 — Module-level size constants (previously chosen, superseded)

Set each `HasqlPoolConfig.size` from a top-level `Int` constant in its
module (`eventStorePoolSize = 6`, etc.), with no config field.

- Rejected: editing a constant in library source is the wrong shape for
  a framework. A per-client deployment cannot raise its pool budget
  without forking and rebuilding nhcore, and the size is invisible to
  the application's own config surface. A configurable, defaulted field
  (Option 1) gives the same safe `B1ms` default at zero cost to Jess
  while letting an operator tune the budget per deployment. (This was
  the initial form of this PR; this ADR revision supersedes it.)

### Option 5 — One shared pool for all three subsystems

Collapse EventStore / QueryObjectStore / FileUpload onto a single
`hasql-pool`.

- Rejected: out of scope and a larger behavioural change than WI-1
  warrants. The three subsystems have different lifecycles and the
  observation handler / keepalive drift between them is WI-2's concern
  (#681), not a pool-merge. Merging also couples FileUpload (often
  unused) contention to the EventStore hot path.

| Option | Verdict | Reason |
|--------|---------|--------|
| 1. Configurable defaulted field + pin | **Chosen** | Makes the budget explicit (Goal 1), stable (Goal 2), documented under 35 (Goal 3), and tunable per deployment without a rebuild (Goal 4) — while keeping a safe default so Jess sees nothing new (Goal 5). |
| 2. Pin only | Rejected | Budget stable but unreadable at call site. |
| 3. Sizes only | Rejected | Other pool defaults still drift on a bump. |
| 4. Module constants | Rejected (superseded) | Editing library source to tune a deploy is the wrong shape for a framework; the defaulted field gives the same default at no extra cost. |
| 5. Shared pool | Rejected | Out of scope; couples unrelated subsystems. |

## Decision outcome

Adopt Option 1. Two coordinated changes, both small and reviewable.

### 1. Pin `hasql-pool` in `core/nhcore.cabal`

Replace the unbounded entry at `core/nhcore.cabal:56` with a bound
consistent with the resolved version (`1.3.0.4`) and the repo's
existing bounded-dependency style (`dotenv`, `fast-logger`,
`opt-env-conf` already use `>= … && < …`):

```text
hasql-pool >= 1.3 && < 1.4,
```

The upper bound `< 1.4` freezes the default-size behaviour and the
`Hasql.Pool.Config` API (`size`, `agingTimeout`, `idlenessTimeout`,
`observationHandler`) nhcore depends on. A future minor bump that keeps
the API is a one-line, reviewed widening.

### 2. Configurable, defaulted `poolSize` field on each Postgres config

Each Postgres config record gains a `poolSize :: Int` field that feeds
`HasqlPoolConfig.size`, defaulted via a `Default` instance so an unset
field is the safe `B1ms` value. EventStore example
(`Service.EventStore.Postgres.Internal`):

```haskell
data PostgresEventStore = PostgresEventStore
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int,
    poolSize :: Int   -- defaults to 6, shared by the FileUpload pool
  }
  deriving (Eq, Ord, Show)


instance Default PostgresEventStore where
  def =
    PostgresEventStore
      { host = "", databaseName = "", user = "", password = "",
        port = 5432, poolSize = 6 }


toConnectionPoolSettings :: Int -> LinkedList Hasql.Setting -> HasqlPoolConfig.Config
toConnectionPoolSettings poolSize settings =
  [ HasqlPoolConfig.staticConnectionSettings settings
  , HasqlPoolConfig.size poolSize
  , HasqlPoolConfig.agingTimeout 300
  , HasqlPoolConfig.idlenessTimeout 60
  , HasqlPoolConfig.observationHandler logPoolObservation
  ]
    |> HasqlPoolConfig.settings
```

`PostgresQueryObjectStoreConfig` takes the analogous change with a
`poolSize` field defaulted to `4`; its `acquirePool` reads
`cfg.poolSize`. FileUpload's `createPool` takes a `PostgresEventStore`
and reads `cfg.poolSize` too — **it shares the EventStore config's pool
size** rather than owning an independent field, so there is no separate
FileUpload knob. The existing `agingTimeout` / `idlenessTimeout` /
`observationHandler` entries are unchanged (ADR-0027 behaviour
preserved).

### 3. Sizes are defaulted config fields, shared where the config is shared

The EventStore and QueryObjectStore sizes are defaulted fields on their
respective config records; the FileUpload pool reuses the
`PostgresEventStore.poolSize` it is already given. Because the fields are
defaulted, the public config surface that Jess must understand is
unchanged for hello-world — `def { host = ..., ... }` still works and an
omitted `poolSize` is the safe `B1ms` value. A tier upgrade sets the
field (e.g. via the application's own config / env wiring, as the
testbed demonstrates with a `DB_POOL_SIZE`-backed `dbPoolSize` field),
with no rebuild of nhcore. WI-2 (#681) now scopes only the shared
connection-settings builder + `sslmode`, and could later give FileUpload
an independent size if its shared default proves too generous.

### 4. Module placement

No new modules. Changes are confined to:

```text
core/nhcore.cabal                                          -- pin hasql-pool
core/service/Service/EventStore/Postgres/Internal.hs       -- poolSize field + Default + size
core/service/Service/QueryObjectStore/Postgres.hs          -- poolSize field + Default + size
core/service/Service/FileUpload/FileStateStore/Postgres.hs -- size from shared PostgresEventStore.poolSize
```

### Performance & testing

This is a configuration change, not a hot-path change — there is no new
allocation, no new per-event work. `size` is read once at pool
construction. No benchmark is required (tier: `simple`).

Verification is by assertion on the defaulted fields and the documented
budget (`core/test-service/Service/EventStore/Postgres/PoolBudgetSpec.hs`):

1. **Default assertion** — a pure unit test asserts
   `(def :: PostgresEventStore).poolSize == 6` and
   `(def :: PostgresQueryObjectStoreConfig).poolSize == 4`, pinning the
   `B1ms` defaults supplied by the `Default` instances.
2. **Override respected** — the same spec asserts
   `(def { poolSize = 10 }).poolSize == 10`, confirming the field is
   genuinely configurable.
3. **Budget arithmetic** — a pure unit test asserts
   `eventStorePoolSize + queryObjectStorePoolSize + fileUploadPoolSize
   (= 6 + 4 + 6, FileUpload sharing the EventStore size) + 3 (listener
   pair + init-listen) = 19 ≤ 35 − adminHeadroom`, encoding the `B1ms`
   budget so a future default bump that breaks it fails the suite rather
   than production.
4. **No regression** — existing EventStore / QueryObjectStore /
   FileUpload specs continue to pass (Postgres-gated specs self-skip
   when `POSTGRES_AVAILABLE` is unset).

## Public API

**Additive, defaulted field.** `PostgresEventStore` and
`PostgresQueryObjectStoreConfig` each gain a `poolSize :: Int` field,
defaulted (6 and 4 respectively) via a `Default` instance. FileUpload's
config is `PostgresEventStore`, so it shares that pool size — no new
FileUpload field. Because the field is defaulted, idiomatic construction
via `def { ... }` is unchanged for callers who do not care about it, and
Jess's `Application.withEventStore postgresConfig` call works exactly as
before. The only new surface is the optional `poolSize` field for
operators who want to raise the budget per deployment.

```haskell
-- Default budget (B1ms-safe) — poolSize omitted, uses the Default instance.
postgresEventStoreConfig :: PostgresEventStore
postgresEventStoreConfig =
  def { host = "...", databaseName = "...", user = "...", password = "..." }

-- Tier upgrade — raise the budget for a larger Postgres tier.
biggerConfig :: PostgresEventStore
biggerConfig = postgresEventStoreConfig { poolSize = 12 }

app :: Application
app =
  Application.new
    |> Application.withEventStore postgresEventStoreConfig
    |> Application.useQueryObjectStore postgresQueryObjectStoreConfig
    |> Application.withService userService
```

## Consequences

### Positive

1. **The connection budget is explicit and reviewable.** A deploy
   reviewer reads the defaulted `poolSize` fields and the `Default`
   instances, not `hasql-pool` source.
2. **The budget is stable across dependency bumps.** The `< 1.4` pin
   freezes the default-size behaviour and the `Config` API nhcore uses.
3. **`B1ms` fits with margin.** Worst-case steady-state is `19 + N`
   connections; with the documented `N` cap the app stays under the
   35-usable ceiling, leaving room for `psql` and maintenance.
4. **Tier upgrades need no rebuild.** Moving to General Purpose is
   setting `poolSize` on the config (e.g. from an env var, as the
   testbed shows), with the budget math in this ADR as the guide.
5. **Jess sees a defaulted field, not a required one.** The field is
   optional at the call site (`def { ... }` omits it), so hello-world
   and the in-memory default path are unaffected.
6. **Sets up WI-2 cleanly.** The `poolSize` field is expressed so the
   shared builder (#681) can lift it without re-deciding the numbers,
   and can later split FileUpload onto its own size.

### Negative

1. **The numbers are a judgement call, not measured.** `6 / 4` plus a
   FileUpload pool that shares the EventStore `6` is reasoned from each
   pool's role, not from production load data. They are deliberately
   conservative for `B1ms`; a real deployment may show the EventStore
   pool wants more and FileUpload wants none. The budget math, not the
   exact split, is the contract.
2. **FileUpload shares the EventStore size.** Until WI-2, FileUpload has
   no independent knob — raising the EventStore size raises FileUpload's
   too. This is accepted because FileUpload is often unused and the
   shared default keeps the budget coherent; WI-2 (#681) can split it.
3. **Two new public config fields.** `poolSize` on two records is new
   surface area Jess could in principle misconfigure. Mitigated by the
   safe default — an omitted field is always correct for `B1ms`.

### Risks

| Risk | Mitigation |
|------|------------|
| Sizes too small → pool starvation under real load (acquisition timeouts). | `acquisitionTimeout` surfaces this as a clean retryable error, not a hang; sizes are documented as a starting point and the budget math shows headroom to raise EventStore first. |
| Sizes too large → `B1ms` connection exhaustion under stream-subscription churn. | Per-stream connections are the unbounded term; WI-4 (#683) releases them on unsubscribe and documents the concurrency cap. The budget test fails the build if the fixed terms alone exceed the ceiling. |
| WI-2 builder merge silently drops a `size` entry or default. | The `PoolBudgetSpec` default assertions (Testing §1–2) fail if either `Default` instance stops yielding the documented size, and the budget arithmetic test (Testing §3) fails if the defaults stop fitting under the ceiling. |
| `hasql-pool` minor bump changes the `Config` API behind the `< 1.4` bound. | Pinned to `< 1.4`; a bump is a reviewed, deliberate widening, at which point the `size` API is re-verified. |

### Mitigations

- The aggregate budget table in this ADR is mirrored into the
  deployment guide by WI-6 (#685), so operators have it without reading
  the ADR.
- The budget unit test encodes the `≤ 35` constraint, so any future
  size change is gated by CI rather than by a human remembering this
  ADR.
- Pool sizes are conservative defaults; the documented tuning order
  (raise EventStore first) gives operators a safe path.

## References

- [#680: WI-1 — Pin `hasql-pool` + explicit pool sizes](https://github.com/neohaskell/NeoHaskell/issues/680)
- [#679: Epic — Deploy-readiness on Azure Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)
- [#681: WI-2 — Unify connection-settings builders + sslmode](https://github.com/neohaskell/NeoHaskell/issues/681) — now scoped to the shared builder and `sslmode`; the `poolSize` field landed here. WI-2 could later give FileUpload an independent size.
- [#683: WI-4 — Release per-stream connections on unsubscribe](https://github.com/neohaskell/NeoHaskell/issues/683) — bounds the per-stream `N` term.
- [#685: WI-6 — Deployment documentation](https://github.com/neohaskell/NeoHaskell/issues/685) — mirrors this budget into the deploy guide.
- [ADR-0027: PostgreSQL Connection Pool Health for Serverless Databases](0027-postgres-pool-health.md) — `agingTimeout` / `idlenessTimeout` / `observationHandler`; this ADR adds `size` alongside them.
- [ADR-0037: PostgreSQL LISTEN Keepalive + Supervised Reconnect](0037-postgres-listen-keepalive-reconnect.md) — the listener pair counted in the budget.
- [ADR-0039: Fix LISTEN/NOTIFY Connection Leak](0039-fix-listen-notify-connection-leak.md) — per-stream connection lifecycle this budget depends on.
- [core/nhcore.cabal](../../core/nhcore.cabal) (line 56) — the unpinned `hasql-pool` entry.
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) (lines 79-86) — EventStore pool config.
- [core/service/Service/QueryObjectStore/Postgres.hs](../../core/service/Service/QueryObjectStore/Postgres.hs) (lines 102-107) — QueryObjectStore pool config.
- [core/service/Service/FileUpload/FileStateStore/Postgres.hs](../../core/service/Service/FileUpload/FileStateStore/Postgres.hs) (lines 174-180) — FileUpload pool config.
- [core/service/Service/EventStore/Postgres/Notifications.hs](../../core/service/Service/EventStore/Postgres/Notifications.hs) (lines 29-65) — the persistent listener pair.
