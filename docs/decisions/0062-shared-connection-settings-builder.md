# ADR-0062: Single Shared Postgres Connection-Settings Builder

> Issue: [#681 — WI-2: Unify the three Postgres connection-settings builders into one shared builder](https://github.com/neohaskell/NeoHaskell/issues/681)

## Status

Proposed

## Context

### Current State

nhcore opens Postgres connections from **three independent `hasql-pool`
pools**, and each pool builds its own libpq connection parameters and its
own `Hasql.Pool.Config` inline. The three builders have **drifted**: they
do not agree on which connection parameters or pool settings to apply.

| Pool | Builder | Keepalives? | Observation handler? | Pool size (ADR-0060) |
|------|---------|-------------|----------------------|----------------------|
| EventStore | `toConnectionSettings` + `toConnectionPoolSettings` (`core/service/Service/EventStore/Postgres/Internal.hs:100-146`) | **yes** (`:140-144`) | **yes** | `PostgresEventStore.poolSize` (default 6) |
| FileUpload | `createPool` (`core/service/Service/FileUpload/FileStateStore/Postgres.hs:168-193`) | **no** | yes | shares `PostgresEventStore.poolSize` |
| QueryObjectStore | `acquirePool` (`core/service/Service/QueryObjectStore/Postgres.hs:113-141`) | **no** | **no** | `PostgresQueryObjectStoreConfig.poolSize` (default 4) |

Three concrete drifts:

1. **TCP keepalives only on EventStore.** The EventStore builder passes
   the four libpq keepalive params from ADR-0037 (`keepalives`,
   `keepalives_idle`, `keepalives_interval`, `keepalives_count`).
   FileUpload and QueryObjectStore pass **none** — their pooled
   connections fall back to the OS default keepalive idle (typically 2
   hours on Linux), far longer than any cloud NAT or Flexible Server
   maintenance window.
2. **Pool observation handler only on EventStore + FileUpload.** The
   ADR-0027 `observationHandler` (which logs aging / idleness / network
   terminations) is wired on EventStore and FileUpload but **not** on
   QueryObjectStore — so QueryObjectStore connection terminations are
   invisible.
3. **The handler itself is duplicated verbatim.** `logPoolObservation`
   is copy-pasted identically into `Internal.hs:114-128` and
   `FileStateStore/Postgres.hs:199-213` — two copies of the same 14-line
   function that must be kept in lockstep by hand.

The cost of this drift is operational. Across a Flexible Server
maintenance restart or an idle network drop, the QueryObjectStore and
FileUpload pools will **not detect dead sockets quickly** (no
keepalives), and a QueryObjectStore connection death is **not logged**
(no observation handler). Worse, the drift is structural: the future
`sslmode` hook from WI-5 (#684), and any other connection parameter,
would have to be added in **three places** and is liable to drift again.

ADR-0060 (#680, WI-1) already made the *pool size* explicit and shared
(FileUpload reuses `PostgresEventStore.poolSize`; QueryObjectStore has
its own `poolSize`). This ADR completes the unification for the
*connection parameters* and the *pool settings* themselves.

### Use Cases

- **Flexible Server maintenance restart.** When Azure restarts the
  Burstable `B1ms` instance for patching, every pooled connection's
  socket dies. With keepalives on all three pools, each pool detects the
  dead socket within ≈80 s (ADR-0037 timing) and recycles it, instead of
  the QueryObjectStore / FileUpload pools handing out a dead connection
  on the next `use` and surfacing a confusing first-query error.
- **Idle network drop on a NAT gateway.** A QueryObjectStore pool that
  has been idle through a cold-start window keeps its NAT mapping alive
  via keepalive probes, rather than silently losing the mapping and
  failing the first read-model write after the app warms up.
- **One place to add `sslmode` (WI-5).** When #684 adds opt-in
  `sslmode=require`, it threads it through the single builder and all
  three pools get it for free, gated by one config field — not three
  edits that can disagree.
- **One place to read the connection contract.** A deploy reviewer reads
  one function to know exactly which libpq params and pool settings every
  nhcore Postgres pool uses.

### Design Goals

1. **Single source of truth for connection params.** Exactly one
   function constructs the libpq `Param` list (host, port, dbname, user,
   password, the four keepalives, and the future `sslmode`). The three
   pools call it; none builds params inline.
2. **Single source of truth for pool settings.** Exactly one function
   constructs the `Hasql.Pool.Config` (static settings + size + aging +
   idleness + observation handler), parameterised by the per-pool size.
   The observation handler is defined once.
3. **Uniform keepalives + observation handler.** After this change all
   three pools carry the ADR-0037 keepalives and the ADR-0027 observation
   handler. The two drifts disappear by construction.
4. **No behavioural regression for the EventStore path.** The EventStore
   already had keepalives + handler + its `poolSize`; its emitted
   params and pool config must be byte-for-byte equivalent before and
   after. The change is additive for the other two pools only.
5. **Reserve the `sslmode` seam for WI-5 without building it now.** The
   shared builder is shaped so #684 adds `sslmode` by extending one
   input and one `Param` line, with no further structural change. WI-5
   is out of scope here; this ADR only makes its landing trivial.
6. **Invisible to Jess.** This is internal plumbing. No new public type,
   no new public function, no new config field on any config record. The
   `Application.withEventStore postgresConfig` surface is unchanged.

### GitHub Issue

- [#681: WI-2 — Unify the three Postgres connection-settings builders into one shared builder](https://github.com/neohaskell/NeoHaskell/issues/681)
- Parent epic: [#679 — Deploy-readiness on Azure Database for PostgreSQL Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)

## Decision drivers

- **The drift is a silent correctness gap, not a style nit.** Two of
  three pools cannot detect a dead socket promptly and one cannot log a
  termination. On the epic's deploy target (Flexible Server `B1ms`,
  behind ACA scale-to-zero) connection deaths are *routine*, so the gap
  is hit in normal operation, not edge cases.
- **WI-5 forces the question now.** #684 will add `sslmode`. Adding it to
  three drifted builders re-creates the exact 3-way duplication this work
  item exists to remove. Unifying first is the cheaper order.
- **The EventStore path is the regression risk.** It is the only pool
  whose params already include keepalives; the refactor must preserve its
  output exactly. The other two pools are *gaining* params, which is the
  intended behaviour change, not a regression.
- **Keep the surface Jess-invisible.** This is deploy-tier plumbing. It
  must add no public type, function, or config knob. The existing
  defaulted `poolSize` fields (ADR-0060) remain the only connection-pool
  surface anyone configures.
- **Honour the config-sharing decision from ADR-0060.** FileUpload still
  shares `PostgresEventStore.poolSize` (Option A). This ADR does **not**
  give FileUpload an independent size — that remains a possible future
  split, deliberately not taken here.

## Considered options

### Option 1 — Two shared functions in `EventStore.Postgres.Core`, both pools import them (chosen)

Place the two builders in the existing
`Service.EventStore.Postgres.Core` module (already imported by
`Internal.hs`):

- `toConnectionParams :: ConnectionParams -> LinkedList Hasql.Setting` —
  the single libpq param constructor (host/port/dbname/user/password +
  the four keepalives, with the `sslmode` seam reserved for WI-5).
- `toPoolConfig :: Int -> LinkedList Hasql.Setting -> HasqlPoolConfig.Config`
  — the single pool-config constructor (static settings + `size` +
  `agingTimeout` + `idlenessTimeout` + `observationHandler`), with
  `logPoolObservation` defined once here too.

`ConnectionParams` is a small **internal** record (`host`, `databaseName`,
`user`, `password`, `port`) that both config types project into — it is
*not* exported from any public module and is not a new public type Jess
sees. EventStore's `toConnectionSettings` becomes a thin projection of
`PostgresEventStore` into `ConnectionParams` then a call to
`toConnectionParams`; QueryObjectStore's `acquirePool` and FileUpload's
`createPool` do the same projection and call `toConnectionParams` +
`toPoolConfig`.

- Chosen: `Postgres.Core` is the lowest module all three pools can depend
  on without a cycle (EventStore already imports it; FileUpload already
  imports `Service.EventStore.Postgres`; QueryObjectStore can import the
  `Core` module without pulling in EventStore's `Internal`). Both
  builders live together; the observation handler is defined once; the
  `sslmode` seam is a single `Param` line behind one `ConnectionParams`
  extension.

### Option 2 — Keep the builders in `EventStore.Postgres.Internal`, export them

Export `toConnectionSettings` / `toConnectionPoolSettings` /
`logPoolObservation` from `Internal.hs` and have the other two pools
import them.

- Rejected: forces QueryObjectStore and FileUpload to depend on
  EventStore's `Internal` module — a heavy, churny module that carries
  the whole event-store implementation. It also couples two unrelated
  subsystems to EventStore internals just to share five lines of param
  construction. A small dedicated home (`Postgres.Core`) is the right
  altitude.

### Option 3 — A new top-level `Service.Postgres.ConnectionConfig` module

Create a brand-new module for the shared builder.

- Rejected for now: a new module is more ceremony than this 2-function
  refactor needs, and #460 / #346 (the layered-package / `ServiceConfig`
  refactors) are the issue's named long-term home for cross-subsystem
  config. Introducing a competing module here would pre-empt that
  decision. `Postgres.Core` already exists and is the natural seam;
  when #460/#346 land, lifting these two functions into the
  `ServiceConfig` layer is a move, not a redesign.

### Option 4 — One shared pool for all three subsystems

Collapse the three pools onto one `hasql-pool`.

- Rejected: same reasoning as ADR-0060 Option 5. Out of scope; the three
  subsystems have different lifecycles, and merging couples FileUpload
  (often unused) contention onto the EventStore hot path. This ADR
  unifies the *builders*, not the *pools*.

| Option | Verdict | Reason |
|--------|---------|--------|
| 1. Two functions in `Postgres.Core` | **Chosen** | Lowest no-cycle home; both builders + handler in one place; trivial `sslmode` seam; Jess-invisible. |
| 2. Export from `Internal` | Rejected | Couples two subsystems to EventStore's heavy internal module. |
| 3. New top-level module | Rejected | More ceremony than needed; pre-empts the #460/#346 config layer. |
| 4. One shared pool | Rejected | Out of scope; couples unrelated subsystem lifecycles. |

## Decision outcome

Adopt Option 1. One internal record and two shared functions in
`Service.EventStore.Postgres.Core`; all three pools route through them.

### 1. Internal `ConnectionParams` record

A small internal record both config types project into. It is **not**
exported publicly and adds no surface Jess configures.

```haskell
-- | Internal connection inputs shared by all three Postgres pools.
-- Not a public type: each config record (PostgresEventStore,
-- PostgresQueryObjectStoreConfig) projects into it. The sslmode hook
-- for WI-5 (#684) is added here as a single field when that work lands.
data ConnectionParams = ConnectionParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int
  }
  deriving (Eq, Ord, Show)
```

### 2. `toConnectionParams` — the single libpq param builder

The one place connection params are constructed. Carries the ADR-0037
keepalives for every pool. The `sslmode` line for WI-5 is the only
addition #684 makes here.

```haskell
toConnectionParams :: ConnectionParams -> LinkedList Hasql.Setting
toConnectionParams cfg = do
  let params =
        ConnectionSettingConnection.params
          [ Param.host cfg.host,
            Param.port (fromIntegral cfg.port),
            Param.dbname cfg.databaseName,
            Param.user cfg.user,
            Param.password cfg.password,
            -- TCP keepalive: detect dead connections in cloud
            -- environments (ADR-0037, #397) — now on ALL three pools.
            Param.other "keepalives" "1",
            Param.other "keepalives_idle" "30",
            Param.other "keepalives_interval" "10",
            Param.other "keepalives_count" "5"
            -- WI-5 (#684) adds: Param.other "sslmode" "<mode>" here.
          ]
  [params |> ConnectionSetting.connection]
```

### 3. `toPoolConfig` — the single pool-config builder

The one place `Hasql.Pool.Config` is constructed, parameterised by the
per-pool size so EventStore/FileUpload (6) and QueryObjectStore (4) keep
their ADR-0060 sizes. The observation handler is defined once, here.

```haskell
toPoolConfig :: Int -> LinkedList Hasql.Setting -> HasqlPoolConfig.Config
toPoolConfig poolSize settings =
  [ HasqlPoolConfig.staticConnectionSettings settings,
    HasqlPoolConfig.size poolSize,
    HasqlPoolConfig.agingTimeout 300,
    HasqlPoolConfig.idlenessTimeout 60,
    HasqlPoolConfig.observationHandler logPoolObservation
  ]
    |> HasqlPoolConfig.settings
```

### 4. The three pools become thin projections

Each pool projects its config into `ConnectionParams`, then calls the
shared builders. No pool constructs params or pool config inline.

```haskell
-- EventStore (Internal.hs): toConnectionSettings keeps its name/signature
-- (it is re-exported) and delegates.
toConnectionSettings :: PostgresEventStore -> LinkedList Hasql.Setting
toConnectionSettings cfg =
  Core.toConnectionParams
    Core.ConnectionParams
      { host = cfg.host,
        databaseName = cfg.databaseName,
        user = cfg.user,
        password = cfg.password,
        port = cfg.port
      }

-- defaultOps.acquire then pipes that through Core.toPoolConfig cfg.poolSize
-- exactly as toConnectionPoolSettings did before.
```

QueryObjectStore's `acquirePool` and FileUpload's `createPool` do the
same projection-then-`toConnectionParams`/`toPoolConfig`, replacing their
inline `params` / `poolConfig` `let`-blocks. The fail-fast
`poolSize > 0` checks added in ADR-0060 stay exactly where they are — the
shared builder is reached only on the `True` branch. The two now-dead
inline copies of `logPoolObservation` (in `Internal.hs` and
`FileStateStore/Postgres.hs`) are deleted in favour of the single
`Core` definition.

### 5. FileUpload still shares `PostgresEventStore.poolSize`

Unchanged from ADR-0060 (Option A). FileUpload's `createPool` continues
to take a `PostgresEventStore` and read `cfg.poolSize`; this ADR does
**not** add an independent FileUpload size. An independent size remains a
possible future split, explicitly not taken here.

### 6. Module placement

No new module. Changes are confined to:

```text
core/service/Service/EventStore/Postgres/Core.hs            -- new ConnectionParams + toConnectionParams + toPoolConfig + logPoolObservation
core/service/Service/EventStore/Postgres/Internal.hs        -- toConnectionSettings delegates; inline pool config + handler removed
core/service/Service/QueryObjectStore/Postgres.hs           -- acquirePool delegates; inline params/poolConfig removed
core/service/Service/FileUpload/FileStateStore/Postgres.hs  -- createPool delegates; inline params/poolConfig + handler removed
```

### Performance & testing

This is a structural refactor on a cold path — connection params and pool
config are constructed **once per pool at startup**, never per request or
per event. There is no new allocation on any hot path and no benchmark is
required (tier: `moderate`; the change is plumbing, not a new IO path).

Verification is by a pure unit spec
(`core/test-service/Service/EventStore/Postgres/ConnectionSettingsSpec.hs`),
running unconditionally with no Postgres, asserting the shared builder's
output:

1. **Keepalives present, all four** — assert
   `toConnectionParams` emits `keepalives` / `keepalives_idle` /
   `keepalives_interval` / `keepalives_count` (the ADR-0037 set), so the
   keepalive contract is pinned for every pool that calls it.
2. **EventStore params unchanged (no regression)** — assert that
   `toConnectionSettings cfg` (EventStore's projection) yields the same
   `Setting` list it produced before the refactor, pinning Design Goal 4.
3. **Pool config carries size + handler + timeouts** — assert
   `toPoolConfig n settings` sets `size = n`, the ADR-0027 aging /
   idleness timeouts, and the observation handler, so the QueryObjectStore
   pool's newly-added handler cannot silently regress.
4. **Single observation handler** — assert (by construction / module
   surface) that `logPoolObservation` is defined once in `Core`; the two
   inline copies are gone.
5. **No regression** — the existing `PoolBudgetSpec` (ADR-0060) and the
   EventStore / QueryObjectStore / FileUpload specs continue to pass;
   Postgres-gated specs self-skip when `POSTGRES_AVAILABLE` is unset.

`hasql`'s `Setting` / `Param` values are opaque (no `Eq`/`Show` on the
final `ConnectionSetting`), so assertions are written against the
`ConnectionParams → Setting` boundary the spec can observe — the param
*inputs* and the pool-config *inputs* — rather than the opaque end value;
this is sufficient to pin keepalive presence, the size, and handler
wiring. The implementation phase confirms the exact assertable surface
the `hasql` version in the pin (`>= 1.3 && < 1.4`, ADR-0060) exposes.

## Public API

**None.** This change introduces no new public type, function, or config
field. `ConnectionParams`, `toConnectionParams`, and `toPoolConfig` are
internal to the Postgres event-store package and are not re-exported from
any module Jess imports. `toConnectionSettings` keeps its existing
name and signature for the EventStore (it is already exported from
`Internal`). The application-facing surface —
`Application.withEventStore postgresConfig`,
`def { host = ..., ... }`, and the defaulted `poolSize` fields from
ADR-0060 — is byte-for-byte unchanged.

```haskell
-- Unchanged for callers — the same construction works exactly as before.
postgresEventStoreConfig :: PostgresEventStore
postgresEventStoreConfig =
  def { host = "...", databaseName = "...", user = "...", password = "..." }

app :: Application
app =
  Application.new
    |> Application.withEventStore postgresEventStoreConfig
    |> Application.useQueryObjectStore postgresQueryObjectStoreConfig
    |> Application.withService userService
```

## Consequences

### Positive

1. **Keepalives now cover all three pools.** QueryObjectStore and
   FileUpload detect dead sockets in ≈80 s across a Flexible Server
   restart or NAT drop, instead of falling back to the 2-hour OS default.
2. **Observation handler now covers all three pools.** QueryObjectStore
   connection terminations are logged, closing the last observability
   gap among the pools.
3. **One place to change connection behaviour.** Adding `sslmode` (WI-5)
   or any future param is a single edit to `toConnectionParams`; it
   cannot drift across pools again.
4. **The observation handler is defined once.** The two verbatim copies
   of `logPoolObservation` collapse into one; they can no longer fall out
   of lockstep.
5. **No regression risk for EventStore.** Its emitted params and pool
   config are pinned equal pre/post by the spec.
6. **Jess sees nothing new.** No public type, function, or config knob is
   added; the application wiring is identical.

### Negative

1. **QueryObjectStore and FileUpload change behaviour.** They gain
   keepalives (and QueryObjectStore gains the handler). This is the
   *intended* fix, but it is a behaviour change: their connections now
   carry keepalive probes they did not before. The risk is negligible
   (keepalives are passive TCP probes) but it is not a pure no-op for
   those two pools.
2. **A small internal record is added.** `ConnectionParams` is new code,
   even if not public. It is the price of one builder that does not
   depend on three different config types.
3. **`Postgres.Core` gains responsibility.** A module previously holding
   only event-store core types now also owns the shared connection
   builder. Mitigated by the #460/#346 plan to lift it into a dedicated
   config layer later — at which point this is a move, not a redesign.

### Risks

| Risk | Mitigation |
|------|------------|
| Refactor silently changes the EventStore param/pool output. | Spec test 2 pins `toConnectionSettings` output equal to its pre-refactor value; spec test 3 pins the pool-config inputs. |
| QueryObjectStore's newly-added handler regresses (e.g. dropped in a later edit). | Spec test 3 asserts the pool config carries the observation handler. |
| `hasql`'s opaque `Setting` value blocks direct equality assertions. | Assertions are written at the observable `ConnectionParams → [Param]` boundary, not the opaque end value; the implementation phase confirms the exact assertable surface for the pinned `hasql` version. |
| Import cycle if the builder is placed wrong. | Placed in `Postgres.Core`, the lowest module all three pools already depend on (directly or transitively); no new edge from `Core` back up to `Internal`. |
| WI-5 `sslmode` lands and re-introduces drift. | The seam is a single field on `ConnectionParams` + one `Param` line in `toConnectionParams`; #684 cannot add `sslmode` to one pool and not another because there is only one builder. |

### Mitigations

- The keepalive and pool-config contracts are encoded in the unit spec,
  so a future edit that drops a param or the handler fails CI rather than
  shipping a silent regression.
- The `sslmode` seam is documented in `toConnectionParams` as a comment
  marking exactly where WI-5 extends it, so #684 has an unambiguous
  landing point.
- The change is confined to the four files listed in §6; the spec's
  no-regression assertion (test 5) plus the existing `PoolBudgetSpec`
  guard the ADR-0060 budget contract across the refactor.

## References

- [#681: WI-2 — Unify the three connection-settings builders](https://github.com/neohaskell/NeoHaskell/issues/681)
- [#679: Epic — Deploy-readiness on Azure Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)
- [#684: WI-5 — Optional `sslmode` TLS hardening](https://github.com/neohaskell/NeoHaskell/issues/684) — lands in the shared builder defined here.
- [ADR-0060: Explicit Postgres Connection-Pool Budget for Flexible Server B1ms](0060-postgres-pool-budget.md) — added the `poolSize` fields this builder reads; FileUpload shares `PostgresEventStore.poolSize` (Option A), unchanged here.
- [ADR-0037: PostgreSQL LISTEN Connection Keepalive and Reconnection](0037-postgres-listen-keepalive-reconnect.md) — the keepalive params now applied to all three pools.
- [ADR-0027: PostgreSQL Connection Pool Health for Serverless Databases](0027-postgres-pool-health.md) — the `agingTimeout` / `idlenessTimeout` / `observationHandler` now applied to all three pools.
- [core/service/Service/EventStore/Postgres/Internal.hs](../../core/service/Service/EventStore/Postgres/Internal.hs) (lines 100-146) — EventStore builder (has keepalives + handler).
- [core/service/Service/QueryObjectStore/Postgres.hs](../../core/service/Service/QueryObjectStore/Postgres.hs) (lines 113-141) — QueryObjectStore builder (no keepalives, no handler).
- [core/service/Service/FileUpload/FileStateStore/Postgres.hs](../../core/service/Service/FileUpload/FileStateStore/Postgres.hs) (lines 168-213) — FileUpload builder (no keepalives, duplicate handler).
- [core/service/Service/EventStore/Postgres/Core.hs](../../core/service/Service/EventStore/Postgres/Core.hs) — proposed home for the shared builder.
