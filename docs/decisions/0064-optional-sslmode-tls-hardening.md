# ADR-0064: Optional `sslmode` TLS Hardening for Postgres Connections

> Issue: [#684 — WI-5: Optional `sslmode` TLS hardening for Postgres connections](https://github.com/neohaskell/NeoHaskell/issues/684)

## Status

Proposed

## Context

### Current State

nhcore opens every Postgres connection through the single shared builder
introduced by ADR-0062 (#681): all three pools (EventStore,
QueryObjectStore, FileUpload) and the LISTEN/NOTIFY paths route their
libpq parameters through
`Service.Infra.Postgres.ConnectionConfig.toConnectionParams`. That builder
emits `host`, `port`, `dbname`, `user`, `password`, and the four ADR-0037
keepalive params — and **nothing about TLS**.

With no `sslmode` set, libpq applies its compiled-in default
`sslmode=prefer`: the client offers TLS, uses it if the server supports
it, and **silently falls back to an unencrypted connection** if the
server (or a man-in-the-middle) declines. Against Azure Database for
PostgreSQL — Flexible Server (the epic #679 deploy target) and Neon, the
default `prefer` already negotiates TLS, so **connectivity is not the
problem**: the deployment connects today with zero TLS config.

The gap is a *security-posture* one, not a connectivity one. `prefer`
permits a **silent downgrade**: if the TLS negotiation is stripped (a
mis-configured server, a downgrade attack on the path between the ACA
container and Flexible Server), the connection proceeds in plaintext and
the password plus all event data cross the wire unencrypted, with no
error and no log line. Production hygiene wants `sslmode=require` (forbid
the downgrade) or `sslmode=verify-full` (forbid the downgrade *and*
authenticate the server against a trusted CA, defeating an active MITM).

ADR-0062 deliberately **reserved the seam** for exactly this work: a
single comment in `toConnectionParams` marks where one `Param.other
"sslmode" "<mode>"` line lands, and the consequences section states that
because there is only one builder, WI-5 "cannot add `sslmode` to one pool
and not another." This ADR fills that seam.

### Use Cases

- **Production deploy forbids silent downgrade.** An operator deploying
  to Flexible Server sets one env var (`DB_SSL_MODE=require`); every nhcore
  Postgres connection now refuses to proceed unencrypted. A downgrade
  attempt fails loudly at connect time instead of leaking plaintext.
- **High-assurance deploy authenticates the server.** An operator who
  also wants MITM protection sets `DB_SSL_MODE=verify-full` and points
  `DB_SSL_ROOT_CERT` at a CA bundle (DigiCert Global Root G2 + Microsoft
  RSA Root CA 2017 for Flexible Server). libpq then verifies the server
  certificate chains to a trusted **root** and that the hostname matches.
- **Localhost / dev is untouched.** A developer running `docker compose`
  Postgres with no TLS sets nothing. The field defaults to off, libpq
  keeps its `prefer` behaviour, and the local stack connects exactly as
  before. No developer has to learn TLS to run the test suite.
- **One knob, all pools.** Because the value threads through the single
  ADR-0062 builder, setting it once applies it to the EventStore pool, the
  QueryObjectStore pool, the FileUpload pool, and the LISTEN/NOTIFY
  one-off connections uniformly — it is structurally impossible to harden
  one pool and forget another.

### Design Goals

1. **Opt-in, default-off.** The hardening must default to "unset" so the
   current dev and CI behaviour (no TLS against localhost) is byte-for-byte
   preserved. Turning it on is a deliberate per-deployment choice.
2. **One field, threaded through the one builder.** The mode is added as a
   single optional input on `ConnectionParams`, resolved into
   `ResolvedParams`, and emitted by `toConnectionParams` as one
   conditional `Param.other "sslmode"` line — landing exactly on the seam
   ADR-0062 reserved. All three pools get it because they all project into
   `ConnectionParams`.
3. **Server-auth only — never client certs.** Support `disable` / `allow`
   / `prefer` / `require` / `verify-ca` / `verify-full`, plus the
   `verify-full` companions `sslrootcert` (trust a CA bundle) and
   `channel_binding`. **Do not** add `sslcert` / `sslkey`: Azure Database
   for PostgreSQL does not support client-certificate (mTLS)
   authentication, and supplying them breaks the connection. The hardening
   authenticates the *server* to the *client*, not the reverse.
4. **Root-only trust.** For `verify-full`, the `sslrootcert` bundle holds
   **root** CAs only (DigiCert Global Root G2 + Microsoft RSA Root CA
   2017), never an intermediate or the leaf server cert — Microsoft
   rotates Flexible Server intermediate CAs on an ongoing basis, so
   pinning an intermediate or the server cert would break on rotation.
   This ADR encodes *which cert to trust* as operator guidance; the code
   only forwards the path libpq reads.
5. **Type-safe mode, no stringly-typed footgun.** The mode is a small
   closed enum (`SslMode`), not a free `Text`, so a typo like
   `"requre"` is a compile error in code and a single clean validation
   error from the env string, not a libpq runtime surprise.
6. **Jess-shaped.** A developer never needs this to build a side project
   on localhost — the default hides it completely. An operator turns it on
   with one documented env var whose values read in plain English
   (`require`, `verify-full`).

### GitHub Issue

- [#684: WI-5 — Optional `sslmode` TLS hardening for Postgres connections](https://github.com/neohaskell/NeoHaskell/issues/684)
- Parent epic: [#679 — Deploy-readiness on Azure Database for PostgreSQL Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)

## Decision drivers

- **Silent downgrade is the actual risk.** `prefer` connects but does not
  *guarantee* encryption; a stripped negotiation leaks the password and
  every event in plaintext with no signal. `require`/`verify-full` turns
  that silent failure into a loud connect-time refusal. This is good
  production hygiene on a deploy target where the DB is reached over a
  cloud network path the operator does not fully control.
- **It must not regress dev.** localhost Postgres in `docker compose` has
  no TLS. If the field were on by default, every contributor's test run
  and the CI Postgres suites would fail to connect. Default-off is
  non-negotiable; the issue's first acceptance criterion is "defaults
  preserve current dev behavior."
- **Client certs are a trap, not a feature.** Azure Flexible Server has no
  mTLS; `sslcert`/`sslkey` would break the connection, not harden it. The
  scope is deliberately server-auth only, and the type does not even
  expose a client-cert field, so the footgun is unrepresentable.
- **The seam already exists.** ADR-0062 shaped `toConnectionParams` so
  this is one input field + one conditional `Param` line. Re-opening that
  decision or adding a parallel builder would re-create the very drift
  WI-2 removed. The cheapest correct move is to land on the reserved seam.
- **Keep it invisible to Jess.** This is a deploy-tier knob. It adds an
  *optional, defaulted* field — Jess who never sets it sees no change in
  the `def { host = ..., ... }` construction she copy-pastes from the
  EventStore docs.
- **Root-only trust survives CA rotation.** Microsoft rotates Flexible
  Server intermediate CAs; trusting roots only (and never pinning a
  server/intermediate cert in code) is the configuration that keeps
  working across those rotations. The code forwards a CA-bundle *path*; it
  pins nothing itself.

## Considered options

### Option 1 — A typed `SslMode` enum field on each config, threaded through the seam (chosen)

Add a closed enum to the shared infra module:

```haskell
-- | Optional libpq TLS negotiation mode. 'Default' is 'SslModeUnset',
-- which emits no sslmode param so libpq keeps its compiled-in 'prefer'
-- default -- localhost/dev without TLS is unchanged. Server-auth only:
-- there is deliberately no client-cert (sslcert/sslkey) field, because
-- Azure Flexible Server has no mTLS and supplying them breaks the
-- connection (ADR-0064).
data SslMode
  = SslModeUnset        -- ^ no sslmode param; libpq default 'prefer'
  | SslModeDisable      -- ^ sslmode=disable
  | SslModeAllow        -- ^ sslmode=allow
  | SslModePrefer       -- ^ sslmode=prefer (explicit)
  | SslModeRequire      -- ^ sslmode=require -- forbids silent downgrade
  | SslModeVerifyCa     -- ^ sslmode=verify-ca
  | SslModeVerifyFull   -- ^ sslmode=verify-full -- forbids downgrade + MITM
  deriving (Eq, Ord, Show)
```

Add an `sslMode :: SslMode` field (and an optional `sslRootCert :: Maybe
Text` for the `verify-full` CA bundle path) to `ConnectionParams`,
resolve it through `resolveParams` into `ResolvedParams`, and have
`toConnectionParams` emit the params *only when set* on the reserved seam.
Each pool config record (`PostgresEventStore`,
`PostgresQueryObjectStoreConfig`) gains the matching defaulted fields and
forwards them into its `ConnectionParams` projection; FileUpload reuses
`PostgresEventStore` and is covered for free.

- **Chosen.** Lands exactly on the ADR-0062 seam: one optional input, one
  conditional `Param` line, all pools covered by construction. The closed
  enum makes an invalid mode a compile error in code and a single clean
  validation error from the env string, never a libpq runtime surprise.
  The absence of any `sslcert`/`sslkey` field makes the Azure-breaking
  mTLS footgun *unrepresentable*. Default `SslModeUnset` preserves dev
  behaviour exactly.

### Option 2 — A free-text `sslMode :: Maybe Text` field

Thread a `Maybe Text` straight to `Param.other "sslmode"`.

- Rejected. It is stringly-typed: `Just "requre"` or `Just "verifyfull"`
  type-checks and only fails deep inside libpq at connect time, with a
  message a junior operator cannot map back to a typo. It also leaves the
  client-cert footgun open — nothing stops someone wiring a second
  `Maybe Text` for `sslcert`. The closed enum (Option 1) costs a handful
  of constructors and removes both hazards.

### Option 3 — A single `requireTls :: Bool` flag

Expose one boolean that maps `True -> sslmode=require`.

- Rejected. It collapses the meaningful distinction between `require`
  (encrypt, no MITM protection) and `verify-full` (encrypt + authenticate
  the server), so the high-assurance deploy in the use-cases is
  unreachable. The issue explicitly wants `verify-full` with an
  `sslrootcert`, which a boolean cannot express. A boolean is *less* clear
  to the operator, not more: "require TLS = true" hides that no server
  authentication is happening.

### Option 4 — Always send `sslmode=require` (no opt-out)

Make `require` unconditional.

- Rejected. It breaks every localhost/dev/CI Postgres that has no TLS —
  directly violating the issue's "defaults preserve current dev behavior"
  acceptance criterion. TLS hardening on the deploy target cannot be paid
  for with a broken local development loop.

| Option | Verdict | Reason |
|--------|---------|--------|
| 1. Typed `SslMode` enum on the seam | **Chosen** | Lands on the reserved ADR-0062 seam; closed enum kills the typo + mTLS footguns; default-off preserves dev. |
| 2. Free-text `Maybe Text` | Rejected | Stringly-typed; typos surface only inside libpq; leaves the client-cert footgun open. |
| 3. `requireTls :: Bool` | Rejected | Cannot express `verify-full` + `sslrootcert`; hides the require-vs-verify distinction. |
| 4. Always `require` | Rejected | Breaks every no-TLS localhost/dev/CI Postgres; violates the default-preserving acceptance criterion. |

## Decision outcome

Adopt **Option 1**. Introduce a closed `SslMode` enum and an optional
CA-bundle path in `Service.Infra.Postgres.ConnectionConfig`, thread both
through the existing `ConnectionParams` → `resolveParams` →
`ResolvedParams` → `toConnectionParams` path, and emit the libpq params on
the seam ADR-0062 reserved. The default is `SslModeUnset` (off), so
localhost/dev is unchanged. Server-auth only: no `sslcert`/`sslkey` field
exists.

### 1. The `SslMode` enum and the two new optional inputs

`SslMode` (above) is added to the infra module. `ConnectionParams` gains
two optional, defaulted inputs:

```haskell
data ConnectionParams = ConnectionParams
  { host :: Text,
    databaseName :: Text,
    user :: Text,
    password :: Text,
    port :: Int,
    -- WI-5 (#684): optional TLS hardening. 'SslModeUnset' (the default
    -- the pool configs pass) emits no sslmode param, so libpq keeps its
    -- 'prefer' default and localhost/dev is unchanged.
    sslMode :: SslMode,
    -- Optional CA-bundle path for 'verify-full' (root CAs only; never an
    -- intermediate or server cert -- Microsoft rotates intermediates).
    sslRootCert :: Maybe Text
  }
  deriving (Eq, Ord)
```

`ResolvedParams` carries the same two values through unchanged (it stays
the inspectable mirror the unit spec asserts against), so a test can pin
exactly which params each mode emits.

### 2. `toConnectionParams` emits on the reserved seam

The single conditional addition replaces the ADR-0062 placeholder comment
(`-- WI-5 (#684) adds: Param.other "sslmode" "<mode>" here.`). The
`sslmode` param is appended **only** when the mode is not `SslModeUnset`;
`sslrootcert` and `channel_binding` are appended only for the
verifying modes when a root cert is supplied:

```haskell
-- inside toConnectionParams, after the keepalive params:
let sslParams = case resolved.sslMode of
      SslModeUnset -> []   -- libpq keeps its 'prefer' default; dev unchanged
      mode ->
        [Param.other "sslmode" (sslModeToText mode)]
          ++ rootCertParams resolved.sslRootCert
```

`sslModeToText` maps the enum to the libpq token (`SslModeRequire ->
"require"`, `SslModeVerifyFull -> "verify-full"`, …) via `case ... of`.
`rootCertParams` emits `Param.other "sslrootcert" <path>` (and, for
channel-binding hardening, `Param.other "channel_binding" "require"`)
when a path is present, and `[]` otherwise. **No branch ever emits
`sslcert` or `sslkey`.** When the mode is `SslModeUnset` the emitted
`Setting` list is byte-for-byte identical to today's, which is what
pins the no-regression guarantee for dev/CI.

### 3. The pool config records forward the field; all pools covered

`PostgresEventStore` and `PostgresQueryObjectStoreConfig` each gain
`sslMode :: SslMode` (default `SslModeUnset`) and `sslRootCert :: Maybe
Text` (default `Nothing`) in their record and `Default` instance, and
forward both into their `ConnectionParams` projection. Because FileUpload
reuses `PostgresEventStore`, it inherits the field with no extra edit.
The LISTEN/NOTIFY one-off connections build their settings from the same
`PostgresEventStore` via `toConnectionSettings`, so they are covered too.
This is the ADR-0062 guarantee in action: one builder, every connection
hardened uniformly.

### 4. App-layer config (env var), defaulted off

The application config DSL gains optional fields so an operator can set
the mode from the environment without touching code, defaulting to off:

```haskell
Config.field @Text "dbSslMode"
  |> Config.doc "Postgres TLS mode: unset|disable|require|verify-full (default unset = libpq 'prefer'; localhost/dev needs nothing)"
  |> Config.defaultsTo ("unset" :: Text)
  |> Config.envVar "DB_SSL_MODE"
Config.field @Text "dbSslRootCert"
  |> Config.doc "Path to a root-CA bundle for verify-full (root CAs only)"
  |> Config.defaultsTo ("" :: Text)
  |> Config.envVar "DB_SSL_ROOT_CERT"
```

The `makePostgresConfig` factory parses the `Text` into `SslMode`
(an unknown token is a single clean config error, not a libpq surprise)
and an empty root-cert path into `Nothing`. The default `"unset"`
threads to `SslModeUnset`, so an operator who sets nothing gets exactly
today's behaviour. This wiring is reference-app glue in `testbed/`; nhcore
ships the enum + parser, and the testbed demonstrates the env binding.

### 5. Module / file placement

```text
core/service/Service/Infra/Postgres/ConnectionConfig.hs   -- add SslMode + sslModeToText; sslMode/sslRootCert on ConnectionParams + ResolvedParams; emit on the seam
core/service/Service/EventStore/Postgres/Internal.hs      -- PostgresEventStore gains defaulted sslMode/sslRootCert; toConnectionSettings forwards them
core/service/Service/QueryObjectStore/Postgres.hs         -- PostgresQueryObjectStoreConfig gains defaulted fields; acquirePool forwards them
core/service/Service/FileUpload/FileStateStore/Postgres.hs -- reuses PostgresEventStore; no field edit, projection picks the fields up
testbed/src/Testbed/Config.hs, testbed/src/App.hs         -- optional DB_SSL_MODE / DB_SSL_ROOT_CERT env binding, defaulted off
core/nhcore.cabal                                         -- no new module; SslMode is exported from the existing ConnectionConfig module
```

`SslMode` and `sslModeToText` are added to the existing
`Service.Infra.Postgres.ConnectionConfig` export list — no new module,
no new namespace.

### Performance & testing

This is a startup-cold-path change: the `sslmode` param is appended once
per pool when its connection settings are built, never per request or per
event. There is no new hot-path allocation and no benchmark is required
(tier: `simple`).

Verification extends the existing pure unit spec at
`core/test-service/Service/Infra/Postgres/ConnectionConfigSpec.hs`
(registered manually in `core/test-service/Main.hs`; runs with no
Postgres). Asserted against the inspectable `ResolvedParams` (the opaque
`hasql` `Setting` has no `Eq`/`Show`, exactly as ADR-0062 established):

1. **Default is off (no regression)** — `resolveParams` with `sslMode =
   SslModeUnset` carries `SslModeUnset` and `sslRootCert = Nothing`, and
   `toConnectionParams` emits a `Setting` list equivalent to the
   pre-WI-5 output (no `sslmode` param) — pinning the dev/CI no-change
   guarantee.
2. **`require` emits `sslmode=require`** — `resolveParams`/`ResolvedParams`
   carries `SslModeRequire`, and `sslModeToText SslModeRequire == "require"`.
3. **`verify-full` + root cert emits the verifying params** — with
   `sslMode = SslModeVerifyFull` and `sslRootCert = Just "/etc/ca.pem"`,
   the resolved params carry the mode and the cert path, and
   `sslModeToText SslModeVerifyFull == "verify-full"`.
4. **No client-cert param is ever emitted** — by construction (no
   `sslcert`/`sslkey` field exists) and asserted at the `ResolvedParams`
   surface: there is no client-cert value to inspect.
5. **`sslModeToText` round-trips every constructor** — a table maps each
   `SslMode` to its libpq token (`disable`/`allow`/`prefer`/`require`/
   `verify-ca`/`verify-full`), guarding a future dropped/typo'd token.
6. **Keepalives + no-regression carried** — the four ADR-0037 keepalive
   asserts and the existing `validatePort` boundary asserts continue to
   pass unchanged; the `sslMode` field is additive.

## Public API

The change adds **one exported enum** plus **optional, defaulted config
fields** — nothing a developer who does not set them ever sees.

```haskell
-- New, exported from Service.Infra.Postgres.ConnectionConfig:
data SslMode
  = SslModeUnset | SslModeDisable | SslModeAllow | SslModePrefer
  | SslModeRequire | SslModeVerifyCa | SslModeVerifyFull
  deriving (Eq, Ord, Show)

-- Unchanged for the developer who sets nothing -- the default off field
-- is invisible in record construction:
postgresEventStoreConfig :: PostgresEventStore
postgresEventStoreConfig =
  def { host = "...", databaseName = "...", user = "...", password = "..." }

-- Opt-in hardening for a production deploy is one extra field:
hardenedConfig :: PostgresEventStore
hardenedConfig =
  def
    { host = "...", databaseName = "...", user = "...", password = "...",
      sslMode = SslModeRequire
    }

-- Or, the env-driven path an operator actually uses (no code change):
--   DB_SSL_MODE=verify-full
--   DB_SSL_ROOT_CERT=/etc/postgresql/azure-roots.pem
```

The `def { host = ..., ... }` construction Jess copy-pastes from the
EventStore docs is byte-for-byte unchanged: `sslMode` defaults to
`SslModeUnset` and `sslRootCert` to `Nothing`. No `sslcert`/`sslkey`
field is added anywhere.

## Consequences

### Positive

1. **Silent downgrade can be forbidden.** Setting `sslMode =
   SslModeRequire` (one field / one env var) makes every nhcore Postgres
   connection refuse to proceed unencrypted, turning a silent plaintext
   leak into a loud connect-time error.
2. **MITM can be defeated.** `SslModeVerifyFull` + a root-CA bundle
   authenticates the server, so an active man-in-the-middle cannot present
   a forged cert.
3. **One knob hardens every connection.** Because it threads through the
   single ADR-0062 builder, the mode applies to all three pools and the
   LISTEN/NOTIFY connections uniformly — it is structurally impossible to
   harden one and forget another.
4. **Dev is untouched.** The default `SslModeUnset` emits no `sslmode`
   param; localhost/CI Postgres connects exactly as before. No contributor
   has to learn TLS to run the suite.
5. **The mTLS footgun is unrepresentable.** No `sslcert`/`sslkey` field
   exists, so nobody can accidentally enable the client-cert auth that
   breaks Azure Flexible Server.
6. **Typos fail clean.** The closed `SslMode` enum makes an invalid mode a
   compile error in code and one clear config error from the env string,
   never a confusing libpq runtime failure.

### Negative

1. **Two optional fields are added to two config records.** A small,
   additive surface increase (`sslMode`, `sslRootCert`). Both default off,
   so existing call sites are unchanged, but the records are slightly
   larger.
2. **One new exported enum.** `SslMode` is new public surface, even though
   the default path never requires touching it. This is the cost of a
   typed mode over a stringly-typed `Maybe Text`.
3. **Operator must supply the right CA bundle for `verify-full`.** The
   code forwards a path; sourcing the DigiCert Global Root G2 + Microsoft
   RSA Root CA 2017 bundle is operator responsibility, documented in the
   deployment guide (WI-6, #685). A wrong/missing bundle fails the
   connection at connect time (loud), not silently.

### Risks

| Risk | Mitigation |
|------|------------|
| Default accidentally ships as `require`, breaking dev/CI. | Default is `SslModeUnset` in both `Default` instances and the env default is `"unset"`; spec test 1 pins that the unset path emits no `sslmode` param and matches the pre-WI-5 output. |
| A client-cert field creeps in and breaks Azure. | No `sslcert`/`sslkey` field exists in the type; spec test 4 asserts no client-cert param is emitted. The footgun is unrepresentable, not merely discouraged. |
| `verify-full` pins an intermediate/server cert that breaks on Microsoft's CA rotation. | The code forwards a `sslrootcert` *path* only and pins nothing; the deployment guide (WI-6) specifies root CAs only (DigiCert Global Root G2 + Microsoft RSA Root CA 2017). |
| A mode token typo reaches libpq. | The mode is the closed `SslMode` enum, not free text; `sslModeToText` is total over the enum and spec test 5 round-trips every constructor. The env parser rejects unknown tokens with one clean config error. |
| One pool is hardened and another forgotten. | Impossible by construction: all pools project into the one `ConnectionParams` and route through the one `toConnectionParams`; FileUpload reuses `PostgresEventStore`. |

### Mitigations

- The default-off contract is encoded in the unit spec (test 1), so a
  future edit that flips the default to a TLS-requiring mode fails CI
  rather than silently breaking every contributor's local Postgres.
- The "no client cert" contract is encoded both in the type (no field to
  set) and in spec test 4, so an Azure-breaking mTLS attempt cannot land.
- The `sslmode` emission is confined to the single seam in
  `toConnectionParams` that ADR-0062 reserved; the existing keepalive and
  `validatePort` asserts guard the rest of the builder across this change.
- Root-only trust and the exact CA bundle are operator guidance carried
  into the deployment guide (WI-6, #685); the code stays cert-agnostic
  (forwards a path), so it survives Microsoft's intermediate-CA rotation.

## References

- [#684: WI-5 — Optional `sslmode` TLS hardening](https://github.com/neohaskell/NeoHaskell/issues/684)
- [#679: Epic — Deploy-readiness on Azure Flexible Server](https://github.com/neohaskell/NeoHaskell/issues/679)
- [ADR-0062: Single Shared Postgres Connection-Settings Builder](0062-shared-connection-settings-builder.md) — reserved the `sslmode` seam this ADR fills; one builder ⇒ all pools covered uniformly.
- [ADR-0060: Explicit Postgres Connection-Pool Budget for Flexible Server B1ms](0060-postgres-pool-budget.md) — the `poolSize` fields the same config records carry; FileUpload shares `PostgresEventStore`.
- [ADR-0037: PostgreSQL LISTEN Connection Keepalive and Reconnection](0037-postgres-listen-keepalive-reconnect.md) — the keepalive params the `sslmode` line sits alongside in `toConnectionParams`.
- [ADR-0016: Redacted Type for Sensitive Data](0016-redacted-type-for-sensitive-data.md) — the secret-hygiene precedent; `sslmode`/`sslrootcert` are public, the existing password field stays the only secret.
- [core/service/Service/Infra/Postgres/ConnectionConfig.hs](../../core/service/Service/Infra/Postgres/ConnectionConfig.hs) — the shared builder; the `-- WI-5 (#684) adds: Param.other "sslmode" "<mode>" here.` comment marks the seam.
- [#644 — outbound-HTTP TLS](https://github.com/neohaskell/NeoHaskell/issues/644) — a separate TLS concern on the HTTP client path, explicitly **not** this work item.
- [#685 — WI-6: Deployment documentation](https://github.com/neohaskell/NeoHaskell/issues/685) — captures the operator-facing `verify-full` CA-bundle guidance (root CAs only).
