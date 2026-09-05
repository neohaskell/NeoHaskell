# Changelog

Entries are **generated from contract-delta specs** (`docs/changes/*.md`) by
`./dev changelog` — do not hand-write them; regenerate instead. A change is
**breaking** iff its spec's `diff signatures` delta removes or changes a
signature line; a breaking entry carries a mandatory migration note (from the
spec's `## User impact`). CI gate: `changelog --check` in `.github/workflows/checks.yml`.

**Release promotion:** at release time, rename the `## [Unreleased]` heading to
`## [X.Y.Z] — YYYY-MM-DD` and add a fresh empty `## [Unreleased]` above it; a
breaking entry in the section forces a major/minor bump per semver. (No release
has been cut yet — everything accrues under Unreleased until the first tag.)

## [Unreleased]

### 008-harden-pipeline-evidence-and-regression-smoke — Change 008: Harden pipeline evidence and regression smoke

No public application API breaks. Contributors get earlier, deterministic failures instead of false-green specs, red tests, stale localization data, or incomparable timing claims. The new benchmark is local/nightly rather than PR-blocking because shared-runner wall-clock noise is not deterministic.

API delta:

- `+ Test.Service.Command.Core: AddItemToCartAfterItemCount :: Uuid -> Uuid -> Int -> AddItemToCartAfterItemCount`
- `+ Test.Service.Command.Core: data AddItemToCartAfterItemCount`
- `+ Test.Service.EventStore.Regression: assertBehavior :: Text -> Bool -> Task Text Unit`
- `+ Test.Service.EventStore.Regression: awaitInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: barrierBeforeInsert :: InsertBarrier -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: data InsertBarrier`
- `+ Test.Service.EventStore.Regression: failFirstWithConsistencyConflict :: EventStore event -> Task error (EventStore event)`
- `+ Test.Service.EventStore.Regression: knownBad :: Text -> Text -> Task error Bool`
- `+ Test.Service.EventStore.Regression: newInsertBarrier :: Task error InsertBarrier`
- `+ Test.Service.EventStore.Regression: recordFetchedRevisions :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (Maybe StreamPosition)))`
- `+ Test.Service.EventStore.Regression: recordFetches :: forall k state (event :: k) error. EntityFetcher state event -> Task error (EntityFetcher state event, Task error (Array (EntityFetchResult state)))`
- `+ Test.Service.EventStore.Regression: recordInsertions :: EventStore event -> Task error (EventStore event, Task error (Array (InsertionPayload event)))`
- `+ Test.Service.EventStore.Regression: releaseInsertions :: Int -> InsertBarrier -> Task error Unit`
- `+ Test.Service.EventStore.Regression: requireInsertionType :: InsertionType -> EventStore event -> EventStore event`
- `+ Test.Service.EventStore.Regression: seedStream :: EventStore event -> EntityName -> StreamId -> Array event -> Task Text Unit`

### 007-make-cold-start-health-constant-time — Change 007: Make cold-start health constant-time without losing replayed events

**Runtime:** container liveness no longer scales with event-store size. Operators
can probe `/health` for process liveness and `/ready` for traffic readiness
without widening a grace period as the store grows. Replay remains ordered and
complete when live events overlap startup; fixing the bind delay must not trade
a loud crash-loop for silent projection loss.

**Performance:** rebuild changes from one full-log pass per query plus
per-operation pool construction to one paged, entity-filtered pass over a
reused Postgres pool. Entity snapshots prevent repeated full-stream fetches.
Progress is visible through the ADR-0059 field names rather than a silent
multi-minute gap.

**Public Haskell surface:** additive only. `Service.Query.Registry` exposes
`registeredEntityNames :: QueryRegistry -> Array EntityName`; existing callers
need no migration.

**CI:** `.github/workflows/test.yml` exports `POSTGRES_AVAILABLE=true` for the
Postgres-backed suites so the concurrency and pool regressions execute on every
substantive PR.

**Deployment documentation:** restore the deployment guide and lead with a
`startupProbe`/readiness configuration, including explicit
`periodSeconds × failureThreshold` arithmetic and the distinction between
`/health` and `/ready`.

**Deliberately deferred:** production checkpoint-store wiring and query-state
migration remain tracked by #854/#855/#666. SIGTERM cancellation remains #662;
outbound-integration recovery is #856; the missing `X-Query-Status` contract is
#664; Neon scale-to-zero support is #857. None is required to make port binding
constant-time and in-process replay/live overlap gap-free.

API delta:

- `+ Service.Query.Registry: registeredEntityNames :: QueryRegistry -> Array EntityName`

### 006-deterministic-uuid-v5 — Change 006: Add deterministic UUID v5 generation to `Uuid` and the `Decision` monad

**Not breaking.** Three added signatures, no removals, no behavior change to any
existing function. `Decision`'s constructor set is unchanged, so `runDecision`
and the command executor are untouched — existing commands cannot regress.

**New capability for users.** Natural-key entity identity becomes expressible in
the framework:

```haskell
getEntityId :: RegisterProject -> Maybe Uuid
getEntityId command =
  Uuid.generateV5 projectNamespace command.repoPath |> Just
```

so the same normalized path always routes to the same stream, and `decide` sees
`Just entity` on a repeat and can `Decider.reject "already registered"`. Inside
`decide`, other derived ids come from
`Decider.generateDeterministicUuid namespace key`, which reads alongside
`Decider.generateUuid` without a `pure`/`Task.yield` lift (which the dialect
hook bans at the call site anyway).

**Security constraint, carried in the haddock of both functions.** A v5 UUID is
**not** a secret — anyone who knows the namespace and the name reproduces it
exactly, and names drawn from a small space can be brute-forced from a known
namespace. It must never be used for capability tokens, session ids,
password-reset components, or anything else that is unguessable by design;
`Uuid.generate` (random v4) stays the correct choice there. The division of
labour is now explicit: *random for secrets and fresh identity, deterministic for
derived identity.*

**No default namespace.** The issue floated a `uuidFromText :: Text -> Decision Uuid`
convenience with a built-in namespace; it is deliberately not shipped. A
framework-wide default namespace is a global collision domain, and since the
whole premise of the primitive is that the same input *is* the same stream, such
a collision is a data-integrity bug across unrelated aggregates. Callers build a
namespace with `Uuid.fromText`. RFC 4122's predefined namespaces (DNS/URL/OID/X500)
are likewise not re-exported yet — rule of three, and no consumer has asked.

**Testbed effect.** A new demo command `Testbed.Cart.Commands.RegisterCartByKey`
(registered in `Testbed.Cart.Service`) exercises the feature end-to-end, per the
`new-command-machinery` extension point's rule that framework write-side features
get a demo command plus hurl coverage. It is additive: no existing testbed
command, query, or hurl file changes, and no existing expectation is touched.

**Relationship to the abandoned `feat/uuid-v5-decision` branch.** The issue asks
that the pre-existing branch be merged. It is not merged as-is: alongside the
~130 relevant lines it carries ~5000 lines of unrelated tooling
(`.atomicorch/**`, `docs/designs/**`), duplicate ADR trees (`docs/adr/` and
`docs/decisions/`), and an ADR numbered **0055**, which `main` has since assigned
to *declarative integrations with fakes*. Its `Uuid.generateV5` also reaches for
`Data.Text.Encoding` and `Data.ByteString` directly rather than the dialect's
`Text.toBytes`. This change re-lands the wanted API on a clean branch through the
spec gate, with the ADR renumbered to 0073 and the criteria above; the old branch
should be closed rather than merged.

API delta:

- `+ Uuid: generateV5 :: Uuid -> Text -> Uuid`
- `+ Bytes: unpack :: Bytes -> [Word8]`
- `+ Decider: generateDeterministicUuid :: Uuid -> Text -> Decision Uuid`

### 005-thread-query-name-through-query-object-store — Change 005: Thread the real query name through the QueryObjectStore so multiple queries per entity stop colliding  **[BREAKING]**

**Breaking (source-level, in-repo callers updated in this PR).** Three exported
signatures gain a query-name input:

- `createDefinitionWithStore` — its store-factory argument becomes
  `Text -> Task Text (QueryObjectStore query)`. Migration: a factory `f` that
  ignored the name becomes `\_ -> f`; a factory that needs it receives the
  query's `NameOf` as `Text`.
- `QueryObjectStore.Core.createQueryObjectStore` / `Postgres.newFromConfig` —
  each takes the query name as a trailing `Text`. Migration: pass the query
  name (`newFromConfig cfg "my-query"`).

The two in-repo callers — `Application.withQuery` (config-backed path) and
`Definition.createDefinition` (in-memory convenience) — are updated here.
External services that call `createDefinitionWithStore` with a **custom** store
factory must adapt the factory shape; the changelog carries the migration note
(generated from the removed signature lines).

**Runtime behavior.** Before: any app with ≥2 queries over one entity, running
on the Postgres QueryObjectStore, corrupts state across queries and fails their
rebuilds — `withQueryObjectStore` with a Postgres config is unusable for
real multi-query services. After: each query's rows are keyed by its own name,
so distinct queries over the same entity persist independently. In-memory stores
are unaffected — each `InMemory.new` already allocates an independent map, so its
`createQueryObjectStore` ignores the name.

**Testbed:** no acceptance-test change — the collision needs a Postgres backend
with ≥2 queries per entity, which the default testbed app does not wire; covered
at the integration level. The existing `PostgresSpec` single-store tests keep
running (their `mkStore` helper supplies a fixed default query name), so the
refactor's regression surface stays green.

**Wiring coverage (review follow-up):** the store-level isolation criteria
(C1/C2) prove that *manually named* Postgres stores do not collide, but not that
the application actually threads the right name into the store. C4 closes that
gap: it drives `createDefinitionWithStore` with a spy store factory and asserts
the factory is handed `NameOf query`, not the `"__trait__"` sentinel — a fast
`unit` test (no Postgres) that exercises the exact wiring hop the fix adds.

**Checkpoint coexistence (implementation follow-up, C5):** before the fix the
trait's per-instance rows lived under `"__trait__"` while the checkpoint marker
(`Subscriber.rebuildFrom` via `CheckpointStore`, keyed by the reserved nil UUID)
lived under the real `query_name` — two different partitions. Threading the real
name unifies them under one `query_name`, isolated only by `instance_uuid` (real
vs nil). `get`/`atomicUpdate` are unaffected (they use real instance UUIDs), and
`resumeFromCheckpoint`/`deleteStaleHash` stay correct (they filter by
`query_hash`, which trait rows leave empty; `deleteStaleHash` only fires before a
full replay-from-0 that rebuilds those rows). The one place that needed a guard
is `getAll`, which now excludes the nil-UUID marker so a checkpointed query's
`GET /queries/{name}` never surfaces the marker's placeholder state — proven by
C5.

API delta:

- `- Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Task Text (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Text -> Task Text (QueryObjectStore query)`
- `- Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore query)`
- `+ Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Text -> Task QueryObjectStoreError (QueryObjectStore query)`
- `- Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => Task Text (QueryObjectStore query) -> QueryDefinition`
- `+ Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => (Text -> Task Text (QueryObjectStore query)) -> QueryDefinition`

### 004-crypto-hmac-sign-verify — Change 004: Add Crypto module with HMAC-SHA256 signWith/verifyWith

None breaking. New public module `Crypto` and new `Bytes.getRandom`
primitive (secure random bytes, mirroring `Int.getRandom`); no existing
signatures change. The `Bytes` newtype now lives in the hidden internal
module `Bytes.Internal` purely to break an import cycle; it is not
importable by applications. The public `Bytes` API is unchanged (`Bytes
(..)` is re-exported as before). `Auth.OAuth2.StateToken` keeps its private
`HmacKey` for now — migrating it onto `Crypto.HmacKey` is a possible
follow-up refactor, deliberately out of scope here. Signature wire format
is lowercase hex (the common webhook header convention, e.g. GitHub/Stripe
style); `verifyWith` is case-insensitive on input.

API delta:

- `+ Crypto: data HmacKey`
- `+ Crypto: hmacKeyFromText :: Text -> Result Text HmacKey`
- `+ Crypto: hmacKeyFromBytes :: Bytes -> Result Text HmacKey`
- `+ Crypto: generateHmacKey :: Task err HmacKey`
- `+ Crypto: signWith :: HmacKey -> Bytes -> Text`
- `+ Crypto: verifyWith :: HmacKey -> Text -> Bytes -> Bool`
- `+ Bytes: getRandom :: Int -> Task w Bytes`

### 003-maintainer-codemap-regeneration — Change 003: Maintainer-triggered codemap regeneration onto a contributor PR

Not breaking. No public signature or wire-format change. New capability for
maintainers only: a manually-dispatched workflow on `main`. Contributors see
their PR branch receive one `chore: regenerate codemap` fast-forward commit after
Nick approves the protected environment; a no-op (codemap already current) leaves
the branch untouched and the run succeeds. Every unsupported or unsafe condition
(maintainer edits disabled, org-owned fork, metadata race, symlink under
`codemap/`, out-of-allowlist manifest/diff, non-fast-forward) fails with an
actionable Actions summary and mutates nothing — **no fallback PR is ever
created**. Testbed: no acceptance-test change — this is CI/tooling with no
HTTP-observable behavior. One-time maintainer setup is **mandatory and
load-bearing**: the `codemap-publish` Environment with **required reviewer Nick**
AND **deployment branches = `main` only**, plus the `CODEMAP_PUBLISH_TOKEN` secret
— a maintainer classic `public_repo` PAT (broad public-repo blast radius
documented; dedicated bot identity recommended; expiry ≤90d; revoke-on-exposure)
— documented in ADR-0070 and the workflow header. Without any of these the
workflow fails closed at `publish`.

### 002-task-control-flow-dialect-rules — Change 002: Enforce Task control-flow dialect — `|> discard`, `Task.when`, `Task.unless`

Not breaking. No public signature or wire-format change — the migrated `if …
pass` blocks and their `Task.when`/`Task.unless` replacements are behaviourally
identical (`Task.when c a` runs `a` iff `c`, `Task.unless c a` runs `a` iff not
`c`, each otherwise doing nothing — exactly like the `if`/`pass` forms). New Task
code is nudged toward the dialect idioms at edit
time (rule 1) and at `./dev lint`/CI (rules 2–3). Existing non-dialect parser
and `Q`-monad code is deliberately preserved via added-lines grandfathering and
a scoped ignore. Testbed: no acceptance-test change — this is a source-dialect
and tooling change with no HTTP-observable behaviour.

### 001-fileupload-dedup-blob-existence-check — Change 001: Verify the blob still exists before returning a dedup match on file upload

Not breaking. No signature or wire-format change; `UploadResponse` still omits
`blobKey` from JSON. Behavior only changes on the failure path: an upload that
previously returned a reference to a missing blob (poisoning the content hash
forever) now re-stores the content and returns a valid reference. The re-stored
bytes are the caller's own uploaded content, matched by the same owner-scoped
content hash, so there is no cross-owner exposure. Testbed: no acceptance-test
change — blob loss cannot be induced over HTTP; covered at the integration
level.

Side effect of making the reproduction executable: `ContentDedupSpec` is listed
in the cabal `other-modules` but was never registered in
`core/test-service/Main.hs`, so its dedup coverage compiled but never ran. This
change registers it, so the regression tests **and** the existing dedup suite
now execute.
