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

### 003-thread-query-name-through-query-object-store — Change 003: Thread the real query name through the QueryObjectStore so multiple queries per entity stop colliding  **[BREAKING]**

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
