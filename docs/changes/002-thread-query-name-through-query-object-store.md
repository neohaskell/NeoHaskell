# Change 002: Thread the real query name through the QueryObjectStore so multiple queries per entity stop colliding

`Service.QueryObjectStore.Postgres` persists **every** query's state under one
hardcoded `query_name` (`traitNamespace = "__trait__"`), keyed only by
`instance_uuid`. The ADR-0059 schema already has the composite primary key
`(query_name, instance_uuid)` for exactly this case, but the trait's
`get` / `atomicUpdate` / `getAll` never thread the real query name — they always
pass the `"__trait__"` sentinel. Any service with **more than one query
projecting the same entity** (the ordinary CQRS case) therefore shares the row
`("__trait__", <entityId>)` across queries: writes overwrite each other, reads
deserialize another query's JSON (`SerializationError`), and the affected
queries reach terminal `Query rebuild failed`. This change threads the query
name — already known at `createDefinitionWithStore` as `NameOf query` — into the
store factory so each query's rows are keyed by its own `query_name`, exactly as
the existing PK intends.

```yaml spec
issue: issue#734                # issue#NNN or adhoc:<slug>
kind: bug                       # feature | bug | refactor
touches: [queries, service-wiring]  # capability IDs from codemap/capabilities.yaml (closed list)
breaking: true                  # MUST be true if the contract delta has any `-` line
new-dependency: false           # any new build-depends / flake input
new-capability: false           # this change adds a row to codemap/capabilities.yaml
new-extension-point: false      # this change adds a row to codemap/extension-points.yaml
```

## Contract delta

The store factory gains the query name as a required input, so a store can never
again be built blind to which query it serves. The factory type carried by
`createDefinitionWithStore` becomes `Text -> Task Text (QueryObjectStore query)`,
the `QueryObjectStoreConfig` typeclass method and the Postgres `newFromConfig`
constructor each gain a `Text` query-name parameter, and the Postgres trait
implementations thread it (deleting the `traitNamespace` sentinel). `NameOf
query` is already resolved to `Text` inside `createDefinitionWithStore`, so the
name flows from the one place that structurally knows it.

```diff signatures
- Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Task Text (QueryObjectStore query)
+ Service.QueryObjectStore.Core: createQueryObjectStore :: (QueryObjectStoreConfig config, FromJSON query, ToJSON query) => config -> Text -> Task Text (QueryObjectStore query)
- Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Task QueryObjectStoreError (QueryObjectStore query)
+ Service.QueryObjectStore.Postgres: newFromConfig :: (FromJSON query, ToJSON query) => PostgresQueryObjectStoreConfig -> Text -> Task QueryObjectStoreError (QueryObjectStore query)
- Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => Task Text (QueryObjectStore query) -> QueryDefinition
+ Service.Query.Definition: createDefinitionWithStore :: forall query (queryName :: Symbol) (entities :: [Type]). (Query query, ToSchema query, ToJSON query, FromJSON query, queryName ~ NameOf query, entities ~ EntitiesOf query, KnownSymbol queryName, WireEntities entities query) => (Text -> Task Text (QueryObjectStore query)) -> QueryDefinition
```

## Criteria

`kind: bug` → C1 is the failing reproduction test, committed red in the draft
PR. All three cross the real Postgres boundary (rows in `query_object_store`
keyed by `(query_name, instance_uuid)`), so they are `integration` and self-gate
on `POSTGRES_AVAILABLE=true`.

| ID | Behavior | Proving test | Level |
|----|----------|--------------|-------|
| C1 | Two Postgres stores built with **distinct** query names, writing the **same** instance UUID with different state, each read back their **own** state — no cross-query overwrite (the collision the issue reports) | `Service.QueryObjectStore.PostgresSpec` "distinct query names isolate rows for the same instance uuid (#734)" | integration |
| C2 | `getAll` on a store returns only the rows written under **its own** query name, not rows a different query wrote for the same table | `Service.QueryObjectStore.PostgresSpec` "getAll is scoped to the store's own query name (#734)" | integration |
| C3 | Single-query behavior preserved — `get` / `atomicUpdate` (insert, overwrite, delete) / `getAll` still round-trip correctly once state is keyed by the threaded query name | `Service.QueryObjectStore.PostgresSpec` "state round-trips under the threaded query name" | integration |

## User impact

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

## ADR

Design decision recorded in
[ADR-0070](../decisions/0070-thread-query-name-through-query-object-store.md):
why the query name is threaded as an explicit factory input (making an unnamed
store unrepresentable) rather than derived via a `KnownSymbol (NameOf query)`
constraint on the store config typeclass — the latter would couple the generic
object store to the Query type machinery and risk an import cycle, while the
former keeps the store a plain data sink and structurally prevents the
"forgot to thread the name" defect class from recurring in a future backend.
