# ADR-0070: Thread the query name through the QueryObjectStore factory

> Fixes #734 — a QueryObjectStore.Postgres data-integrity bug. Builds on
> [ADR-0059](0059-async-query-rebuild-with-persistent-checkpoints.md) (the
> persistent store and its `(query_name, instance_uuid)` primary key) and
> [ADR-0007](0007-queries-read-models.md) (the read-side / query model).

## Status

Accepted

## Context

ADR-0059 gave `query_object_store` the composite primary key
`(query_name, instance_uuid)` precisely so that many queries can share one
Postgres table while each query's per-instance state stays isolated. The trait
that reads and writes that table — `Service.QueryObjectStore.Core.QueryObjectStore`,
with `get` / `atomicUpdate` / `getAll` — predates the checkpoint work and its
method signatures take only an instance `Uuid`. There is no `query_name`
parameter anywhere on the trait.

The Postgres implementation bridged that gap with a sentinel: every trait row is
written and read under a fixed `query_name = "__trait__"` (`traitNamespace`). The
in-code comment named this an unfinished impedance bridge — "extending the trait
to thread `query_name` through every method is a larger refactor." The
consequence is a correctness bug, not a cosmetic one: **any service with two or
more queries projecting the same entity** collapses all of their state onto the
single row `("__trait__", <entityId>)`. Queries overwrite each other, each read
deserializes a different query's JSON (`SerializationError`), and the affected
queries reach terminal `Query rebuild failed`. Until it is fixed, the Postgres
`withQueryObjectStore` backend is unsafe for the ordinary multi-query CQRS case —
those services must stay on the in-memory store and pay a full rebuild from
`StreamPosition 0` on every restart, the exact cost ADR-0059 set out to remove.

The query name is not missing information — it is `NameOf query`, and it is
already resolved to `Text` inside `Service.Query.Definition.createDefinitionWithStore`
(as `queryNameText`, used to wire entities and the HTTP endpoint). The only
question is *how* to route that already-known name to the point where the store's
operation closures are built.

## Decision

**Make the query name a required input to the store factory.** The factory that
`createDefinitionWithStore` carries changes from `Task Text (QueryObjectStore query)`
to `Text -> Task Text (QueryObjectStore query)`; `createDefinitionWithStore`
applies the `queryNameText` it already computes. The `QueryObjectStoreConfig`
typeclass method `createQueryObjectStore` and the Postgres `newFromConfig`
constructor each gain a trailing `Text` query-name parameter, and the Postgres
`getImpl` / `atomicUpdateImpl` / `getAllImpl` thread that name into their
`query_name` column instead of `traitNamespace`. The `traitNamespace` sentinel
and its explanatory comment are deleted. The in-memory backend ignores the name
(each `InMemory.new` already allocates an independent `ConcurrentVar` map, so its
stores never shared a namespace to begin with).

### Alternative considered and rejected

Derive the name inside `createQueryObjectStore` from a
`KnownSymbol (NameOf query)` constraint added to the typeclass method, leaving
the factory type `Task Text (QueryObjectStore query)` unchanged (a smaller
diff). Rejected because it couples the *generic* object store — today a plain
JSON data sink parameterized over any `query` with `FromJSON`/`ToJSON` — to the
`Query`/`NameOf` type machinery in `Service.Command.Core`, introducing a new
module dependency (and a plausible import cycle: `QueryObjectStore.Core` would
import the command core). It also leaves the name *implicit*: a future backend
author could still write a store that silently drops it. Threading the name as
an explicit factory input makes an unnamed store **unrepresentable** — the type
forces the caller to supply the identity — which is the property that structurally
prevents this exact "forgot to thread the name" defect class from recurring.

## Consequences

**Positive.**
- The `(query_name, instance_uuid)` primary key does the isolation job it was
  designed for; multiple queries per entity persist independently on Postgres.
- The Postgres `withQueryObjectStore` backend becomes safe for real multi-query
  services, unlocking the persistent-checkpoint benefit ADR-0059 targeted.
- The store factory's type now states that a store serves a specific named
  query, so the bug class cannot silently return.

**Negative / cost.**
- Source-level breaking change to three exported signatures
  (`createDefinitionWithStore`, `createQueryObjectStore`, `newFromConfig`). The
  two in-repo callers are updated in the same change; external custom-backend
  callers adapt their factory shape, guided by the generated changelog migration
  note.
- No data migration is performed for rows already written under `"__trait__"` by
  a prior deployment. Such rows were, by definition, already cross-contaminated
  (that is the bug); the correct recovery is a query rebuild, which repopulates
  rows under the real query names. Automatic cleanup of legacy `"__trait__"` rows
  is out of scope here and can be a follow-up if any deployment needs it.
