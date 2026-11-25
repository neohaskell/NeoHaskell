# Queries

A query is a stored read model derived from entity changes. `EntitiesOf query` identifies its contributing entity types. `QueryOf entity query` defines a row identifier and how an entity contributes to the row.

## Projection declarations

Import `Core` and `Derive`. Define one pair of functions for each source entity. The first chooses the destination row's ID. Multiple source entities can contribute to the same row. The second receives the latest entity and the existing row, and returns `Update row`, `Delete`, or `NoOp`.

```haskell ignore
projectionIdFromCounterEntity :: CounterEntity -> Uuid
projectionIdFromCounterEntity entity = entity.counterId

combineCounterEntity :: CounterEntity -> Maybe CounterView -> QueryAction CounterView
combineCounterEntity entity _ =
  Update CounterView {
    counterId = entity.counterId,
    label = entity.label,
    value = entity.value
  }

derive ''CounterView query { entities = [''CounterEntity] }
```

The declaration generates the projection instances for the listed entities. Place any custom JSON instances before the marker. Existing queries using `deriveQuery` with explicit `QueryOf` instances remain supported.

## Access and consistency

`canAccess` runs before storage access. `canView` filters individual fetched rows. Both functions precede the query marker. Use `authenticatedAccess`, `requirePermission`, or `publicAccess` for query access and `ownerOnly`, `tenantOnly`, or `publicView` for row visibility.

For a model-backed application, declare a source-local Slice with `query @Row`, `readsFrom @Entity`, and each subscribed `onEvent @Payload`. The Event Model registers the query and only the sources declared in that Slice. See [source-local Event Models](event-model.md) for a complete example.

For a legacy application assembled directly with `Application`, `Application.withQuery @Row` remains supported and registers all entities in `EntitiesOf Row`. It is not deprecated.

Projection updates are asynchronous. A successful command response does not prove that its query row has caught up. Retry reads until the expected values appear when a test depends on the projection.

## Choose which routes to publish

By default a query publishes both `Index` and `Lookup`. `Index` lists visible rows at `GET /queries/{name}`. `Lookup` reads one visible row at `GET /queries/{name}/{id}`.

For a mug shop order detail page, put `only [Lookup]` inside the `query @OwnedOrderSummary do` block alongside `readsFrom`. The owner can open the order by UUID. Removing the ID from the URL returns HTTP 404. The published lookup still applies `canAccess` and `canView`.

Use `only [Index]` for a list-only query. Use `only []` to keep projecting rows without publishing either read operation. When assembling an `Application` directly, use `Application.withQueryOnly @OwnedOrderSummary [Application.Lookup]` for the same lookup-only behavior. If several slices register the same query, each must select the same routes. Conflicting selections cause a startup error.

## Pagination and storage

Collection responses contain `items`, `total`, `hasMore`, and `effectiveLimit`. `total` counts rows after authorization filtering. The default limit is 100 and the hard maximum is 1000. Define `maxResults :: Int` before `deriveQuery` to configure the query's maximum.

## Read one row

Use `GET /queries/{kebab-case-name}/{id}` with the UUID selected by `QueryOf.queryId`. This is the stored row key. It does not require a JSON field named `id`.

The response is the row itself, without the collection envelope. The keyed read does not apply collection filters or pagination. It checks `canAccess` before fetching the row and `canView` before returning it. Public queries can accept anonymous reads through `publicAccess`.

A malformed UUID returns HTTP 400. A missing row and a row denied by `canView` return the same HTTP 404 response. Query-level access failures retain HTTP 401 or 403. Storage and response-resolution failures return HTTP 500 without internal error details.

## Storage

Queries use memory storage unless the application configures a query store. Set that store before registering queries. Read [storage](storage.md) for the separate event and query choices.
