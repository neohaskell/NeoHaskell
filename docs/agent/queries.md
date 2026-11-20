# Queries

A query is a stored read model derived from entity changes. `EntitiesOf query` identifies its contributing entity types. `QueryOf entity query` defines a row identifier and how an entity contributes to the row.

## Projection declarations

In this example, `CounterView` has `counterId`, `label`, and `value` fields. `queryId` chooses the counter identifier. `combine` replaces the row with the entity's current values.

This fragment needs the row declaration, imports, and access functions.

```haskell ignore
deriveQuery ''CounterView [''CounterEntity]

instance QueryOf CounterEntity CounterView where
	queryId entity = entity.counterId
	combine entity _existing =
		Update CounterView
			{ counterId = entity.counterId
			, label = entity.label
			, value = entity.value
			}
```

`combine` returns `Update row`, `Delete`, or `NoOp`. Several entity types can contribute to one query type. Each `QueryOf entity query` instance defines how its entity changes that row.

`deriveQuery` supplies missing `Show`, `Generic`, `Json.FromJSON`, `Json.ToJSON`, and `ToSchema` instances. Define a custom instance before the marker.

## Access and consistency

`canAccess` runs before storage access. `canView` filters individual fetched rows. Both functions precede `deriveQuery`. Use `authenticatedAccess`, `requirePermission`, or `publicAccess` for query access and `ownerOnly`, `tenantOnly`, or `publicView` for row visibility.

For a model-backed application, declare a source-local Slice with `query @Row`, `readsFrom @Entity`, and each subscribed `onEvent @Payload`. The Event Model registers the query and only the sources declared in that Slice. See [source-local Event Models](event-model.md) for a complete example.

For a legacy application assembled directly with `Application`, `Application.withQuery @Row` remains supported and registers all entities in `EntitiesOf Row`. It is not deprecated. On Web transport, registered queries provide `GET /queries/{kebab-case-name}` and `GET /queries/{kebab-case-name}/{id}`.

Projection updates are asynchronous. A successful command response does not prove that its query row has caught up. Retry reads until the expected values appear when a test depends on the projection.

## Pagination and storage

Collection responses contain `items`, `total`, `hasMore`, and `effectiveLimit`. `total` counts rows after authorization filtering. The default limit is 100 and the hard maximum is 1000. Define `maxResults :: Int` before `deriveQuery` to configure the query's maximum.

## Read one row

Use `GET /queries/{kebab-case-name}/{id}` with the UUID selected by `QueryOf.queryId`. This is the stored row key. It does not require a JSON field named `id`.

The response is the row itself, without the collection envelope. The keyed read does not apply collection filters or pagination. It checks `canAccess` before fetching the row and `canView` before returning it. Public queries can accept anonymous reads through `publicAccess`.

A malformed UUID returns HTTP 400. A missing row and a row denied by `canView` return the same HTTP 404 response. Query-level access failures retain HTTP 401 or 403. Storage and response-resolution failures return HTTP 500 without internal error details.

Registering a query still publishes its collection endpoint. A keyed endpoint alone does not restrict collection access.

## Storage

Queries use memory storage unless the application configures a query store. Set that store before registering queries. Read [storage](storage.md) for the separate event and query choices.
