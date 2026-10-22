# Queries

A query is a stored read model derived from entity changes. `EntitiesOf query` identifies its contributing entity types. `QueryOf entity query` defines a row identifier and how an entity contributes to the row. See query classes in `core/service/Service/Query/Core.hs`.

## Projection declarations

The CounterView module in `neo/starter/src/Starter/Counter/Queries/CounterView.hs` defines a row with `counterId`, `label`, and `value`. Its `queryId` chooses the counter identifier. Its `combine` replaces the row with the entity's current values.

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

`combine` returns `Update row`, `Delete`, or `NoOp`. Several entity types can contribute to one query type. The CartSummary in `testbed/src/Testbed/Cart/Queries/CartSummary.hs` and query contract in `core/service/Service/Query/Core.hs` show the mapping rules.

## Access and consistency

`canAccess` runs before storage access. `canView` filters individual fetched rows. Both functions precede `deriveQuery`. Use access helpers in `core/service/Service/AccessControl.hs` to state the policy. The endpoint implementation in `core/service/Service/Query/Endpoint.hs` applies these checks.

`Application.withQuery @Row` registers the query and its entity subscriptions. On Web transport, the generated route is `GET /queries/{kebab-case-name}`. See query registration in `core/service/Service/Application.hs` and Web transport in `core/service/Service/Transport/Web.hs`.

Projection updates are asynchronous. A successful command response does not prove that its query row has caught up. The Counter flow in `neo/starter/tests/scenarios/counter-flow.hurl` retries reads until it observes the expected values.

## Pagination and storage

Query responses contain `items`, `total`, `hasMore`, and `effectiveLimit`. `total` counts rows after authorization filtering. The default limit is 100 and the hard maximum is 1000. Define `maxResults :: Int` before `deriveQuery` to configure the query's maximum. See pagination in `core/service/Service/Query/Pagination.hs`, query defaults in `core/service/Service/Query/Core.hs`, and the marker in `core/service/Service/Query/TH.hs`.

Queries use memory storage unless the application configures a query store. Set that store before registering queries. Read [storage](storage.md) for the separate event and query choices.
