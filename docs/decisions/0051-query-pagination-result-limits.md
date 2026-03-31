# ADR-0051: Query Pagination and Result Limits

## Status

Proposed

## Context

### Current State

NeoHaskell's query endpoints return all matching results without limits. The `createQueryEndpoint` function in `Service.Query.Endpoint` calls `queryStore.getAll`, retrieves every stored query instance, applies authorization filtering and optional NeoQL filtering, then encodes the entire array as JSON:

```haskell
createQueryEndpoint queryStore userClaims maybeExpr = do
  case canAccessImpl @query userClaims of
    Just authErr -> Task.throw (Auth.AuthorizationError authErr)
    Nothing -> do
      allQueries <- queryStore.getAll |> Task.mapError storeErrorToEndpointError
      let authorizedQueries = allQueries |> Array.takeIf (...)
      let filteredQueries = case maybeExpr of ...
      let responseText = filteredQueries |> Json.encodeText
      Task.yield responseText
```

This creates three problems:

1. **Memory exhaustion**: A query returning thousands of results loads all of them into memory, serializes them to JSON, and sends them over the wire. A service with 100k query instances could exhaust the server's heap.

2. **Network saturation**: Large response payloads consume bandwidth and increase latency for all clients. A single unbounded query can degrade the experience for other users.

3. **Denial-of-service vector**: An attacker (or an accidental bug in a client) can issue unbounded queries repeatedly, amplifying resource consumption without rate limits on result size.

### Use Cases

- **Dashboard pagination**: Jess builds an admin dashboard that shows a paginated table of orders. She needs to request page 2 of 25 results, and know if there are more pages.

- **Mobile infinite scroll**: A mobile client fetches the first 20 items, then fetches the next 20 when the user scrolls down. The client needs `hasMore` to know when to stop requesting.

- **Production safety**: A service with 50k query instances needs a hard cap on response payloads. Note: the server still loads all query instances into memory via `getAll` before slicing — this ADR limits the *response payload*, not the in-memory working set. Database-level pagination (`QueryObjectStore.getPage`) is deferred to a future ADR.

### Design Goals

1. **Opt-in for Jess, safe by default**: Pagination works out of the box with sensible defaults. Jess doesn't need to think about it unless she wants custom limits.

2. **Graceful degradation**: Clients that don't send pagination parameters receive results up to the default limit. However, the response format change from bare array to `QueryPageResponse` object is a breaking change for existing API consumers (acceptable pre-1.0).

3. **Per-query configurability**: Each query type can declare its own maximum result limit. A lightweight `CartSummary` might allow 1000 results; a heavy `AuditLog` might cap at 50.

4. **Minimal API surface**: Add the fewest new types and functions needed. Don't introduce cursor-based pagination, keyset pagination, or other advanced patterns yet — offset/limit is sufficient for the current use cases.

### GitHub Issue

- [#265: Add pagination and result limits to Query endpoints](https://github.com/neohaskell/NeoHaskell/issues/265)

## Decision

### 1. Pagination Types

Add pagination request/response types to a new module `Service.Query.Pagination`.

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Types in `Service.Query.Auth` | Rejected | Auth module handles authorization concerns, not response shaping. Mixing pagination types there violates single responsibility. |
| Types in `Service.Query.Endpoint` | Rejected | Endpoint is the handler module. Types should be in their own module for reuse and clarity. |
| New `Service.Query.Pagination` module | **Chosen** | Clean separation. The module holds `QueryPageRequest`, `QueryPageResponse`, and pagination constants. Follows the pattern of `Service.Query.Auth` holding auth types separately. |

Type names carry the `Query` prefix to match the namespace convention (`QueryAction`, `QueryAuthError`, `QueryEndpointError`, `QueryObjectStore`, `QueryOf`). This ensures Jess finds them when searching for "Query" across the codebase.

```haskell
-- | Pagination request parameters.
data QueryPageRequest = QueryPageRequest
  { limit :: !Int    -- ^ Max items per page
  , offset :: !Int   -- ^ Starting position
  }
  deriving (Eq, Show, Generic)

-- | Paginated response wrapper.
data QueryPageResponse a = QueryPageResponse
  { items :: !(Array a)       -- ^ Items in this page
  , total :: !Int             -- ^ Total count of matching items (AFTER authorization filtering)
  , hasMore :: !Bool          -- ^ Whether more items exist after this page
  , effectiveLimit :: !Int    -- ^ Actual limit applied (may differ from requested if capped)
  }
  deriving (Eq, Show, Generic)
```

The `effectiveLimit` field surfaces any silent capping to the client. When `limit=5000` is capped to 1000, the client sees `effectiveLimit: 1000` and can detect the difference. This eliminates a class of debugging confusion where Jess requests a large page and silently receives fewer items than expected.

### 2. Default Limits and Constants

```haskell
-- | Default page size when no limit is specified.
defaultLimit :: Int
defaultLimit = 100

-- | Absolute maximum page size (hard cap).
absoluteMaxLimit :: Int
absoluteMaxLimit = 1000
```

If a client sends `limit=5000`, the server caps to the query's `maxResults` (or `absoluteMaxLimit`). No error is returned — the capping is surfaced via the `effectiveLimit` field in `QueryPageResponse` so clients can detect and adapt.

### 3. Query Typeclass Extension

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Add `maxResultsImpl` to `Query` typeclass | Rejected | Modifies a core typeclass, adding a method that not all queries need to customize. Also requires updating `deriveQuery` TH macro. |
| Separate `PaginatedQuery` typeclass | Rejected | Over-engineering — adds a new typeclass for a single default method. |
| Default function with per-query override via `maxResults` | **Chosen** | Add a `maxResultsImpl` method to the `Query` typeclass with a default implementation returning `absoluteMaxLimit`. Queries that need tighter limits override it. The TH macro picks up a `maxResults` function if defined. |

```haskell
class Query query where
  canAccessImpl :: Maybe UserClaims -> Maybe QueryAuthError
  canViewImpl :: Maybe UserClaims -> query -> Maybe QueryAuthError
  -- | Maximum results this query type will return in a single page.
  -- Default: 1000 (absoluteMaxLimit). Override for tighter limits.
  maxResultsImpl :: Int
  maxResultsImpl = absoluteMaxLimit
```

The `deriveQuery` TH macro will be updated to wire a `maxResults :: Int` function (if defined in the query module) to `maxResultsImpl`. If no `maxResults` function exists, the default applies.

### 4. Endpoint Changes

The `createQueryEndpoint` function signature changes to accept `QueryPageRequest`:

```haskell
createQueryEndpoint ::
  forall query.
  ( Json.ToJSON query, Query query ) =>
  QueryObjectStore query ->
  Maybe UserClaims ->
  Maybe Expr ->
  QueryPageRequest ->
  Task QueryEndpointError Text
```

The implementation applies pagination after authorization and NeoQL filtering:

```haskell
createQueryEndpoint queryStore userClaims maybeExpr pageRequest = do
  case canAccessImpl @query userClaims of
    Just authErr -> Task.throw (Auth.AuthorizationError authErr)
    Nothing -> do
      allQueries <- queryStore.getAll |> Task.mapError storeErrorToEndpointError
      let authorizedQueries = allQueries |> Array.takeIf (...)
      let filteredQueries = case maybeExpr of ...
      -- SECURITY: total must be computed AFTER canViewImpl filtering.
      -- Exposing pre-auth count leaks record existence to unauthorized users.
      let total = Array.length filteredQueries
      let effectiveLimit = min pageRequest.limit (maxResultsImpl @query)
      let pagedItems =
            filteredQueries
              |> Array.drop pageRequest.offset
              |> Array.take effectiveLimit
      let hasMore = pageRequest.offset + effectiveLimit < total
      let response = QueryPageResponse
            { items = pagedItems
            , total = total
            , hasMore = hasMore
            , effectiveLimit = effectiveLimit
            }
      Task.yield (Json.encodeText response)
```

### 5. Web Transport Query Parameter Parsing

The web transport parses `?limit=N&offset=M` from query parameters alongside the existing `?q=` NeoQL parameter:

```haskell
let limitParam = getQueryParam "limit"
      |> Maybe.andThen Text.toInt
      |> Maybe.withDefault defaultLimit
let offsetParam = getQueryParam "offset"
      |> Maybe.andThen Text.toInt
      |> Maybe.withDefault 0
let pageRequest = QueryPageRequest
      { limit = max 1 (min limitParam absoluteMaxLimit)
      , offset = max 0 (min offsetParam 10_000_000)
      }
```

Invalid values (negative, non-numeric) fall back to defaults. This follows the established pattern of graceful degradation used by the NeoQL `?q=` parameter.

### 6. Response Format Change

The endpoint response changes from a bare JSON array to a `QueryPageResponse` object:

**Before:**

```json
[{"cartSummaryId": "...", "itemCount": 3}, ...]
```

**After:**

```json
{
  "items": [{"cartSummaryId": "...", "itemCount": 3}, ...],
  "total": 142,
  "hasMore": true,
  "effectiveLimit": 100
}
```

This is a **breaking change** for API consumers. However, NeoHaskell is pre-1.0 and the query API is explicitly unstable. The `QueryPageResponse` wrapper provides essential metadata that clients need for any production use.

The `total` field in the response body provides the count. An `X-Total-Count` response header may be added in a future iteration for REST convention compliance.

### 7. Module Placement

| File | Purpose |
|------|---------|
| `core/service/Service/Query/Pagination.hs` | `QueryPageRequest`, `QueryPageResponse`, constants, `parsePageRequest` |

The `Pagination` module is added to the `Service.Query` namespace alongside `Auth`, `Core`, `Endpoint`, etc.

## Consequences

### Positive

- Jess gets pagination for free — all query endpoints automatically enforce limits
- Production services are protected from unbounded result sets
- Mobile clients can implement infinite scroll with `hasMore`
- Per-query `maxResults` lets Jess tune limits for heavy vs. lightweight queries
- Response metadata (`total`, `hasMore`) enables proper UI pagination controls

### Negative

- Breaking change to API response format (array → `QueryPageResponse` object)
- Clients must update to handle the new response shape
- `getAll` in `QueryObjectStore` still loads all items into memory; true database-level pagination would require a store interface change (future work)

### Risks

- **In-memory pagination**: Pagination is applied after `getAll`, meaning the server still loads all query instances into memory before slicing. For very large datasets this is inefficient.
- **Offset-based pagination drift**: If items are inserted/deleted between page requests, offset-based pagination can skip or duplicate items.

### Mitigations

- The `maxResultsImpl` hard cap limits the damage even without true database-level pagination
- For the MVP, in-memory pagination is acceptable because query instances are typically bounded by domain constraints (e.g., number of active carts)
- Future ADR can introduce `QueryObjectStore.getPage` for database-level pagination when needed
- Cursor-based pagination can be added later as a separate enhancement without breaking `QueryPageResponse`
- Introduce `QueryObjectStore.getPage` when a service has >10k query instances of a single type

## References

- [#265: Add pagination and result limits to Query endpoints](https://github.com/neohaskell/NeoHaskell/issues/265)
- [ADR-0007: Queries and Read Models](0007-queries-read-models.md)
- [Service/Query/Endpoint.hs](../../core/service/Service/Query/Endpoint.hs)
- [Service/Query/Core.hs](../../core/service/Service/Query/Core.hs)
- [Service/QueryObjectStore/Core.hs](../../core/service/Service/QueryObjectStore/Core.hs)
