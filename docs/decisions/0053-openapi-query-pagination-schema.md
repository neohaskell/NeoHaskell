# ADR-0053: OpenAPI Query Pagination Schema

## Status

Accepted

## Context

### Current State

ADR-0051 introduced paginated responses for query endpoints. At runtime, all query endpoints return a `QueryPageResponse` envelope:

```json
{
  "items": [{"cartSummaryId": "...", "itemCount": 3}, ...],
  "total": 142,
  "hasMore": true,
  "effectiveLimit": 100
}
```

However, the OpenAPI spec generator in `Schema.OpenApi.makeQueryPath` still declares query responses as plain arrays:

```haskell
let arraySchema = toOpenApiSchema (SArray schema.responseSchema)
```

This produces:

```json
{
  "schema": {
    "type": "array",
    "items": { ... }
  }
}
```

Any client generated from the OpenAPI spec (via Orval, openapi-generator, etc.) expects a raw array and breaks at runtime. In the mobile app, `Array.isArray(data)` returned `false` on the paginated wrapper object, causing query results to never match.

Additionally, `makeQueryPath` does not declare the `limit`, `offset`, or `q` query parameters, so generated clients have no knowledge of pagination or NeoQL filtering capabilities.

### Use Cases

- **Code-generated clients**: Jess uses Orval to generate a TypeScript client from the OpenAPI spec. The generated types expect `UserOrder[]` but receive `{ items: UserOrder[], total: number, ... }`, causing type errors and runtime failures.

- **API documentation**: The Scalar docs UI at `/docs` shows query responses as arrays, misleading developers about the actual response shape.

- **Pagination discovery**: Without `limit`/`offset` parameters in the spec, Jess doesn't know pagination exists unless she reads the backend source code.

### Design Goals

1. **Spec matches runtime**: The OpenAPI spec must declare the exact response shape that the server returns.

2. **Discoverable pagination**: The `limit`, `offset`, and `q` query parameters must appear in the spec so code generators and docs expose them.

3. **Minimal change surface**: Fix only the OpenAPI generation layer. No changes to runtime behavior, pagination logic, or transport routing.

### GitHub Issue

- [#600: OpenAPI spec declares query responses as plain arrays but runtime returns paginated objects](https://github.com/neohaskell/NeoHaskell/issues/600)

## Decision

### 1. Query Response Schema

Replace the plain `SArray` wrapper in `makeQueryPath` with an inline paginated envelope schema that matches `QueryPageResponse`:

| Candidate | Verdict | Rationale |
|-----------|---------|-----------|
| Derive schema from `QueryPageResponse` via `ToSchema` | Rejected | `QueryPageResponse` is parameterized; the OpenAPI generator needs to inline the item schema per-query. Auto-derivation would produce a generic `$ref` without the per-query item type. |
| Hardcode the envelope schema in `makeQueryPath` | **Chosen** | The envelope shape is stable (defined in ADR-0051) and has exactly 4 fields. Inlining the schema in `makeQueryPath` keeps the fix self-contained and mirrors how `makeFileUploadPath` hardcodes its multipart schema. |

The generated schema becomes:

```json
{
  "type": "object",
  "required": ["items", "total", "hasMore", "effectiveLimit"],
  "properties": {
    "items": {
      "type": "array",
      "items": { ... per-query item schema ... }
    },
    "total": { "type": "integer", "description": "Total matching items after authorization filtering" },
    "hasMore": { "type": "boolean", "description": "Whether more items exist beyond this page" },
    "effectiveLimit": { "type": "integer", "description": "Actual page size limit applied (may be lower than requested)" }
  }
}
```

### 2. Query Parameters

Add three query parameters to `makeQueryPath`:

| Parameter | Type | Required | Description |
|-----------|------|----------|-------------|
| `limit` | integer | No | Max items per page (default: 100, max: 1000) |
| `offset` | integer | No | Starting position (default: 0) |
| `q` | string | No | NeoQL filter expression |

These match the parameters already parsed in `Service.Transport.Web` at runtime.

### 3. Module Placement

All changes are in `Schema.OpenApi` (`core/schema/Schema/OpenApi.hs`). No new modules required.

### 4. Test Updates

The existing test `"query endpoint response schema is array type with items"` must be updated to assert the paginated envelope shape instead of a plain array.

New test cases:
- Query response schema is an object with `items`, `total`, `hasMore`, `effectiveLimit` properties
- Query `items` property is an array of the item schema
- Query parameters include `limit`, `offset`, and `q`

## Consequences

### Positive

- Generated clients (Orval, openapi-generator) produce correct types matching the runtime response
- Scalar docs at `/docs` show the actual paginated response shape
- Pagination parameters are discoverable in the spec without reading source code
- NeoQL filtering is documented in the OpenAPI spec

### Negative

- Clients generated from the old (incorrect) spec will need regeneration — but they were already broken at runtime, so this is strictly an improvement

### Risks

- If `QueryPageResponse` shape changes in the future, the hardcoded schema in `makeQueryPath` must be updated manually

### Mitigations

- The `QueryPageResponse` shape is stable and defined in ADR-0051
- Tests verify the schema matches the expected envelope structure

## References

- [#600: OpenAPI spec declares query responses as plain arrays but runtime returns paginated objects](https://github.com/neohaskell/NeoHaskell/issues/600)
- [ADR-0051: Query Pagination and Result Limits](0051-query-pagination-result-limits.md)
- [ADR-0014: WebTransport OpenAPI Integration](0014-webtransport-openapi-integration.md)
- [Schema/OpenApi.hs](../../core/schema/Schema/OpenApi.hs)
- [Service/Query/Pagination.hs](../../core/service/Service/Query/Pagination.hs)
