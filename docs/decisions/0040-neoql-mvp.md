# ADR-0040: NeoQL MVP — Field Access and Equality Filtering

## Status

Proposed

## Context

NeoHaskell query endpoints return all authorized records as a JSON array. Clients that need a subset of those records currently have no way to filter server-side: they must fetch everything and filter in the browser or calling service. For large read models this is wasteful, and it couples filtering logic to every consumer.

NeoQL is a small query language for NeoHaskell that lets clients express filters in a `?q=` URL parameter. This ADR covers the MVP phase: field access and equality filtering only. The full NeoQL specification (boolean operators, comparisons, nested paths, sort/limit) is tracked in GitHub Issue #391 and is explicitly out of scope here.

### Use Cases

- `GET /queries/cart-summary?q=.status == "pending"` — return only carts with status "pending"
- `GET /queries/stock-level?q=.warehouseId == "wh-42"` — return stock records for one warehouse
- Any query endpoint where a client wants a single-field equality filter without a dedicated endpoint per value

### Requirements

1. **Minimal grammar**: field access (`.fieldName`) and optional equality test (`== value`). Nothing more.
2. **No database changes**: filtering runs in memory on the JSON array already returned by `getAll`. `QueryObjectStore` is unchanged.
3. **Security ordering**: NeoQL filtering must run after both authorization phases, never before.
4. **Graceful degradation**: absent `?q=` parameter returns the full authorized result set (existing behavior).
5. **Clear error contract**: parse failures return HTTP 400; unknown fields return an empty array with HTTP 200.
6. **Megaparsec for parsing**: consistent with the project's DSL parsing approach, with good error messages.

### GitHub Issues

- [#447: NeoQL MVP — field access and equality filtering](https://github.com/neohaskell/NeoHaskell/issues/447)
- [#391: Full NeoQL specification](https://github.com/neohaskell/NeoHaskell/issues/391)

## Decision

### 1. MVP Grammar (Frozen)

The grammar accepted by the MVP parser is:

```
expr        = fieldAccess ( "==" value )?
fieldAccess = "." identifier
value       = string | number
identifier  = letter (letter | digit | "_")*
string      = '"' { char } '"'
number      = [ "-" ] digit { digit } [ "." digit { digit } ]
```

A bare `.fieldName` with no operator is a valid expression: it projects the field value but does not filter. An expression with `== value` filters to records where that field equals that value.

No other operators, no boolean connectives, no nested paths. The grammar is intentionally small so the MVP can ship and be validated before the full spec is built.

### 2. Module Location: `core/neoql/`

A new `neoql` source directory in `nhcore.cabal`, following the existing multi-source-dir pattern:

```
core/neoql/
  NeoQL.hs          -- re-exports (public API)
  NeoQL/
    Types.hs        -- Expr, FieldName, Value, FilterResult
    Parser.hs       -- megaparsec grammar
    Execute.hs      -- Aeson.Value executor
```

`NeoQL.hs` is re-exported from `Core.hs` so it is available project-wide without extra imports.

### 3. Type Definitions

```haskell
-- NeoQL/Types.hs

newtype FieldName = FieldName Text
  deriving (Eq, Show)

data Value
  = StringValue Text
  | NumberValue Double
  deriving (Eq, Show)

data Expr
  = FieldAccess FieldName
  | FieldEquals FieldName Value
  deriving (Eq, Show)

data FilterResult
  = Matches
  | NoMatch
  | FieldNotFound
  deriving (Eq, Show)
```

`FilterResult` distinguishes "field present but value differs" from "field not present at all". The executor uses this to return an empty array (not an error) when a field is unknown.

### 4. Parser: Megaparsec

The parser lives in `NeoQL/Parser.hs` and uses `megaparsec >= 9.0 && < 10`.

**Why megaparsec over alternatives:**

| Option | Verdict | Rationale |
|--------|---------|-----------|
| Hand-rolled | Rejected | Error messages are poor; maintenance burden grows with grammar |
| `ReadP` (base) | Rejected | Used elsewhere in the project but produces opaque error output; not suitable for returning HTTP 400 with a useful message |
| `attoparsec` | Rejected | Fast but error messages are minimal; designed for binary/network parsing |
| `megaparsec` | **Chosen** | Proven pattern for Haskell DSLs; rich error messages that can be surfaced to the client; grammar is small enough that the dependency is not over-engineering |

The parser exports a single function:

```haskell
parseExpr :: Text -> Result Text Expr
```

`Result Text Expr` — `Left` carries a human-readable parse error suitable for the HTTP 400 body.

### 5. Executor: Aeson.Value

The executor in `NeoQL/Execute.hs` operates on `Aeson.Value` directly. No schema knowledge is required.

```haskell
matchesExpr :: Expr -> Aeson.Value -> FilterResult
filterValues :: Expr -> Array Aeson.Value -> Array Aeson.Value
```

`filterValues` applies `matchesExpr` to each element and keeps only those returning `Matches`. Elements where the field is absent (`FieldNotFound`) are excluded silently.

Field lookup uses `Aeson.Object` key access. The field name from the expression is matched against JSON object keys as-is (case-sensitive). Non-object values (arrays, primitives) return `FieldNotFound`.

### 6. Security Ordering

NeoQL filtering runs after both authorization phases. The execution order for a query endpoint is:

```
canAccessImpl  (phase 1: can this user access this query type?)
  -> getAll    (fetch all records from store)
    -> canViewImpl  (phase 2: filter out records this user cannot see)
      -> NeoQL filter  (apply ?q= expression to authorized records)
        -> JSON encode -> HTTP 200
```

This ordering is non-negotiable. A NeoQL expression must never be used to bypass or short-circuit authorization. The filter sees only what the authorization layer has already approved.

This is enforced structurally: `createQueryEndpoint` in `Service.Query.Endpoint` owns the execution pipeline. NeoQL is applied as a final step inside that function, after `canViewImpl` has run.

### 7. HTTP Error Contract

| Condition | HTTP Status | Body |
|-----------|-------------|------|
| No `?q=` parameter | 200 | Full authorized result set (unchanged) |
| Valid `?q=`, matches found | 200 | Filtered JSON array |
| Valid `?q=`, no matches | 200 | Empty JSON array `[]` |
| Invalid `?q=` syntax | 400 | Parse error message |
| Unknown field (valid syntax) | 200 | Empty JSON array `[]` |
| Auth failure (pre-existing) | 401 / 403 | Unchanged |

Returning 200 with an empty array for unknown fields avoids leaking schema information. A 400 is reserved for syntax errors only.

### 8. WebTransport Integration

`Service.Transport.Web` extracts the `?q=` parameter from the Wai query string before calling the query handler. The integration point is the `processQueryWithClaims` helper inside the `["queries", queryNameKebab]` branch (around line 593 of `Web.hs`).

The flow:

1. Extract `q` from `Request.queryString`
2. If absent, call handler with no filter (existing path)
3. If present, call `NeoQL.parseExpr`
4. On parse error, return HTTP 400 immediately (before calling the handler)
5. On parse success, call handler, then apply `NeoQL.filterValues` to the result before encoding

The handler itself (`createQueryEndpoint`) does not need to know about NeoQL. Filtering is applied by the transport layer after the handler returns its authorized JSON text. This keeps the query handler API stable.

### 9. Dependency: megaparsec

Add to `nhcore.cabal` build-depends:

```
megaparsec >= 9.0 && < 10
```

No other new dependencies. `aeson` is already present.

### 10. Deferred Features

The following are explicitly out of scope for this ADR and tracked in Issue #391:

- Boolean operators (`and`, `or`, `not`)
- Comparison operators (`>`, `<`, `>=`, `<=`)
- Nested field access (`.a.b`)
- Array operations
- String matching (`contains`, `startsWith`)
- Membership (`in`, `not in`)
- Null checks
- Sort, limit, offset parameters
- `[ql|...|]` quasiquoter
- Database-level pushdown (`QueryObjectStore` unchanged)

## Consequences

### Positive

1. **Clients can filter without extra endpoints**: a single query endpoint serves multiple filter values without requiring a dedicated endpoint per value or per filter combination.

2. **No database changes**: the executor works on the in-memory JSON array. `QueryObjectStore`, `EventStore`, and all persistence layers are untouched.

3. **Security is preserved by construction**: the execution pipeline in `createQueryEndpoint` places NeoQL after both authorization phases. There is no code path where NeoQL runs before auth.

4. **Incremental path to full NeoQL**: the grammar, parser, and executor are isolated in `core/neoql/`. Adding operators in a future ADR means extending these modules without touching the transport or query layers.

5. **Good error messages**: megaparsec produces structured parse errors. HTTP 400 responses can include a readable description of where the parse failed, which helps clients debug malformed expressions.

6. **Existing behavior unchanged**: endpoints without a `?q=` parameter behave exactly as before. No migration required.

### Negative

1. **In-memory filtering only**: for large result sets, all records are fetched from the store before filtering. This is acceptable for the MVP but will need revisiting if result sets grow large enough to cause performance issues.

2. **Case-sensitive field matching**: JSON object keys are matched exactly. A client using `.Status` instead of `.status` gets an empty array with no error. This may surprise users but is consistent with JSON semantics.

3. **New dependency**: `megaparsec` is added to `nhcore`. It is a well-maintained library with no transitive surprises, but it is a new entry in the dependency graph.

### Risks

1. **Filter applied to encoded text, not typed values**: the executor works on `Aeson.Value` after JSON encoding. If a query handler returns non-standard JSON (e.g., numbers encoded as strings), equality comparisons may not behave as expected. Mitigated by documenting that NeoQL operates on the JSON representation.

2. **Grammar expansion pressure**: once clients use NeoQL, there will be pressure to add operators. The deferred features list in this ADR and the full spec in Issue #391 provide a clear boundary. New operators require a new ADR.

3. **Transport coupling**: placing the `?q=` extraction in `Web.hs` means `CliTransport` (ADR-0034) does not automatically get NeoQL support. A future ADR will need to address NeoQL for non-HTTP transports if that becomes necessary.

## References

- [GitHub Issue #447](https://github.com/neohaskell/NeoHaskell/issues/447) — NeoQL MVP feature request
- [GitHub Issue #391](https://github.com/neohaskell/NeoHaskell/issues/391) — Full NeoQL specification
- [Service/Query/Endpoint.hs](../../core/service/Service/Query/Endpoint.hs) — Query endpoint pipeline (auth ordering)
- [Service/Transport/Web.hs](../../core/service/Service/Transport/Web.hs) — WebTransport query handler
- [ADR-0007: Queries (Read Models)](0007-queries-read-models.md) — Query system context
- [ADR-0036: Wave 1 Security Hardening](0036-wave1-security-hardening.md) — Authorization pipeline
- [ADR-0034: CliTransport](0034-cli-transport.md) — Non-HTTP transport (NeoQL not yet integrated)
