# TESTBED KNOWLEDGE BASE

**Dual purpose**: Example NeoHaskell application + blackbox acceptance tests.

## STRUCTURE

```text
testbed/
├── launcher/Launcher.hs       # Entry point (thin: just calls App.run)
├── src/
│   ├── App.hs                 # Service composition (fat: all wiring)
│   ├── Testbed/Service.hs     # Aggregates Cart + Stock services
│   ├── Testbed/Cart/          # Bounded context: shopping cart
│   │   ├── Core.hs            # Entity, Events, update logic
│   │   ├── Service.hs         # Command registration
│   │   ├── Commands/*.hs      # CreateCart, AddItem
│   │   ├── Queries/*.hs       # CartSummary projection
│   │   └── Integrations.hs    # Outbound (→Stock) + Inbound (timer)
│   └── Testbed/Stock/         # Bounded context: inventory
│       ├── Core.hs            # StockEntity, StockEvent
│       ├── Service.hs         # Command registration
│       ├── Commands/*.hs      # InitializeStock, ReserveStock
│       └── Queries/*.hs       # StockLevel projection
├── tests/
│   ├── commands/*.hurl        # Single command tests
│   ├── queries/*.hurl         # Read model tests
│   └── scenarios/*.hurl       # Multi-step workflows
└── scripts/run-tests.sh       # Auto-starts app, runs all Hurl tests
```

## WHERE TO LOOK

| Pattern | File(s) | Notes |
|---------|---------|-------|
| App composition | `src/App.hs` | withService, withQuery, withOutbound |
| Thin launcher | `launcher/Launcher.hs` | 11 lines total |
| Entity + Events | `*/Core.hs` | Type families, update function |
| Command decide | `*/Commands/*.hs` | Business logic, getEntityId |
| Query projection | `*/Queries/*.hs` | deriveQuery TH, combine |
| Process Manager | `Cart/Integrations.hs` | Cross-domain coordination |
| Timer inbound | `Cart/Integrations.hs` | periodicCartCreator |

## HURL TEST PATTERNS

**Commands** (`tests/commands/`): Single operation, verify response structure
```hurl
POST http://localhost:8080/commands/create-cart
[]
HTTP/1.1 200
[Asserts]
jsonpath "$.entityId" matches /^[0-9a-f-]{36}$/
```

**Queries** (`tests/queries/`): Verify read model endpoint
```hurl
GET http://localhost:8080/queries/cart-summary
HTTP/1.1 200
[Asserts]
jsonpath "$" isCollection
```

**Scenarios** (`tests/scenarios/`): Multi-step with captures + retry
```hurl
POST http://localhost:8080/commands/create-cart
[]
HTTP/1.1 200
[Captures]
cart_id: jsonpath "$.entityId"

GET http://localhost:8080/queries/stock-level
[Options]
retry: 10
retry-interval: 200
```

## ADDING A BOUNDED CONTEXT

1. Create `src/Testbed/NewDomain/Core.hs` (Entity, Events, update)
2. Create `src/Testbed/NewDomain/Commands/*.hs` (decide logic)
3. Create `src/Testbed/NewDomain/Service.hs` (register commands)
4. Create `src/Testbed/NewDomain/Queries/*.hs` (optional projections)
5. Register in `App.hs`: `withService`, `withQuery`
6. Add tests: `tests/commands/new-command.hurl`

## COMMANDS

```bash
# Run testbed
cabal run nhtestbed

# Run all tests (auto-starts app)
./testbed/scripts/run-tests.sh

# Run specific test category
hurl --test testbed/tests/commands/*.hurl
hurl --test testbed/tests/scenarios/*.hurl
```

## NOTES

- **Retry for async**: Use `retry: N` + `retry-interval: ms` for integration effects
- **Captures**: `[Captures]` to pass IDs between requests
- **JSONPath filters**: `$[?(@.field == 'value')]` for array element matching
- Tests run from repo root, paths are `testbed/tests/...`
