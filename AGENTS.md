# NEOHASKELL KNOWLEDGE BASE

## OVERVIEW

NeoHaskell dialect of Haskell for newcomer-friendliness. Monorepo: core library (nhcore), testbed example app, Astro website. Event-sourcing architecture with CQRS.

## STRUCTURE

```text
NeoHaskell/
├── core/           # nhcore library (9 source dirs in one package)
│   ├── core/       # Primitives: Text, Int, Array, Maybe, Result, Task
│   ├── service/    # Event-sourcing: EventStore, Command, Query, Integration
│   ├── concurrency/# AsyncTask, Channel, Lock, ConcurrentMap
│   ├── traits/     # Typeclasses: Mappable, Appendable, Combinable
│   └── system/     # File, Directory, Path, Environment
├── testbed/        # Example app + Hurl acceptance tests
└── website/        # Astro/Starlight docs
```

## WHERE TO LOOK

| Task                             | Location                       | Notes                      |
| -------------------------------- | ------------------------------ | -------------------------- |
| Core types (Text, Array, Result) | `core/core/`                   | NOT base/Data.\*           |
| Event-sourcing                   | `core/service/Service/`        | EventStore, Command, Query |
| Integration patterns             | `core/service/Integration.hs`  | Outbound/Inbound           |
| Example app patterns             | `testbed/src/`                 | Cart, Stock domains        |
| Test patterns                    | `core/test/`, `testbed/tests/` | Hspec + Hurl               |

## CONVENTIONS (NeoHaskell-Specific)

### Type Aliases

| Haskell         | NeoHaskell                |
| --------------- | ------------------------- |
| `IO a`          | `Task err val`            |
| `Either a b`    | `Result error value`      |
| `pure`/`return` | `Task.yield`, `Result.ok` |
| `Data.Text`     | `Text`                    |
| `Data.Map`      | `Map`                     |

### Style Rules

- **Pipe over nesting**: `x |> foo |> bar` NOT `bar $ foo x`
- **Do blocks**: Use `do` + `let` for bindings, never `let..in`/`where`
- **Case only**: Pattern match in `case`, not function definitions
- **Descriptive types**: `forall input output.` NOT `forall a b.`
- **Qualified imports**: `Module.function` design (e.g., `EventStore.new`)
- **String interpolation**: `[fmt|Hello {name}!|]` NOT `<>`/`++`

### Base Imports

When unavoidable, prefix with `Ghc`:

```haskell
import Data.List qualified as GhcList
```

## ANTI-PATTERNS (FORBIDDEN)

- `let..in` or `where` clauses
- Pattern matching in function definitions
- Point-free style
- Single-letter type params (`a`, `b`, `m`)
- `pure`/`return` - use `Type.yield`
- Raw `IO` - use `Task`
- `Either` - use `Result`
- String concat with `++`/`<>` - use `[fmt|...|]`
- Creating modules when existing ones can extend

## COMMANDS

```bash
# Build
cabal build all
nix develop            # Or: nix-shell

# Test
cabal test             # All tests
cabal test nhcore-test # Core only
./scripts/run-doctest  # Doctests

# Lint/Format
hlint .                # Linter (fourmolu auto-formats on save)

# Integration tests (requires testbed running)
cabal run nhtestbed &  # Start testbed
./testbed/scripts/run-tests.sh
```

## BUILD SYSTEM

**Nix + Hix** (not Stack):

- `flake.nix` → Haskell.nix with GHC 9.8
- `nix/hix.nix` → Hix config
- `.envrc` → Direnv auto-load

**CI**: Separate Linux/macOS workflows, PostgreSQL service, Hurl integration tests

## NOTES

- **Outside-in development**: Write usage first, let compiler guide
- **Tests before code**: Hspec unit + Hurl blackbox
- **Multi-source-dir package**: nhcore uses 9 hs-source-dirs (non-standard)
- **Testbed = example + tests**: Real app used for acceptance testing
- **Never modify test expectations** without asking

## TESTING REQUIREMENTS (MANDATORY)

Every change MUST include tests. Tests are NOT optional.

### Rules

- **All new functionality must have behavioral tests** covering happy path, error cases, and boundary conditions
- **Bug fixes must include regression tests** that would have caught the bug
- **Never modify existing test expectations** without explicit approval from the maintainer
- **Tests must compile and pass** before any PR is submitted
- **Run `hlint` on all changed files** and fix any warnings (treated as errors in CI)

### Test Suites

| Suite | Scope | PostgreSQL | Command |
| ----- | ----- | ---------- | ------- |
| `nhcore-test-core` | Core primitives (Text, Array, Result, etc.) | No | `cabal test nhcore-test-core` |
| `nhcore-test-auth` | Auth & JWT | No | `cabal test nhcore-test-auth` |
| `nhcore-test-service` | Service layer (EventStore, Commands, Queries) | Yes | `cabal test nhcore-test-service` |
| `nhcore-test-integration` | Integration tests | Yes | `cabal test nhcore-test-integration` |
| `nhcore-test` | All of the above | Yes | `cabal test nhcore-test` |

### Test Registration

- `nhcore-test` uses `hspec-discover` (automatic — just add `*Spec.hs` files under `core/test/`)
- `nhcore-test-service` uses **manual registration** in `core/test-service/Main.hs` — new spec modules must be imported and added to the `Hspec.describe` list
- All test modules must be listed in `other-modules` in `core/nhcore.cabal` for their respective test suites

### Test Patterns

- Use `Hspec` via the `Test` module (re-exported by nhcore)
- Follow NeoHaskell style in tests: `|>` pipes, `case..of`, `do` blocks, qualified imports
- Use `Result.isOk` / `Result.isErr` with `shouldSatisfy` for Result assertions
- Use `Test.fail [fmt|message|]` for custom failure messages
- Use `def` from `Default` for placeholder values in test records
