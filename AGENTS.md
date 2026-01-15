# NEOHASKELL KNOWLEDGE BASE

## OVERVIEW

NeoHaskell dialect of Haskell for newcomer-friendliness. Monorepo: core library (nhcore), CLI tool (neo), Astro website. Event-sourcing architecture with CQRS.

## STRUCTURE

```text
NeoHaskell/
├── core/           # nhcore library (9 source dirs in one package)
│   ├── core/       # Primitives: Text, Int, Array, Maybe, Result, Task
│   ├── service/    # Event-sourcing: EventStore, Command, Query, Integration
│   ├── concurrency/# AsyncTask, Channel, Lock, ConcurrentMap
│   ├── traits/     # Typeclasses: Mappable, Appendable, Combinable
│   └── system/     # File, Directory, Path, Environment
├── cli/            # neo CLI: new, build, run, shell
├── testbed/        # Example app + Hurl acceptance tests
└── website/        # Astro/Starlight docs
```

## WHERE TO LOOK

| Task                             | Location                       | Notes                      |
| -------------------------------- | ------------------------------ | -------------------------- |
| Core types (Text, Array, Result) | `core/core/`                   | NOT base/Data.\*           |
| Event-sourcing                   | `core/service/Service/`        | EventStore, Command, Query |
| Integration patterns             | `core/service/Integration.hs`  | Outbound/Inbound           |
| CLI commands                     | `cli/src/Neo/`                 | new, build, run, shell     |
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
cabal test nhcli-test  # CLI only
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
