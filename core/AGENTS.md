# nhcore Knowledge Base

**Directory:** `core/`
**Package:** nhcore (9 hs-source-dirs in one library)

## STRUCTURE

```text
core/
├── core/           # 26 modules - Primitives replacing base
├── traits/         # 8 modules  - Typeclasses with friendly names
├── concurrency/    # 7 modules  - Async, channels, locks
├── service/        # 40+ modules - Event-sourcing (CQRS)
├── system/         # 6 modules  - File, Directory, Path, Env
├── http/           # 2 modules  - HTTP client
├── json/           # 1 module   - JSON encoding/decoding
├── meta/           # 1 module   - TypeName reflection
├── options-parser/ # 1 module   - CLI argument parsing
└── testlib/        # 48 modules - Test utilities & specs
```

## WHERE TO LOOK

| Task | Location | Key Modules |
|------|----------|-------------|
| Replace `IO` | `core/Task.hs` | `Task err val` wrapping `ExceptT err IO` |
| Replace `Either` | `core/Result.hs` | `Result err val` with `Ok`/`Err` |
| Collections | `core/Array.hs` | `Array`, `Map`, `Set`, `LinkedList` |
| Text handling | `core/Text.hs` | `Text` (re-export of Data.Text) |
| Concurrency | `concurrency/` | `AsyncTask`, `Channel`, `ConcurrentVar`, `Lock` |
| File I/O | `system/File.hs` | `File.readText`, `File.writeText` |
| HTTP requests | `http/Http/Client.hs` | `Http.get`, `Http.post` |
| JSON | `json/Json.hs` | Re-exports `Aeson.ToJSON`, `Aeson.FromJSON` |
| Event store | `service/Service/EventStore/` | `Core.hs`, `InMemory.hs`, `Postgres/` |
| Commands | `service/Service/Command/` | `Command` typeclass, `CommandExecutor` |
| Queries | `service/Service/Query/` | `Query`, `QueryOf`, read model projections |
| Integrations | `service/Integration.hs` | `Outbound`, `Inbound`, `ToAction` |
| Entity pattern | `service/Service/Entity/` | `Entity`, `initialStateImpl`, `updateImpl` |
| Application | `service/Service/Application.hs` | Wires services, queries, integrations |
| Testing | `testlib/Test.hs` | `Test.Spec`, `Test.AppSpec` |

## MODULE HIERARCHY

### Core Prelude (`Core.hs`)
Auto-imported in all NeoHaskell files. Re-exports:
- Primitives: `Text`, `Array`, `Map`, `Maybe`, `Result`, `Task`, `Uuid`
- Concurrency: `Channel`, `ConcurrentVar`, `Lock`, `DurableChannel`
- Basics: `|>`, `<|`, `(++)`, `identity`, `always`, `fmt`
- Service types: `Entity`, `Command`, `Query`, `Service`

### Traits (Type Aliases)
| Trait | Haskell Equivalent | Key Method |
|-------|-------------------|------------|
| `Mappable` | `Functor` | `map` |
| `Thenable` | `Monad` | `andThen`, `yield` |
| `Applicable` | `Applicative` | `apply` |
| `Appendable` | `Semigroup` | `(++)` |
| `Combinable` | `Monoid` | `empty` |
| `Default` | custom | `def`, `defaultValue` |
| `ToText` | `Show` | `toText`, `toPrettyText` |
| `Collection` | custom (31 methods) | `map`, `reduce`, `takeIf`, etc. |

### Task vs IO
```haskell
-- Task wraps ExceptT for typed errors
newtype Task err value = Task { runTask :: ExceptT err IO value }

-- Key functions
Task.yield :: value -> Task _ value           -- NOT pure/return
Task.throw :: err -> Task err _
Task.andThen :: (a -> Task err b) -> Task err a -> Task err b
Task.fromIO :: IO a -> Task _ a
Task.runOrPanic :: Task err a -> IO a
```

## CONVENTIONS (core/-specific)

### Import Pattern
```haskell
-- Always import type + qualified module
import Array (Array)
import Array qualified

-- Use qualified for operations
words |> Array.map Text.length
```

### Qualified Function Design
Modules export functions designed for qualified use:
```haskell
-- Good: Reads naturally
Task.yield value
Array.map f xs
File.readText path
Channel.write msg chan

-- Bad: Unqualified collision
yield value     -- Could be any monad's yield
map f xs        -- Prelude's map
```

### Error Handling
```haskell
-- File errors
data File.Error = NotFound | NotWritable | NotReadable

-- Tasks carry typed errors
readConfig :: Path -> Task File.Error Config

-- Convert errors with mapError
task |> Task.mapError toAppError
```

### Concurrency Primitives
| Type | GHC Wrapper | Pattern |
|------|-------------|---------|
| `AsyncTask` | `Async` | `AsyncTask.run task` |
| `Channel` | `Unagi.Chan` | `Channel.new`, `Channel.read`, `Channel.write` |
| `ConcurrentVar` | `MVar` | `ConcurrentVar.containing`, `ConcurrentVar.modify` |
| `Lock` | `MVar ()` | Mutex pattern |
| `ConcurrentMap` | `TVar (Map k v)` | Concurrent dictionary |

## ANTI-PATTERNS (nhcore-specific)

| Instead of | Use |
|-----------|-----|
| `pure`/`return` | `Task.yield`, `Result.Ok` |
| `IO a` | `Task err a` |
| `Either a b` | `Result a b` |
| `Data.Text` | `Text` (re-exported) |
| `Data.Vector` | `Array` |
| `Control.Concurrent.MVar` | `ConcurrentVar` |
| `fmap` | `Task.map`, `Array.map`, etc. |
| `Data.Aeson` direct | `Json` module |

## FILE QUICK REFERENCE

| File | Purpose |
|------|---------|
| `core/Core.hs` | Prelude re-exports |
| `core/Basics.hs` | Operators, math, `fmt` quasi-quoter |
| `core/Task.hs` | Task monad (IO + errors) |
| `core/Result.hs` | Result type (Either replacement) |
| `core/Array.hs` | Vector-backed arrays |
| `traits/Collection.hs` | 31-method collection typeclass |
| `service/Decider.hs` | Decision monad for commands |
| `service/Integration.hs` | Outbound/Inbound integrations |
| `testlib/Test/Spec.hs` | Test specification DSL |
