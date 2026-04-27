---
id: perf_baseline
target_max_tokens: 3000
---

# NeoHaskell Performance Baseline

NeoHaskell targets **50,000 requests/second** sustained throughput on a single core at p99 < 10ms. All code in hot paths must be reviewed against this baseline.

---

## Hot Path Budgets

| Operation | Budget |
|-----------|--------|
| Request parsing | ≤ 50 µs |
| Authorization check | ≤ 10 µs |
| Command dispatch | ≤ 20 µs |
| Event serialization (per event) | ≤ 5 µs |
| Event persistence (single event) | ≤ 200 µs |
| Response serialization | ≤ 50 µs |
| Total per request (p50) | ≤ 500 µs |
| Total per request (p99) | ≤ 10 ms |

---

## 7 Performance Categories

### 1. INLINE Pragmas

Functions called in hot paths must be explicitly inlined. GHC's inliner is conservative; critical small functions will not be inlined without a pragma.

```haskell
-- CORRECT: INLINE on small hot-path functions
applyMiddleware :: Request -> Response
applyMiddleware = authenticate |> authorize |> process
{-# INLINE applyMiddleware #-}

-- Pattern: every function in core/auth/, core/http/, core/service/Service/Command/
-- with a call frequency > 10k/sec needs INLINE
```

Red line: Any function in a `critical_areas` path missing `INLINE` that is called in the request hot path.

### 2. UNPACK Pragmas

Strict fields in frequently-allocated records must use `{-# UNPACK #-}` to avoid heap indirection.

```haskell
-- CORRECT: Unpack numeric and small fixed-size fields
data RequestMetrics = RequestMetrics
  { {-# UNPACK #-} requestId :: Word64
  , {-# UNPACK #-} startTimeNs :: Int64
  , {-# UNPACK #-} bytesSent :: Word32
  }
```

Apply to: `Int`, `Word`, `Word64`, `Int64`, `Double`, `Float`, `Bool` fields in records allocated > 1k/sec.

### 3. Laziness and Strictness

NeoHaskell compiles with `{-# LANGUAGE Strict #-}` enabled project-wide. Do not add `~` (lazy) patterns in hot paths without profiler evidence that it helps.

```haskell
-- CORRECT: Explicit strict binding (redundant under Strict but documents intent)
let !result = expensiveComputation input

-- ANTI: Lazy accumulator in tight loop (causes space leak under non-Strict modules)
foldl (\acc x -> acc + x) 0 largeList  -- use foldl' or Array.foldl
```

### 4. Serialization

- Use `aeson` with `{-# LANGUAGE DeriveGeneric #-}` — do not hand-write `ToJSON`/`FromJSON` unless benchmarks show it's faster
- For binary protocols, use `flat` or `store` (via nhcore-serialize wrappers)
- Avoid `encode . decode` round-trips in hot paths — cache parsed values
- Use `Text` not `String` everywhere — `String` allocates O(n) cons cells

```haskell
-- ANTI: String-based serialization
show userId ++ " processed"

-- CORRECT: Text-based
[fmt|{userId} processed|]
```

### 5. Allocation Reduction

- Prefer `Array.foldl'` over `Array.map |> Array.foldl'` (fuse with rewrite rules)
- Use `Builder` (via `nhcore-text`) for constructing large `Text` values
- Avoid `Array.concat` on large arrays in hot paths — use streaming
- Pool frequently-allocated objects at the service level when possible

```haskell
-- ANTI: Multiple intermediate allocations
let step1 = input |> Array.map transform
    step2 = step1 |> Array.filter predicate
    step3 = step2 |> Array.map finalize

-- CORRECT: Fused single pass (if transform and finalize can be composed)
let result = input |> Array.filterMap (transform |> Option.filter predicate |> Option.map finalize)
```

### 6. Concurrency

- Use `nhcore-async` (wraps `async` library) for parallel work — never `forkIO` directly
- Bounded queues only — `TBQueue` with explicit capacity, never unbounded `TQueue`
- Avoid `MVar` in hot paths; use `IORef` with `atomicModifyIORef'` for simple counters
- `STM` is acceptable for complex state but benchmark against `IORef` alternatives first

```haskell
-- CORRECT: Bounded concurrency
let semaphore = Semaphore.new maxConcurrent
result <- Semaphore.withPermit semaphore (processRequest req)
```

### 7. SPECIALIZE Pragmas

Polymorphic functions that are called with known types in hot paths should be specialized:

```haskell
-- Generic definition
processEvents :: forall event. Json.FromJson event => Array Bytes -> Result ParseError (Array event)
processEvents = ...

-- Specialization for the common case
{-# SPECIALIZE processEvents :: Array Bytes -> Result ParseError (Array UserEvent) #-}
{-# SPECIALIZE processEvents :: Array Bytes -> Result ParseError (Array OrderEvent) #-}
```

---

## Compiler Context

All nhcore packages compile with:

```
ghc-options: -O2 -funbox-strict-fields
default-extensions: Strict NoImplicitPrelude
```

This means:
- All fields are strict by default (use `~` to opt out, rarely)
- Small record fields are unboxed automatically when possible
- `-O2` enables aggressive inlining and specialization

---

## Red Lines

| Red Line | Reason |
|----------|--------|
| `forkIO` in application code | Unbounded threads; use nhcore-async |
| Unbounded `TQueue` | Memory exhaustion under load |
| `String` in hot-path data types | O(n) allocation overhead |
| Missing `INLINE` on functions in `critical_areas` | Regression from 50k req/s target |
| `Data.Map.Lazy` in hot path | Lazy thunks cause GC pressure |
| `show` for logging numeric fields in hot path | Allocates String unnecessarily |

---

## Profiling Workflow

When a performance regression is detected:

1. `cabal run nhcore-bench -- --output bench.html` — run criterion benchmarks
2. `cabal build --enable-profiling && cabal run nhcore-profiled -- +RTS -p -RTS` — profiler report
3. Check `.prof` for unexpected allocations in the hot path
4. Apply INLINE/UNPACK/SPECIALIZE as directed by the profiler
5. Re-run benchmarks to confirm improvement
