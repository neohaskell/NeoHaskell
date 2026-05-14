# Performance review methodology

Scope: feature-level diffs (one type, one module, one codec) on a NeoHaskell event-sourcing service targeting ~50k req/s. `Strict` is on globally, Hasql is the Postgres driver, Aeson is the JSON codec, and the custom Prelude already exports `foldl` as strict.

## Contents

- [1. INLINE / INLINABLE / SPECIALIZE](#1-inline--inlinable--specialize)
- [2. UNPACK with strict fields](#2-unpack-with-strict-fields)
- [3. Strict-extension footguns](#3-strict-extension-footguns)
- [4. `toEncoding` vs `toJSON` (Aeson)](#4-toencoding-vs-tojson-aeson)
- [5. `Text` vs `ShortText` vs `ByteString`](#5-text-vs-shorttext-vs-bytestring)
- [6. Fusion (`foldr` / `build`)](#6-fusion-foldr--build)
- [7. Profiling-first vs eyeball review](#7-profiling-first-vs-eyeball-review)
- [8. STM / IORef / TVar contention](#8-stm--ioref--tvar-contention)
- [9. The premature-optimization smell](#9-the-premature-optimization-smell)

## 1. INLINE / INLINABLE / SPECIALIZE

Rule: use `INLINABLE` for exported polymorphic (typeclass-overloaded) functions whose cross-module call-sites should be specialised; reserve `INLINE` for small functions unlocking further `RULES`/fusion; attach `SPECIALIZE` only for a known-hot concrete type when the function is too large to be reliably `INLINABLE`. GHC inlines nothing past its unfolding threshold unless told, and `INLINE` overrides aggressively. Loop-breakers never inline regardless of pragma. Without `INLINABLE`/`INLINE` on an exported overloaded function, GHC cannot specialise across modules and dictionary passing dominates.

Reviewer check: does the diff add an exported polymorphic function with NO `INLINABLE` that is called in a hot loop from another module, OR add `INLINE` to a function >~10 lines that does not participate in a `RULES` rewrite?

Sources: <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html> ; <https://wiki.haskell.org/Inlining_and_Specialisation> ; <https://well-typed.com/blog/2024/04/choreographing-specialization-pt1/>

## 2. UNPACK with strict fields

Rule: with `-funbox-strict-fields`, GHC already unpacks strict fields of *single-constructor* types — `{-# UNPACK #-}` is redundant there. `UNPACK` is only a measurable win for (a) sum-type fields, which `-funbox-strict-fields` will NOT unbox, or (b) coalescing sub-word fields into a single word. It is harmful when the field is pattern-matched then passed to a non-strict consumer, forcing re-boxing at the call site.

Reviewer check: is the new `UNPACK` on a single-constructor strict field in a project compiled with `-funbox-strict-fields` (redundant), or on a field whose value is frequently passed to lazy/polymorphic consumers (net negative)? If yes, request removal or a microbench justifying it.

Sources: <https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/pragmas.html#unpack-pragma> ; <https://wiki.haskell.org/Performance/Data_types>

## 3. Strict-extension footguns

Rule: `Strict`/`StrictData` does NOT make top-level bindings strict, does NOT affect imported types (Prelude `Maybe`/`Either`/lists/tuples stay lazy), does NOT recurse into nested patterns (`let (a,b) = ...` forces the pair but not `a`/`b`), and does NOT deep-force — a strict `Map k V` field still contains lazy `V`s. It also harms when (i) a recursive accumulator over a non-flat structure builds a partial WHNF retaining the prior thunk graph, or (ii) short-circuiting (`&&`, `||`, `find`, early-exit `foldr`) is defeated by unconditional forcing.

Reviewer check: does the diff (a) rely on Prelude `Maybe`/list for hot-path data assuming `Strict` covers it (it does not), (b) introduce a recursive `go` building a non-flat accumulator without `deepseq` reasoning, or (c) replace a `foldr`-based search with a strict fold and lose short-circuiting?

Sources: <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/strict.html> ; <https://chshersh.com/blog/2022-08-08-space-leak.html> ; <https://free.cofree.io/2021/12/13/space-leak/>

## 4. `toEncoding` vs `toJSON` (Aeson)

Rule: `toEncoding` writes directly to a `ByteString.Builder` and is documented at >2x faster with ~1/3 the allocation versus `toJSON`, which materialises a full `Value` tree. The default `toEncoding = toEncoding . toJSON` gives ZERO speedup; the win only appears when `toEncoding` is hand-written or derived (TH `deriveJSON`/Generic both emit a real one).

Reviewer check: does the diff add a hand-written `ToJSON` instance on a serialisation hot path (events, API responses, projections) that defines only `toJSON` and omits `toEncoding`? Request `toEncoding` or switch to `deriveJSON`/Generic.

Sources: <https://hackage.haskell.org/package/aeson/docs/Data-Aeson.html> ; <https://github.com/haskell/aeson/blob/master/changelog.md>

## 5. `Text` vs `ShortText` vs `ByteString`

Rule: `Text` (UTF-8 since text-2) is the default for human-readable strings and slicing-heavy workloads; ~6 words plus payload, unpinned. `ShortText` is ~4 words plus payload with no slicing — prefer for populations of millions of short keys (event IDs, tags, fixed-vocabulary identifiers) where memory dominates over substring ops. `ByteString` is pinned and suited for binary, network buffers, FFI, pass-through bytes. Every `pack`/`unpack` and `encodeUtf8`/`decodeUtf8'` round-trip is O(n) allocation; chains on a hot path are a smell.

Reviewer check: does the diff round-trip the same payload through `pack`/`unpack` or `encodeUtf8`/`decodeUtf8` more than once per request? Does it use `Text` for a field whose values are millions of short tags? Does `String` appear anywhere in the hot path?

Sources: <https://hasufell.github.io/posts/2024-05-07-ultimate-string-guide.html> ; <https://hackage.haskell.org/package/text-short> ; <https://markkarpov.com/post/short-bs-and-text.html>

## 6. Fusion (`foldr` / `build`)

Rule: good producers (`map`, `filter`, `++` left arg, `take`, `iterate`, `zip`/`zipWith`, list comprehensions, enumerations) and good consumers (`foldr`, `foldl'`-via-`foldr`, `map`, `filter`, `concat`, `any`/`all`, `head`, `take`) fuse and eliminate the intermediate list. Fusion breaks when an intermediate list escapes (bound and used twice), crosses a non-fusing combinator (`reverse`, `sort`, `nub`, custom recursion), or crosses a `Vector`/`Array`/`Text` boundary without using that type's own fusion framework. Bang patterns on the intermediate list and some `case` scrutinee shapes also defeat the `RULES` matcher.

Reviewer check: does the diff (a) introduce `let xs = map f ...` consumed twice (blocks fusion), (b) pipe through a non-fusing combinator mid-chain, or (c) bang-pattern an intermediate list?

Sources: <https://wiki.haskell.org/GHC_optimisations> ; <https://well-typed.com/blog/2024/03/haskell-unfolder-episode-22-foldr-build-fusion/> ; <https://downloads.haskell.org/ghc/latest/docs/users_guide/rewrite_rules.html>

## 7. Profiling-first vs eyeball review

Rule: eyeball review suffices for catching *known anti-patterns* (missing `INLINABLE` on a cross-module overloaded function, `TVar (Map k v)` in a write-heavy path, `toJSON`-only on a hot codec, `String` in hot path, `pack`/`unpack` round-trips). Profiling evidence is REQUIRED for any "this is faster" claim beyond removing those anti-patterns — including new `INLINE`/`UNPACK` pragmas, algorithm swaps, data-structure replacements. Current GHC recommendation: `-fprof-late` (9.4+) so cost centres don't perturb optimisation, plus eventlog + `eventlog2html` for allocation and ticky for entry counts. Criterion/tasty-bench is the bar for microbenches.

Reviewer check: does the PR claim a speedup without (a) a Criterion or tasty-bench result, (b) a late-cost-centre profile, or (c) a measurable production metric? Block on evidence or downgrade the claim.

Sources: <https://well-typed.com/blog/2023/03/prof-late/> ; <https://downloads.haskell.org/ghc/latest/docs/users_guide/profiling.html> ; <https://github.com/haskell-perf/checklist>

## 8. STM / IORef / TVar contention

Rule: a single `TVar` wrapping `Map`/`HashMap`/`Set` serialises ALL writers and invalidates every concurrent reader on any write, regardless of key, collapsing scaling past 2-4 threads. Sharded structures (`stm-containers`, `ttrie`) or per-key `TVar` indirection avoid this. Uncontended baseline: `modifyTVar'` ~191 ns, `atomicModifyIORef'` ~877 ns, `modifyMVar_` ~3.3 us; under contention `TVar (Map ...)` degrades catastrophically while `stm-containers` stays flat. Adopting `stm-containers`/sharding is overkill for state that is read-mostly, single-writer, or already serialised behind an event-loop or command-handler queue.

Reviewer check: does the diff introduce `TVar (Map _ _)`, `TVar (HashMap _ _)`, or `TVar (Set _)` AND will multiple request threads write to it? If yes, push for `stm-containers` or sharding. If only one writer exists, accept the `TVar`.

Sources: <https://nikita-volkov.github.io/stm-containers/> ; <https://www.parsonsmatt.org/2025/12/17/the_subtle_footgun_of_tvar_(map____).html> ; <https://discourse.haskell.org/t/the-subtle-footgun-of-tvar-map/13429>

## 9. The premature-optimization smell

Rule: pragma cascades (`INLINE` + `UNPACK` + `SPECIALIZE` + bang-everywhere) on code not demonstrated to be hot are net negative — they inflate compile time, bloat code size (hurting I-cache and real throughput), can defeat fusion, and lock GHC out of cross-module specialisation. Well-Typed and haskell-perf consensus: algorithm and data-structure first, profile second, pragmas last, and only on functions the profile shows in top allocations or top entries. Scale review intensity to measured hot-path impact: event decoders and projection steps deserve scrutiny; one-shot admin/setup code does not.

Reviewer check: is the pragma being added to a function that appears in a late-cost-centre profile or ticky top-N for this service? If not, request removal and ship the feature plain; revisit if a future profile proves it hot.

Sources: <https://well-typed.com/training_performance_and_optimization/> ; <https://github.com/haskell-perf/checklist> ; <https://wiki.haskell.org/Performance> ; <https://well-typed.com/blog/2023/03/prof-late/>
