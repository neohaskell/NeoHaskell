> **ARCHIVAL KNOWLEDGE — DO NOT USE OR REFERENCE.**
> Superseded by `codemap/` + root `AGENTS.md`. Scheduled for deletion once the pipeline is verified (Phase 6).
> Manifest: `docs/archive/2026-07-ai-artifacts/MANIFEST.md`

# nhcore context for reviewers

Compiler flags and framework conventions that change what a review must (and must not) flag.

## Contents

- [Boy scout rule](#boy-scout-rule)
- [Toolchain invocation](#toolchain-invocation)
- [Global compiler flags](#global-compiler-flags)
- [Custom Prelude](#custom-prelude)
- [Framework-provided defaults](#framework-provided-defaults)
- [Red-flag-but-already-handled list](#red-flag-but-already-handled-list)

## Boy scout rule

Every phase that opens a file to write to it must leave that file at least as clean as it found it. Concretely: when an implementation, fix-findings, build-loop, or build-iter step modifies a file that *also* contains pre-existing style debt — unqualified `import Module (helper)`, point-free top-level bodies, `_` wildcard parameters, raw `String`/`IO`/`Either`, `$` operators, `let..in` or `where`, `<>`/`++` for string concat — the agent fixes the debt while it is touching the file. The rule applies even when the style violation is unrelated to the original task: every file the diff lists is now ours.

The rule does **not** mean "rewrite every file in the repo": it applies only to files the current phase's diff already includes. Untouched files stay untouched. The rule also does **not** override "tests are immutable in phase 10" — test bodies / assertions stay immutable; only the surrounding helpers, imports, and fixtures that the implementation phase legitimately touches fall under the cleanup obligation.

When applying the boy scout rule, the cleanup is mentioned in the phase's commit message under a separate "Boy scout cleanup:" bullet so reviewers can distinguish the load-bearing change from the style sweep.

## Toolchain invocation

Every Haskell-toolchain command (`cabal`, `hlint`, `ghc`, `hspec-discover`, etc.) must be invoked through the repo's nix dev shell. The canonical form is:

```
nix develop --command <command> <args...>
```

Examples used by the pipeline's leaves:

- `nix develop --command cabal build all`
- `nix develop --command cabal test --test-show-details=streaming`
- `nix develop --command cabal test`
- `nix develop --command hlint <files...>`

Bare `cabal ...` / `hlint ...` calls will fail outside the dev shell because the toolchain is pinned by `flake.nix`. System-level tools (`git`, `gh`, `python3`) are not wrapped — they live on the user's PATH and do not depend on the dev shell. Refusal: if `nix` is not on PATH, the leaf cannot proceed and must surface the missing binary rather than fall back to system `cabal`.

## Global compiler flags

- `Strict` / `StrictData` are on globally. All let-bindings, function args, and data-constructor fields are strict by default. Do not request `!` on fields; do not advise "use strict fields"; do not flag `foldl` as lazy (nhcore's `foldl` IS `foldl'`).
- `NoImplicitPrelude` is on. Every module imports from nhcore. Bare `Prelude.foldl`, raw `[]`, raw `String` cannot appear by accident — flag them when they do.
- `-funbox-strict-fields` is on. `{-# UNPACK #-}` on a strict field of a single-constructor type is redundant. Only request `UNPACK` for sum-type fields or sub-word coalescing per [perf §2](./performance-methodology.md#2-unpack-with-strict-fields).

## Custom Prelude

- `Text` (not `String`).
- `Array a` (not `[]`).
- `Result error value` (not `Either`).
- `Task err val` (not `IO`).
- `Result.ok` / `Task.yield` (not `pure` / `return`).
- `|>` for piping (not `$` / nesting).
- `[fmt|...|]` for interpolation (not `<>` / `++`).
- `do` + `let` for bindings (no `let..in`, no `where`).
- `case ... of` for pattern matching.

## Framework-provided defaults

- `Redacted` wrapper for secret values; hand-written `Show` printing `<redacted>` is the established pattern for secret newtypes (e.g. `ClientSecret`, `AccessToken`, `RefreshToken`, `HmacKey`).
- `canAccess` / `canView` two-phase authorization is a compile-time requirement for every query.
- `RequestContext` threading is compile-time-enforced for every command.
- `constEq` is the constant-time comparison primitive, with a mandatory `{-# INLINE constEq #-}` pragma.
- Postgres EventStore uses Hasql's typed `Statement` + `Encoders` + `Decoders` — string concatenation is unrepresentable.
- `Crypto.Random.getRandomBytes` is the only sanctioned CSPRNG source for security-sensitive random.

## Red-flag-but-already-handled list

A grounding pass demotes any finding that names one of these conditions, because the framework already handles it:

- "Strict fields are missing" → `Strict` covers it; demote.
- "Use `foldl'` instead of `foldl`" → nhcore's `foldl` is strict; demote.
- "SQL string concatenation risk" → Hasql forbids it; demote unless the diff bypasses Hasql.
- "Lazy `String` in hot path" → nhcore exports `Text`, not `String`; demote unless `String` appears.
- "Missing `pure`/`return` discipline" → `Task.yield`/`Result.ok` are the framework calls; demote.
