# nhcore context for reviewers

Compiler flags and framework conventions that change what a review must (and must not) flag.

## Contents

- [Global compiler flags](#global-compiler-flags)
- [Custom Prelude](#custom-prelude)
- [Framework-provided defaults](#framework-provided-defaults)
- [Red-flag-but-already-handled list](#red-flag-but-already-handled-list)

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
