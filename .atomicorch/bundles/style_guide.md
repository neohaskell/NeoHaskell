---
id: style_guide
target_max_tokens: 2000
---

# NeoHaskell Style Enforcement Guide

Each rule below includes the grep pattern that detects violations. All checks run against `**/*.hs` files. A passing codebase returns ∅ for every command.

## Rule 1: Pipe over nesting

Dollar-sign application is forbidden.

```
Rule 1 (Pipe over nesting):
  grep -rE '\$\s+\w' --include='*.hs' must return ∅
  grep -rn '\$' --include='*.hs' | grep -v '^\s*--' must return ∅
```

## Rule 2: Do + let for bindings

`let..in` expressions and `where` clauses are forbidden.

```
Rule 2a (No let..in):
  grep -rn '\blet\b.*\bin\b' --include='*.hs' must return ∅

Rule 2b (No where):
  grep -rn '^\s*where\s*$' --include='*.hs' must return ∅
  grep -rn '\bwhere\b' --include='*.hs' must return ∅
```

## Rule 3: Case-only for pattern match

Function-level pattern matching (multiple equations for same function) is forbidden.

```
Rule 3 (No multi-equation functions):
  # Detect back-to-back function definitions with same name at column 0
  grep -rPn '^(\w+)\s+[^\s=].*=\n\1\s' --include='*.hs' must return ∅
```

## Rule 4: If-then-else for Booleans

`case expr of { True -> ...; False -> ... }` pattern is forbidden.

```
Rule 4 (No bool case):
  grep -rn 'case.*of' --include='*.hs' -A2 | grep -E 'True\s*->|False\s*->' must return ∅
```

## Rule 5: Descriptive type parameters

Single-letter type variables in `forall` are forbidden.

```
Rule 5 (No single-letter type vars):
  grep -rn 'forall [a-z]\.' --include='*.hs' must return ∅
  grep -rn ':: [a-z] ->' --include='*.hs' must return ∅
```

## Rule 6: Qualified imports

Unqualified imports of non-prelude modules are forbidden.

```
Rule 6 (No unqualified imports):
  grep -rn '^import\s\+[^q]' --include='*.hs' | grep -v 'qualified' | grep -v 'NeoHaskell.Prelude' must return ∅
```

## Rule 7: nhcore before base

Direct imports from `Data.*`, `System.*`, or `Control.*` without nhcore wrapper are flagged.

```
Rule 7 (nhcore before base):
  grep -rn '^import\s.*\bData\.' --include='*.hs' must return ∅
  grep -rn '^import\s.*\bSystem\.IO\b' --include='*.hs' must return ∅
  grep -rn '^import\s.*\bControl\.Monad\b' --include='*.hs' must return ∅
```

## Rule 8: Interpolation with [fmt|...|]

String concatenation with `<>` for message construction is forbidden.

```
Rule 8 (No string concat for messages):
  grep -rn '".*"\s*<>' --include='*.hs' must return ∅
  grep -rn '<>\s*"' --include='*.hs' must return ∅
```

## Rule 9: Result, not Either

`Either` type usage in signatures is forbidden.

```
Rule 9 (No Either):
  grep -rn '\bEither\b' --include='*.hs' must return ∅
```

## Rule 10: Task, not IO

`IO` in type signatures is forbidden in application code (permitted only in `Main.hs` entry points).

```
Rule 10 (No IO):
  grep -rn '::\s*IO\b' --include='*.hs' | grep -v 'Main.hs' must return ∅
  grep -rn '->\s*IO\b' --include='*.hs' | grep -v 'Main.hs' must return ∅
```

## Rule 11: Task.yield, not pure/return

`pure` and `return` as Task lifts are forbidden.

```
Rule 11 (No pure/return):
  grep -rn '\bpure\b' --include='*.hs' | grep -v '^\s*--' must return ∅
  grep -rn '\breturn\b' --include='*.hs' | grep -v '^\s*--' must return ∅
```

## Rule 12: INLINE in hot paths

Every function in `critical_areas` directories must have an `INLINE` pragma.

```
Rule 12 (INLINE in hot paths):
  # Manual review required for core/auth/, core/http/, core/crypto/,
  # core/eventstore/, core/service/Service/Command/
  # Automated: flag public functions in those dirs missing INLINE
  grep -rLZ '{-# INLINE' core/auth/*.hs core/http/*.hs core/crypto/*.hs must return ∅
```

---

## Quick Reference: Allowed vs Forbidden

| Construct | Status |
|-----------|--------|
| `x \|> f \|> g` | ALLOWED |
| `g $ f $ x` | FORBIDDEN |
| `do { let x = ... }` | ALLOWED |
| `let x = ... in ...` | FORBIDDEN |
| `where` clause | FORBIDDEN |
| `case expr of` (constructors) | ALLOWED |
| `case bool of { True -> ... }` | FORBIDDEN |
| `forall element result.` | ALLOWED |
| `forall a b.` | FORBIDDEN |
| `import X qualified` | ALLOWED |
| `import X (foo, bar)` | FORBIDDEN |
| `Result error value` | ALLOWED |
| `Either left right` | FORBIDDEN |
| `Task error value` | ALLOWED |
| `IO a` | FORBIDDEN |
| `Task.yield x` | ALLOWED |
| `pure x` / `return x` | FORBIDDEN |
| `[fmt\|msg {var}\|]` | ALLOWED |
| `"prefix " <> show x` | FORBIDDEN |
