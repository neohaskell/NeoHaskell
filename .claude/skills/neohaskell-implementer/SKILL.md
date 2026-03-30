---
name: neohaskell-implementer
description: NeoHaskell code implementation guide. Use when implementing features, writing tests, build/test loops, or any task requiring NeoHaskell code. Handles pipeline phases 7-9 (tests, implementation, build loop), 12-13 (fix reviews, final build), and 16 (fix bot comments).
---

# NeoHaskell Implementer

You are implementing code for the NeoHaskell project. Every line of code must follow NeoHaskell conventions exactly. Your code must be indistinguishable from what the project maintainer would write.

## Operating Principles

1. **Follow the architecture document exactly** — execute, not redesign
2. **Reuse what exists** — before writing ANY utility, check if nhcore provides it
3. **Follow the style guide** — the style guide is law
4. **Minimal changes only** — no bonus refactors, no "while I'm here" improvements

## Reuse-First Rule (CRITICAL)

Before writing any helper function:
1. Search nhcore — grep for existing functions
2. Check the architecture document — it lists specific utilities to use
3. Check `Core.hs` re-exports
4. Only if nothing exists — then write it, following existing patterns

## NeoHaskell Code Style (MANDATORY)

### Syntax Rules

| # | Rule | Correct | Wrong |
|---|------|---------|-------|
| 1 | Pipe operator | `x \|> foo \|> bar` | `bar $ foo x` |
| 2 | Do-blocks for bindings | `do let y = expr` | `let y = expr in ...` |
| 3 | Case expressions only | `case x of { ... }` | Pattern matching in function head |
| 4 | Descriptive type params | `forall element result.` | `forall a b.` |
| 5 | Qualified imports | `import Module qualified` | Unqualified imports |
| 6 | String interpolation | `[fmt\|Hello #{name}!\|]` | `"Hello " <> name` |
| 7 | Result, not Either | `Result error value` | `Either error value` |
| 8 | Task, not IO | `Task err val` | `IO a` |
| 9 | Task.yield | `Task.yield value` | `pure` / `return` |
| 10 | If-then-else for Bools | `if cond then a else b` | `case cond of True -> ...` |

### Import Convention

```haskell
-- Type unqualified, module qualified
import Array (Array)
import Array qualified
import Result (Result (..))
import Result qualified

-- GHC/base modules: qualified with full path
import Data.Text qualified
import Data.Aeson qualified as Json
```

### Test Convention

```haskell
spec :: Spec Unit
spec = do
  describe "ModuleName" do
    describe "functionName" do
      it "describes expected behavior" \_ -> do
        input |> ModuleName.functionName |> shouldBe expected
```

### Performance Annotations

- `{-# INLINE fn #-}` on small, hot-path functions
- `{-# UNPACK #-}` on primitive fields in hot-path types
- `toEncoding` over `toJSON` for Aeson on hot paths

## Phase 7: Test Suite Writing

1. Read test specification from Phase 6
2. Create stub type definitions (enough for tests to compile)
3. Translate each test case to Hspec code — follow spec EXACTLY
4. Register tests in `nhcore.cabal`
5. Verify tests compile: `cabal build all`
6. Verify all tests fail (no implementation yet)

## Phase 8: Implementation

1. Read architecture document and test files
2. Implement one module at a time
3. After each module: `cabal build all`
4. Follow all style rules
5. DO NOT modify test files

### Common Pitfalls

- **Orphan instances**: Define typeclass instances with the type
- **TH staging**: Splices must come AFTER definitions they reference
- **Missing cabal entries**: New modules must be in `nhcore.cabal`
- **NoImplicitPrelude**: Import everything from nhcore

## Phase 9: Build & Test Loop

1. Run `cabal build all`
2. If compilation fails — fix root cause, re-run
3. If compilation succeeds — run `cabal test`
4. If tests fail — fix IMPLEMENTATION (never tests), re-run
5. When all pass — run `hlint` on changed files
6. Fix any hlint warnings

### Circuit Breaker

**Maximum 10 iterations.**

After 10 iterations with failures:
1. STOP all edits
2. Document what you've tried
3. Report specific errors with file:line references
4. Do NOT keep trying random fixes

### Self-Review Checklist

Before reporting completion:
- [ ] No `$` operator (use `|>`)
- [ ] No `let..in` or `where` (use `do`)
- [ ] No pattern matching in function definitions (use `case`)
- [ ] No single-letter type parameters
- [ ] No `pure` or `return` (use `Task.yield`)
- [ ] No `Either` (use `Result`)
- [ ] No string concatenation (use `[fmt|...|]`)
- [ ] INLINE pragmas on hot-path functions
- [ ] Test files UNMODIFIED

## Phase 12: Fix Review Notes

1. Read security review — address every Critical/High finding
2. Read performance review — add INLINE, strict fields, fix allocations
3. For each finding: understand root cause, implement minimal fix
4. Run `cabal build all && cabal test` after all fixes
5. If fix touches security/hot-path code, note for re-review

## Phase 16: Fix Bot Comments

1. Read bot comments: `gh api repos/neohaskell/NeoHaskell/pulls/{PR}/comments`
2. For each comment: understand concern, implement fix
3. Run `cabal build all && cabal test`
4. Commit and push
5. Maximum 5 fix-and-push cycles — if CI still fails, escalate

## Event-Sourcing Patterns

### New Command

```haskell
data MyCommand = MyCommand
  { entityId :: Uuid
  , someField :: Text
  }
  deriving (Eq, Show, Generic)

instance Json.ToJSON MyCommand
instance Json.FromJSON MyCommand

-- TH macro (define endpoint handlers BEFORE this)
command "MyCommand" ''MyCommand ''MyEntity
  [ 'someEndpoint
  ]

decide :: MyCommand -> Maybe MyEntity -> RequestContext -> Decision MyEvent
decide cmd entity _ctx =
  case entity of
    Just _ -> Decider.reject "Entity already exists"
    Nothing -> Decider.acceptNew [MyEventCreated {entityId = cmd.entityId}]
```

### New Entity

```haskell
data MyEntity = MyEntity
  { entityId :: !Uuid
  , someField :: !Text
  }
  deriving (Eq, Show, Generic)

data MyEvent
  = MyEventCreated { entityId :: Uuid }
  | MyEventUpdated { someField :: Text }
  deriving (Eq, Show, Generic)

-- Type families
type instance EntityOf MyCommand = MyEntity
type instance EventOf MyEntity = MyEvent
```

## Red Lines (NEVER Do These)

1. NEVER modify test expectations
2. NEVER use `$` — use `|>`
3. NEVER use `let..in` or `where` — use `do`
4. NEVER use `Either` — use `Result`
5. NEVER use `pure` or `return`
6. NEVER use single-letter type parameters
7. NEVER import from `Prelude` directly
8. NEVER create orphan instances
9. NEVER continue after 10 failed iterations — report and stop
10. NEVER refactor while fixing — minimal changes only
