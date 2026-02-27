# NeoHaskell Effect System

**Status:** Exploration (not for v0.1)  
**Last updated:** 2026-02-25

---

## Context

Discussion about making side effects testable and potentially compilable to other targets (Python, JS). Conclusion: multi-target is future, testability is now.

---

## The Current Problem

1. `Task` is wrapper of `IO` + `ExceptT`
2. `IO` is not directly mockable
3. To test, you need patterns like ReaderT + service records
4. Integrations have lambdas (`onSuccess = \response -> ...`)

---

## What DOES Matter: Testability

### Goal (for Jess, non-technical)

```neohaskell
test "when user registers, welcome email is sent" =
  given
    [ UserExists { id = "user-1", email = "jess@example.com" } ]
  when
    RegisterUser { userId = "user-1" }
  therefore
    [ EmailSent { to = "jess@example.com", subject = "Welcome!" } ]
```

**No manual mocks.** No dependency setup. Just declarative.

---

## Clarified Model

```
Integration (Worker)
    │
    └── executes Tasks
            │
            └── Task = Primitive effect (extensible effects)
                    │
                    ├── Prod interpreter (real IO)
                    └── Test interpreter (mock + random gen)
```

- **Integration** ≠ **Task**
- Integration = worker that reacts to events and executes tasks
- Task = primitive effect with two interpreters

---

## Structure of a Primitive Effect

```haskell
-- 1. The effect (GADT)
data Http :: Effect where
  Post :: Url -> Body -> Http Response
  Get :: Url -> Http Response

-- 2. Prod interpreter
runHttpProd :: Http a -> IO a

-- 3. Test interpreter (deterministic)
runHttpPure :: [(Url, Response)] -> Eff effs a -> a

-- 4. Test interpreter (random / property-based)
runHttpGen :: Eff effs a -> Gen a

-- 5. Generators (MANDATORY)
genResponse :: Gen Response
genSuccessResponse :: Gen Response  
genErrorResponse :: Gen Response
```

**Rule:** If you define an effect, you MUST provide generators for testing.

---

## Property-Based Testing Integrated

```haskell
it "handles any valid sendgrid response" do
  property do
    given do
      user <- forAll genUser
    when \user -> do
      registerUser user.id
    -- Http.post receives RANDOM response from generator
    therefore \result -> do
      shouldEmit (EmailSent { to = user.email })
```

Effect generators produce random valid responses.

---

## Syntax Proposal (Future)

```neohaskell
-- Effect block (transforms to EffectList):
myFunction = {
  msg <- readLine
  printLine msg
}

-- Escape to specific monad:
pureComputation = Maybe.{
  x <- Just 5
  y <- Just 3
  pure (x + y)
}
```

---

## Summary

| Topic | Decision |
|-------|----------|
| Effects as monoids | Explore post-release |
| Multi-target (Python/JS) | Not priority |
| Testability without mocks | **YES, priority** |
| Mandatory generators | **YES** |
| Property-based testing | **YES, integrated** |
| Lambdas in integrations | Keep (with future lint) |
| Syntax `{ }` blocks | Prepare parser, implement post-release |

---

_Brainstorm document. Not final spec._
