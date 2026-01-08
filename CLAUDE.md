# CLAUDE.md

## Project Overview

NeoHaskell is a dialect of Haskell focused on newcomer-friendliness and productivity. This is a monorepo containing:

- **cli/**: Command line tool (`nhcli`) with executable `neo`
- **core/**: Core library (`nhcore`) with NeoHaskell's standard library modules
- **website/**: Documentation website built with Astro/Starlight

## Build Commands

```bash
cabal build all              # Build everything
cabal test                   # Run all tests
cabal test nhcore-test       # Core library tests only
cabal test nhcli-test        # CLI tests only
hlint .                      # Linter
./scripts/run-doctest        # Doctests
```

Formatting is automatic via fourmolu (2-space indent, 120 char limit).

---

## Workflow

### Before Writing ANY Code

1. **Read relevant files completely** - don't skim, don't read partial files
2. **Search for existing implementations** - reuse before creating
3. **Check `core/core/`** for NeoHaskell equivalents of what you need
4. **Ask if uncertain** - it's better to ask one question than make wrong assumptions

### TDD Process (Mandatory)

All implementation follows test-driven development:

1. **Write the complete test specification first** - define all requirements and expected behaviors
2. **Get approval on the test spec** before implementing
3. **Implement to make tests pass**
4. **Run `cabal build all && cabal test`** to verify

### After Changes

1. Run `cabal build all` - must compile
2. Run relevant tests - must pass
3. Run `hlint .` on modified files
4. Ensure Haddock comments and doctests exist for public functions

---

## NeoHaskell vs Haskell

**This is NOT standard Haskell. NeoHaskell has its own idioms.**

### Use NeoHaskell Modules, Not Base

The `core/core/` directory contains NeoHaskell's standard library that wraps/replaces base:

| Instead of (Haskell) | Use (NeoHaskell)     |
| -------------------- | -------------------- |
| `Data.List`          | `LinkedList`         |
| `Data.Text`          | `Text`               |
| `Data.Map`           | `Map`                |
| `Either a b`         | `Result error value` |
| `IO a`               | `Task error value`   |
| `Maybe a`            | `Maybe value`        |

If you need base/ecosystem functionality, check if nhcore already exposes it. When importing Haskell base modules is unavoidable, prefix with `Ghc`:

```haskell
import Data.List qualified as GhcList
```

### Naming Convention: Qualified Usage

Functions are designed for qualified import. Name them for how they'll be called:

```haskell
-- ❌ Wrong: redundant when qualified
EventStore.newEventStore
EventStore.getEventStoreEvents

-- ✅ Correct: clean when qualified
EventStore.new
EventStore.getEvents

-- Usage:
store <- EventStore.new
events <- store |> EventStore.getEvents
```

### Task, Not IO

All effectful code uses `Task err val` (isomorphic to `IO (Either err val)`):

```haskell
-- ❌ Wrong
readConfig :: IO Config

-- ✅ Correct
readConfig :: Task ConfigError Config
```

Stay within Task. Use Task's built-in error handling functions, not exceptions.

---

## Code Style

### Imports

Always explicit types + qualified modules:

```haskell
import Service.Event (Event(..))
import Service.Event qualified as Event
```

### Pipe Operator Over Nesting

```haskell
-- ❌ Wrong
foo $ bar $ baz x

-- ✅ Correct
x
  |> baz
  |> bar
  |> foo
```

### Explicit Application, No Point-Free

```haskell
-- ❌ Wrong
processItems = map transform . filter isValid

-- ✅ Correct
processItems items = do
  let validItems = items |> Array.filter isValid
  validItems |> Array.map transform
```

### Do Blocks for Bindings

Use `do` for intermediate bindings, even in pure code. No `let..in` or `where`:

```haskell
-- ❌ Wrong
calculate x =
  let y = x + 1
      z = y * 2
  in z + 3

-- ❌ Wrong
calculate x = z + 3
  where
    y = x + 1
    z = y * 2

-- ✅ Correct
calculate x = do
  let y = x + 1
  let z = y * 2
  z + 3
```

### Pattern Matching Only in Case Expressions

```haskell
-- ❌ Wrong
isEmpty [] = True
isEmpty _ = False

-- ✅ Correct
isEmpty list = case list of
  [] -> True
  _ -> False
```

### Type Parameters: Descriptive Names

```haskell
-- ❌ Wrong
map :: (a -> b) -> [a] -> [b]

-- ✅ Correct
map :: forall input output. (input -> output) -> Array input -> Array output
```

### String Interpolation

```haskell
-- ❌ Wrong
"Hello " ++ name ++ "!"
"Hello " <> name <> "!"

-- ✅ Correct
[fmt|Hello {name}!|]
```

### Return Values

```haskell
-- ❌ Wrong
pure value
return value

-- ✅ Correct
Task.yield value
Maybe.yield value
Result.ok value
```

---

## Forbidden Patterns

**Never do these:**

- ❌ Import Haskell ecosystem modules without checking nhcore first
- ❌ Use `let..in` or `where` clauses
- ❌ Pattern match in function definitions
- ❌ Point-free style
- ❌ Single-letter type parameters (`a`, `b`, `m`)
- ❌ Redundant prefixes in function names (`newEventStore` → `new`)
- ❌ Raw `IO` - use `Task` instead
- ❌ `Either` - use `Result` instead
- ❌ `pure` or `return` - use `<Type>.yield`
- ❌ String concatenation with `++` or `<>` - use `[fmt|...|]`
- ❌ Create new modules when existing abstractions can be extended

---

## Testing

Framework: **Hspec**

### Structure

```haskell
spec :: Spec
spec = do
  describe "ModuleName.functionName" do
    it "describes expected behavior" do
      functionName input `shouldBe` expectedOutput

    it "handles edge case" do
      functionName edgeInput `shouldBe` edgeOutput
```

### Documentation

All public functions need:

1. Haddock comment explaining purpose
2. Doctest example showing usage

```haskell
-- | Transforms a value using the provided function.
--
-- >>> transform (+1) 5
-- 6
transform :: forall input output. (input -> output) -> input -> output
transform f value = f value
```

---

## Architecture

### Module Organization

```
core/core/
├── Core types (Text, Int, Array, Maybe, Result, Task)
├── Traits (Mappable, Appendable, Combinable)
├── System (File, Directory, Path, Environment)
├── Concurrency (AsyncTask, Channel, Lock)
└── Service (EventStore, etc.)
```

### Decision Guide

| Need                   | Location       |
| ---------------------- | -------------- |
| New primitive type     | `Core/`        |
| New behavior/typeclass | `Traits/`      |
| IO/system operations   | `System/`      |
| Higher-level patterns  | `Service/`     |
| CLI commands           | `cli/src/Neo/` |

### Adding to nhcore

1. Add module to `core/nhcore.cabal` exposed-modules
2. Follow existing module structure as template
3. Include Haddock + doctests
4. Write Hspec tests first

---

## Error Handling

When builds fail:

1. Read the **complete** error message
2. Check for missing imports or type mismatches
3. Look at similar working code in the repo
4. Don't guess - understand the error

When tests fail:

1. Run the specific failing test: `cabal test --test-option=--match="/description/"`
2. Read the assertion output carefully
3. **Never modify test expectations without asking** - tests define requirements

When uncertain about NeoHaskell patterns:

- Search existing code in `core/` for examples
- Ask before deviating from established patterns

---

## Communication

**Be concise.** No walls of text.

**Be direct.** Don't explain what you're about to do - just do it.

**No sycophancy:**

- ❌ "You're absolutely right"
- ❌ "Great question"
- ❌ "That's a really interesting point"

**When you make a mistake:**

- State what went wrong: "I made an error - used LinkedList.map instead of Array.map"
- Explain briefly why
- Offer the fix
- Ask: "Should I revert and try again?"

**Ask questions when:**

- Requirements are ambiguous
- Multiple valid approaches exist
- You're about to change a public API
- You're unsure if nhcore has what you need

**Don't ask when:**

- The task is routine (formatting, standard imports)
- You can verify by reading existing code
- You can verify by running tests

---

## Common Mistakes to Avoid

1. **Using Haskell idioms instead of NeoHaskell**

   - Always check if nhcore has the function you need
   - Names differ: `fmap` → `map`, `pure` → `yield`, etc.

2. **Verbose function names**

   - Think about qualified usage: `Module.function`
   - `EventStore.new` not `EventStore.newEventStore`

3. **Creating new modules unnecessarily**

   - First: can this extend an existing module?
   - Second: can this use existing abstractions?
   - Last resort: new module

4. **Writing implementation before tests**

   - Always spec first, implement second
   - Get approval on test spec before coding

5. **Partial file reads leading to broken code**
   - Read entire files when you need context
   - Don't optimize for tokens at the cost of correctness

---

## Git

- Conventional commits: `feat:`, `fix:`, `docs:`, `refactor:`, `test:`
- Work on current branch
- Run full build + tests before committing
- One logical change per commit
