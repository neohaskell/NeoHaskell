---
name: neohaskell-style-guide
description: "NeoHaskell coding style reference and enforcement rules. Load when writing, reviewing, or modifying any Haskell code in the NeoHaskell project. Triggers: 'NeoHaskell style', 'NeoHaskell conventions', 'how to write NeoHaskell', 'code style', 'style guide'. Also load when implementing any NeoHaskell feature or reviewing NeoHaskell code."
tags:
  - haskell
  - neohaskell
  - style
  - conventions
  - code-quality
---

# NeoHaskell Style Guide

This is the authoritative coding style reference for the NeoHaskell project. NeoHaskell is NOT standard Haskell — it has its own conventions that differ significantly. Violations of these rules are hard failures.

## Quick Reference: Syntax Rules

| # | Rule | Correct | Wrong |
|---|------|---------|-------|
| 1 | Pipe operator over nesting | `x \|> foo \|> bar` | `bar $ foo x` or `bar (foo x)` |
| 2 | Do blocks with let bindings | `do let y = expr` | `let y = expr in ...` or `where y = expr` |
| 3 | Case expressions only | `case x of { ... }` | Pattern matching in function definitions |
| 4 | Descriptive type parameters | `forall element result.` | `forall a b.` |
| 5 | Qualified imports | `import Module qualified` | Unqualified module imports |
| 6 | String interpolation | `[fmt\|Hello {name}!\|]` | `"Hello " <> name <> "!"` |
| 7 | Result, not Either | `Result error value` | `Either error value` |
| 8 | Task, not IO | `Task err val` | `IO a` |
| 9 | Task.yield, not pure/return | `Task.yield value` | `pure value` or `return value` |
| 10 | Forward composition | `f .> g` | `g . f` |
| 11 | Strict fields on hot paths | `!fieldName` | Lazy fields in hot-path data types |
| 12 | INLINE pragmas on hot paths | `{-# INLINE fn #-}` | Missing INLINE on hot functions |

---

## Rule 1: Pipe Operator (`|>`) Over Nesting

Data flows left-to-right through transformations. Never use `$` for nesting.

```haskell
-- CORRECT: Left-to-right data flow
escapeHtml text = text
  |> replace "&" "&amp;"
  |> replace "<" "&lt;"
  |> replace ">" "&gt;"
  |> replace "\"" "&quot;"
  |> replace "'" "&#x27;"

-- CORRECT: Pipeline with intermediate steps
processItems cart = cart.items
  |> Array.filter (\item -> item.quantity > 0)
  |> Array.map (\item -> item.price * Int.toFloat item.quantity)
  |> Array.foldl (+) 0.0

-- WRONG: Dollar-sign nesting
escapeHtml text = replace "'" "&#x27;" $ replace "\"" "&quot;" $ replace ">" "&gt;" text

-- WRONG: Parenthetical nesting
processItems cart = Array.foldl (+) 0.0 (Array.map (\item -> item.price) (Array.filter isActive cart.items))
```

---

## Rule 2: Do-Blocks for ALL Intermediate Bindings

Use `do` with `let` for all intermediate bindings — even in pure (non-monadic) code. Never use `let..in` or `where`.

```haskell
-- CORRECT: do-block for pure code
range lo hi = do
  let values = enumFromTo lo hi
  Array values

-- CORRECT: do-block with multiple bindings
partitionBy predicate (Array vector) = do
  let (matching, nonMatching) = Data.Vector.partition predicate vector
  (Array matching, Array nonMatching)

-- CORRECT: do-block in monadic code
execute command entityId = do
  entity <- entityFetcher.fetch entityId
  let result = Decider.runDecision (decide command entity)
  eventStore.insert result

-- WRONG: let..in
range lo hi =
  let values = enumFromTo lo hi
  in Array values

-- WRONG: where clause
partitionBy predicate (Array vector) = (Array matching, Array nonMatching)
  where
    (matching, nonMatching) = Data.Vector.partition predicate vector
```

**Exception**: Guards in pattern matching are acceptable but rare. Prefer `case` expressions.

---

## Rule 3: Case Expressions Only for Pattern Matching

All pattern matching happens in `case..of`. Never pattern match in function definitions.

```haskell
-- CORRECT: case expression
withDefault fallback result =
  case result of
    Ok value -> value
    Err _ -> fallback

-- CORRECT: nested case
getOrDie maybe =
  case maybe of
    Just value -> value
    Nothing -> panic "Maybe.getOrDie: Got Nothing"

-- CORRECT: case in do-block
processResult action = do
  result <- action
  case result of
    Ok value -> Task.yield value
    Err err -> Task.throw err

-- WRONG: pattern matching in function definition
withDefault fallback (Ok value) = value
withDefault fallback (Err _) = fallback

-- WRONG: function definition pattern matching
getOrDie (Just value) = value
getOrDie Nothing = panic "Got Nothing"
```

---

## Rule 4: Descriptive Type Parameters

Type parameters must be descriptive. Never use single letters.

```haskell
-- CORRECT: descriptive names
map :: forall element result. (element -> result) -> Array element -> Array result
dropWhile :: forall value. (value -> Bool) -> Array value -> Array value
foldl :: forall element accumulator. (element -> accumulator -> accumulator) -> accumulator -> Array element -> accumulator

-- CORRECT: with Kind signatures
partitionBy :: forall (value :: Type). (value -> Bool) -> Array value -> (Array value, Array value)

-- WRONG: single-letter parameters
map :: forall a b. (a -> b) -> Array a -> Array b
foldl :: forall a b. (a -> b -> b) -> b -> Array a -> b
```

---

## Rule 5: Qualified Import Convention

Import types unqualified, then import the module qualified. GHC base modules get a `Ghc` prefix.

```haskell
-- CORRECT: Type unqualified, module qualified
import Array (Array)
import Array qualified
import Result (Result (..))
import Result qualified
import Maybe (Maybe (..))
import Maybe qualified

-- CORRECT: GHC/base modules with Ghc prefix or full path
import Control.Monad qualified
import Data.Vector qualified
import Data.Text qualified
import GHC.IsList qualified as GHC
import Text.Read qualified

-- CORRECT: External libraries qualified with full name
import Data.Aeson qualified as Json
import Data.Default (Default (..))
import Test.QuickCheck qualified as QuickCheck

-- WRONG: Unqualified module import
import Array
import Data.Text

-- WRONG: Short aliases
import Array qualified as A
import Data.Text qualified as T
```

---

## Rule 6: String Interpolation with `[fmt|...|]`

Use the `fmt` quasi-quoter for all string construction. Never concatenate with `<>` or `++`.

```haskell
-- CORRECT: fmt quasi-quoter
greet name = [fmt|Hello {name}!|]
logRetry count = [fmt|Retrying command (attempt #{count})|]
errorMessage id err = [fmt|Failed to process entity {id}: {err}|]

-- WRONG: String concatenation
greet name = "Hello " <> name <> "!"
logRetry count = "Retrying command (attempt " <> show count <> ")"

-- WRONG: Linked list concatenation
greet name = "Hello " ++ name ++ "!"
```

The `fmt` quasi-quoter is imported from `Basics` (part of the NeoHaskell prelude).

---

## Rule 7: Result Over Either

NeoHaskell uses `Result error value` instead of `Either`. Constructors are `Ok` and `Err`.

```haskell
-- CORRECT: Result type
validate :: Text -> Result ValidationError ValidatedInput
validate input =
  case Text.isEmpty input of
    True -> Err EmptyInput
    False -> Ok (ValidatedInput input)

-- CORRECT: Result helpers
Result.isOk result    -- check success
Result.isErr result   -- check failure
Result.withDefault fallback result  -- extract with default
Result.map transform result  -- transform success value

-- WRONG: Either type
validate :: Text -> Either ValidationError ValidatedInput
```

---

## Rule 8: Task Over IO

NeoHaskell uses `Task err val` instead of `IO`. Use `Task.yield` to return values.

```haskell
-- CORRECT: Task with yield
readConfig :: Task ConfigError Config
readConfig = do
  contents <- File.readText "config.json"
  case Json.decode contents of
    Ok config -> Task.yield config
    Err err -> Task.throw (ParseError err)

-- WRONG: IO with pure/return
readConfig :: IO Config
readConfig = do
  contents <- readFile "config.json"
  pure (parseConfig contents)
```

---

## Rule 9: Forward Composition (`.>`) and Backward Composition (`<.`)

Use `.>` for left-to-right function composition and `<.` for right-to-left. These are defined in `Basics.hs`.

```haskell
-- CORRECT: Forward composition (.>)
length = unwrap .> Data.Vector.length
isEmpty = unwrap .> Data.Vector.null
fromInt = Prelude.show .> Data.Text.pack

-- CORRECT: Backward composition (<.)
fromInt = Data.Text.pack <. Prelude.show

-- WRONG: Standard Haskell composition
length = Data.Vector.length . unwrap
```

**Note**: Composition operators are for defining simple pipelines. For complex logic, use explicit `do` blocks with `let` bindings and `|>` pipes.

---

## Module Conventions

### Export Through Core

If a type is foundational, it should be re-exported from the `Core` module. Users should never need to import implementation modules directly.

### Module File Structure

```haskell
module ModuleName
  ( -- * Types
    MyType (..)
    -- * Construction
  , new
  , empty
    -- * Operations
  , transform
  , combine
  )
where

-- Imports: types first, then qualified modules
import Basics
import OtherType (OtherType)
import OtherType qualified
import Data.SomeGhcLib qualified

-- Implementation
```

### Multi-Source-Dir Package

The `nhcore` package uses multiple `hs-source-dirs` in the cabal file (core, json, schema, service, etc.). New modules must be placed in the correct source directory and listed in `nhcore.cabal`.

### Qualified Module Design

Modules are designed to be used qualified: `EventStore.new`, `Array.map`, `Result.ok`. Public API functions should have clear, unambiguous names when qualified.

---

## Test Conventions

### Test Structure

```haskell
module MyModuleSpec (spec) where

import Core
import Test

spec :: Spec Unit
spec = do
  describe "MyModule" do
    describe "functionName" do
      it "describes expected behavior" \_ -> do
        input |> MyModule.functionName |> shouldBe expectedOutput

      it "handles edge case" \_ -> do
        edgeInput |> MyModule.functionName |> shouldSatisfy isValid
```

### Test Rules

1. **Tests are IMMUTABLE** — once written, never modify test expectations during implementation
2. **`spec :: Spec Unit`** return type
3. **`\_ ->` for unused test parameter** in `it` blocks
4. **`|>` pipes in assertions** — `value |> shouldBe expected`
5. **`shouldBe`** for equality, **`shouldSatisfy`** for predicates
6. **`Result.isOk` / `Result.isErr`** with `shouldSatisfy` for Result assertions
7. **`Test.fail [fmt|message|]`** for custom failure messages

### Required Test Categories

- **Unit tests**: Happy path for each public function
- **Edge cases**: Empty inputs, boundary values, error conditions
- **Serialization round-trips**: `decode (encode x) == x` for all serializable types
- **Property-based tests**: QuickCheck for invariants where appropriate

### Test Registration

- `nhcore-test` uses `hspec-discover` — just add `*Spec.hs` files under `core/test/`
- `nhcore-test-service` uses manual registration in `core/test-service/Main.hs` — new spec modules must be imported and added to the `Hspec.describe` list
- All test modules must be listed in `other-modules` in `core/nhcore.cabal`

---

## GHC Extensions in Use

The project uses these GHC extensions project-wide (set in `nhcore.cabal`):

| Extension | What It Enables | Impact on Code |
|-----------|----------------|----------------|
| `NoImplicitPrelude` | No automatic Prelude import | Must import from nhcore (`Basics`, `Core`) |
| `OverloadedStrings` | String literals as Text | `"hello" :: Text` works |
| `OverloadedRecordDot` | Dot syntax for record fields | `entity.fieldName` instead of `fieldName entity` |
| `DuplicateRecordFields` | Same field names across types | Multiple types can have `id`, `name`, etc. |
| `QuasiQuotes` | Template Haskell quasi-quoters | `[fmt\|...\|]` syntax |
| `TypeFamilies` | Type-level functions | `EntityOf`, `EventOf`, `NameOf` |
| `DataKinds` | Type-level literals | `forall (value :: Type).` |
| `TypeApplications` | Explicit type application | `@MyType` syntax |
| `DeriveGeneric` | Generic deriving | `deriving (Generic)` |

**Critical**: Because of `NoImplicitPrelude`, every module must import what it needs from nhcore. Never import from `Prelude` directly (use `import Prelude qualified` if absolutely necessary for GHC compatibility).

---

## Common Mistakes and Fixes

### Mistake 1: Using `where` clauses

```haskell
-- WRONG
processOrder order = total
  where
    items = order.lineItems
    total = Array.foldl (+) 0 items

-- FIX: Use do-block
processOrder order = do
  let items = order.lineItems
  let total = Array.foldl (+) 0 items
  total
```

### Mistake 2: Pattern matching in function head

```haskell
-- WRONG
toString (Ok value) = [fmt|Success: {value}|]
toString (Err err) = [fmt|Error: {err}|]

-- FIX: Use case expression
toString result =
  case result of
    Ok value -> [fmt|Success: {value}|]
    Err err -> [fmt|Error: {err}|]
```

### Mistake 3: Single-letter type parameters

```haskell
-- WRONG
mapResult :: forall a b e. (a -> b) -> Result e a -> Result e b

-- FIX: Descriptive names
mapResult :: forall value result error. (value -> result) -> Result error value -> Result error result
```

### Mistake 4: Using `pure` or `return`

```haskell
-- WRONG
fetchEntity entityId = do
  entity <- store.get entityId
  pure entity

-- FIX: Use Task.yield
fetchEntity entityId = do
  entity <- store.get entityId
  Task.yield entity
```

### Mistake 5: String concatenation

```haskell
-- WRONG
logMessage = "Processing entity " <> entityId <> " with " <> show count <> " events"

-- FIX: Use fmt quasi-quoter
logMessage = [fmt|Processing entity {entityId} with {count} events|]
```

### Mistake 6: Orphan instances

```haskell
-- WRONG: Defining instance in a different module than the type
-- File: Serialization.hs
instance Json.ToJSON MyType where ...  -- MyType is defined in Types.hs

-- FIX: Define instance in the same module as the type
-- File: Types.hs (where MyType is defined)
instance Json.ToJSON MyType where ...
```

### Mistake 7: Importing from Prelude directly

```haskell
-- WRONG
import Prelude (show, map, filter)

-- FIX: Use nhcore equivalents
import Basics
import Array qualified
-- Use Array.map, Array.filter, toText (instead of show)
```

### Mistake 8: Using `$` instead of `|>`

```haskell
-- WRONG
result = Array.map transform $ Array.filter predicate items

-- FIX: Use pipe operator
result = items
  |> Array.filter predicate
  |> Array.map transform
```

---

## Deriving Conventions

```haskell
-- Standard derives for data types
data MyType = MyType
  { field1 :: Text
  , field2 :: Int
  }
  deriving (Eq, Show, Generic)

-- JSON instances (prefer generic)
instance Json.ToJSON MyType
instance Json.FromJSON MyType

-- For newtypes, use deriving via or newtype deriving
newtype Wrapper = Wrapper { unwrap :: Text }
  deriving (Eq, Show)
  deriving newtype (Json.ToJSON, Json.FromJSON)
```

---

## Performance Annotations

### When to Use INLINE

```haskell
-- Add INLINE to small, frequently-called functions in hot paths
yield :: value -> Task _ value
yield value = Task (Applicable.pure value)
{-# INLINE yield #-}

-- Add INLINE to accessor functions
getOrDie :: Maybe value -> value
getOrDie maybe = case maybe of
  Just value -> value
  Nothing -> panic "Got Nothing"
{-# INLINE getOrDie #-}
```

### When to Use Strict Fields

```haskell
-- Hot-path data types: use strict fields
data CommandExecutor = CommandExecutor
  { !eventStore :: EventStore
  , !entityFetcher :: EntityFetcher
  , !maxRetries :: Int
  }

-- Cold-path data types: lazy is fine
data Config = Config
  { appName :: Text
  , port :: Int
  }
```

### When to Use SPECIALIZE

```haskell
-- Polymorphic functions called in hot paths
fold :: forall element accumulator. (element -> accumulator -> accumulator) -> accumulator -> Array element -> accumulator
{-# SPECIALIZE fold :: (Event -> EntityState -> EntityState) -> EntityState -> Array Event -> EntityState #-}
```

---

## Reference Files

When in doubt about conventions, refer to these canonical source files:

| File | Demonstrates |
|------|-------------|
| `core/core/Text.hs` | Pipe chains, do-blocks, qualified imports, composition |
| `core/core/Array.hs` | Collection operations, type parameters, do-blocks for pure code |
| `core/core/Result.hs` | Case expressions, error handling |
| `core/core/Task.hs` | Task.yield, error handling, do-blocks |
| `core/core/Basics.hs` | Pipe operators, fmt quasi-quoter, composition operators |
| `core/test/IntSpec.hs` | Test structure, pipe assertions |
| `core/test-core/RedactedSpec.hs` | String interpolation in tests, Result assertions |
| `testbed/src/Testbed/Cart/Core.hs` | Entity/Event definitions, update function |
| `testbed/src/Testbed/Cart/Commands/CreateCart.hs` | Command implementation, Decider usage |
