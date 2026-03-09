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
| 4 | If-then-else for Bools | `if cond then a else b` | `case cond of True -> a; False -> b` |
| 5 | Descriptive type parameters | `forall element result.` | `forall a b.` |
| 6 | Qualified imports | `import Module qualified` | Unqualified module imports |
| 7 | NeoHaskell modules first | `import Array qualified` | `import Data.Vector qualified` |
| 8 | String interpolation | `[fmt\|Hello #{name}!\|]` | `"Hello " <> name <> "!"` |
| 9 | Result, not Either | `Result error value` | `Either error value` |
| 10 | Task, not IO | `Task err val` | `IO a` |
| 11 | Task.yield, not pure/return | `Task.yield value` | `pure value` or `return value` |
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

**Important**: Case expressions are for matching on **constructors** (`Ok`, `Err`, `Just`, `Nothing`, etc.), NOT for matching on `Bool` values. For `Bool`, use `if..then..else` (see Rule 4).

```haskell
-- CORRECT: case expression on constructors
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

## Rule 4: If-Then-Else for Bool Conditionals

Use `if..then..else` for Bool conditions. Never `case` match on `True`/`False`. For monadic (Task) conditionals, prefer `Task.when` and `Task.unless`.

```haskell
-- CORRECT: if-then-else for pure expressions
validate input =
  if Text.isEmpty input
    then Err EmptyInput
    else Ok (ValidatedInput input)

-- CORRECT: if-then-else inline
let scheme = if isSecure then "https://" else "http://"

-- CORRECT: Task.when for monadic conditionals
Task.when shouldNotify do
  Log.info [fmt|Sending notification for #{entityId}|]

-- CORRECT: Task.unless for negated monadic conditionals
Task.unless (Array.isEmpty errors) do
  Task.throw [fmt|Validation failed: #{errorsText}|]

-- WRONG: case matching on Bool
validate input =
  case Text.isEmpty input of
    True -> Err EmptyInput
    False -> Ok (ValidatedInput input)

-- WRONG: case on Bool in nested chains
case config.workerCapacity <= 0 of
  True -> Just "workerCapacity must be positive"
  False -> case config.timeoutMs <= 0 of
    True -> Just "timeoutMs must be positive"
    False -> Nothing
```

**Note**: `Task.when` and `Task.unless` are defined in `Task.hs` (part of nhcore). They are the preferred way to conditionally execute Task actions.

---

## Rule 5: Descriptive Type Parameters

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

## Rule 6: Qualified Import Convention

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

## Rule 7: NeoHaskell Modules First

**Always prefer nhcore modules over base/hackage equivalents.** NeoHaskell provides wrappers with friendly APIs. Only fall back to base/hackage when nhcore doesn't provide what you need.

### nhcore Module Map

| Need | Use (nhcore) | NOT (base/hackage) |
|------|-------------|-------------------|
| Strings | `Text`, `Text qualified` | `Data.Text` |
| Arrays/Lists | `Array`, `Array qualified` | `Data.Vector`, `Data.List` |
| Key-value maps | `Map`, `Map qualified` | `Data.Map.Strict` |
| Optional values | `Maybe`, `Maybe qualified` | `Data.Maybe` |
| Error handling | `Result`, `Result qualified` | `Data.Either` |
| IO with errors | `Task`, `Task qualified` | `Control.Monad.Trans.Except` |
| UUIDs | `Uuid`, `Uuid qualified` | `Data.UUID` |
| Binary data | `Bytes` | `Data.ByteString` |
| Sets | `Set`, `Set qualified` | `Data.Set` |
| JSON | `Json qualified` | `Data.Aeson` |
| HTTP | `Http.Client qualified` | `Network.HTTP.Client` |
| File I/O | `File qualified` | `System.IO` |
| Directories | `Directory qualified` | `System.Directory` |
| Paths | `Path qualified` | `System.FilePath` |
| Environment vars | `Environment qualified` | `System.Environment` |
| Concurrency (MVar) | `ConcurrentVar` | `Control.Concurrent.MVar` |
| Concurrency (Async) | `AsyncTask` | `Control.Concurrent.Async` |
| Channels | `Channel` | `Control.Concurrent.Chan` |
| Locks | `Lock` | `Control.Concurrent.MVar ()` pattern |
| Concurrent maps | `ConcurrentMap` | `Control.Concurrent.STM.TVar` |

### When nhcore Doesn't Have What You Need

If you need functionality that nhcore doesn't provide:

1. **Proceed with the base/hackage import** using the `Ghc` prefix convention (see Rule 6)
2. **Create a GitHub issue** requesting the nhcore wrapper, tagged with `enhancement` and assigned to the community lead
3. **Add a code comment** marking the import as a candidate for nhcore wrapping:

```haskell
-- TODO(nhcore): Replace with nhcore wrapper when available
-- See: https://github.com/neohaskell/NeoHaskell/issues/XXX
import Data.SomeGhcLib qualified as GhcSomeLib
```

This ensures the project incrementally grows its friendly API surface.

---

## Rule 8: String Interpolation with `[fmt|...|]`

Use the `fmt` quasi-quoter for all string construction. Interpolation uses `#{expression}` syntax. Never concatenate with `<>` or `++`.

```haskell
-- CORRECT: fmt quasi-quoter with #{} interpolation
greet name = [fmt|Hello #{name}!|]
logRetry count = [fmt|Retrying command (attempt #{count})|]
errorMessage id err = [fmt|Failed to process entity #{id}: #{err}|]

-- CORRECT: multi-variable interpolation
connectionInfo host port db = [fmt|#{host}:#{port}/#{db}|]

-- WRONG: String concatenation
greet name = "Hello " <> name <> "!"
logRetry count = "Retrying command (attempt " <> show count <> ")"

-- WRONG: Linked list concatenation
greet name = "Hello " ++ name ++ "!"

-- WRONG: Missing # in interpolation
greet name = [fmt|Hello {name}!|]  -- This does NOT interpolate!
```

The `fmt` quasi-quoter is imported from `Basics` (part of the NeoHaskell prelude).

---

## Rule 9: Result Over Either

NeoHaskell uses `Result error value` instead of `Either`. Constructors are `Ok` and `Err`.

```haskell
-- CORRECT: Result type
validate :: Text -> Result ValidationError ValidatedInput
validate input =
  if Text.isEmpty input
    then Err EmptyInput
    else Ok (ValidatedInput input)

-- CORRECT: Result helpers
Result.isOk result    -- check success
Result.isErr result   -- check failure
Result.withDefault fallback result  -- extract with default
Result.map transform result  -- transform success value

-- WRONG: Either type
validate :: Text -> Either ValidationError ValidatedInput
```

---

## Rule 10: Task Over IO

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

-- Imports: nhcore types first, then qualified nhcore modules, then GHC/external last
import Basics
import Array (Array)
import Array qualified
import Result (Result (..))
import Result qualified

-- GHC/base only when nhcore doesn't provide what's needed
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
| `Strict` | All fields/bindings strict by default | `~` needed for laziness; `!` is redundant |
| `OverloadedStrings` | String literals as Text | `"hello" :: Text` works |
| `OverloadedRecordDot` | Dot syntax for record fields | `entity.fieldName` instead of `fieldName entity` |
| `DuplicateRecordFields` | Same field names across types | Multiple types can have `id`, `name`, etc. |
| `QuasiQuotes` | Template Haskell quasi-quoters | `[fmt\|...\|]` syntax |
| `TypeFamilies` | Type-level functions | `EntityOf`, `EventOf`, `NameOf` |
| `DataKinds` | Type-level literals | `forall (value :: Type).` |
| `TypeApplications` | Explicit type application | `@MyType` syntax |
| `DeriveGeneric` | Generic deriving | `deriving (Generic)` |

**Critical**: Because of `NoImplicitPrelude`, every module must import what it needs from nhcore. Never import from `Prelude` directly (use `import Prelude qualified` if absolutely necessary for GHC compatibility).

**Critical**: Because of `Strict`, all fields are strict by default. Do NOT add redundant `!` bang patterns. Use `~` (tilde) to opt into laziness when needed.

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
toString (Ok value) = [fmt|Success: #{value}|]
toString (Err err) = [fmt|Error: #{err}|]

-- FIX: Use case expression
toString result =
  case result of
    Ok value -> [fmt|Success: #{value}|]
    Err err -> [fmt|Error: #{err}|]
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

-- FIX: Use fmt quasi-quoter with #{} interpolation
logMessage = [fmt|Processing entity #{entityId} with #{count} events|]
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

### Mistake 7: Importing from base/hackage instead of nhcore

```haskell
-- WRONG
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V
import Data.Either (Either (..))

-- FIX: Use nhcore modules
import Text (Text)
import Text qualified
import Array (Array)
import Array qualified
import Result (Result (..))
import Result qualified
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

### Mistake 9: Case-matching on Bool values

```haskell
-- WRONG
case Text.isEmpty input of
  True -> Err EmptyInput
  False -> Ok (ValidatedInput input)

-- FIX: Use if-then-else
if Text.isEmpty input
  then Err EmptyInput
  else Ok (ValidatedInput input)

-- WRONG: Nested case on Bool (deeply confusing)
case x <= 0 of
  True -> Just "must be positive"
  False -> case y <= 0 of
    True -> Just "must also be positive"
    False -> Nothing

-- FIX: Use if-then-else (or guards if complex)
if x <= 0
  then Just "must be positive"
  else if y <= 0
    then Just "must also be positive"
    else Nothing
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

### When to Use SPECIALIZE

```haskell
-- Polymorphic functions called in hot paths
fold :: forall element accumulator. (element -> accumulator -> accumulator) -> accumulator -> Array element -> accumulator
{-# SPECIALIZE fold :: (Event -> EntityState -> EntityState) -> EntityState -> Array Event -> EntityState #-}
```

**Note on Strict Fields**: The `Strict` extension is enabled project-wide, so all record fields and let-bindings are strict by default. Do NOT add redundant `!` bang patterns. Use `{-# UNPACK #-}` on primitive fields (`Int`, `Bool`, `Word`) in hot-path data types for unboxing. Use `~` (tilde) prefix when you explicitly need lazy evaluation.

---

## Reference Files

When in doubt about conventions, refer to these canonical source files:

| File | Demonstrates |
|------|-------------|
| `core/core/Text.hs` | Pipe chains, do-blocks, qualified imports |
| `core/core/Array.hs` | Collection operations, type parameters, do-blocks for pure code |
| `core/core/Result.hs` | Case expressions, error handling |
| `core/core/Task.hs` | Task.yield, Task.when/unless, error handling, do-blocks |
| `core/core/Basics.hs` | Pipe operators, fmt quasi-quoter |
| `core/test/IntSpec.hs` | Test structure, pipe assertions |
| `core/test-core/RedactedSpec.hs` | String interpolation in tests, Result assertions |
| `testbed/src/Testbed/Cart/Core.hs` | Entity/Event definitions, update function |
| `testbed/src/Testbed/Cart/Commands/CreateCart.hs` | Command implementation, Decider usage |
