---
name: neohaskell-style-guide
description: NeoHaskell coding style reference and enforcement rules. Load when writing, reviewing, or modifying any Haskell code in the NeoHaskell project. Triggers on 'NeoHaskell style', 'NeoHaskell conventions', 'how to write NeoHaskell', 'code style', 'style guide'.
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

## Rule 1: Pipe Operator (`|>`) Over Nesting

Data flows left-to-right through transformations. Never use `$` for nesting.

```haskell
-- CORRECT: Left-to-right data flow
escapeHtml text = text
  |> replace "&" "&amp;"
  |> replace "<" "&lt;"
  |> replace ">" "&gt;"

-- WRONG: Dollar-sign nesting
escapeHtml text = replace ">" "&gt;" $ replace "<" "&lt;" $ replace "&" "&amp;" text
```

## Rule 2: Do-Blocks for ALL Intermediate Bindings

Use `do` with `let` for all intermediate bindings — even in pure (non-monadic) code. Never use `let..in` or `where`.

```haskell
-- CORRECT: do-block for pure code
range lo hi = do
  let values = enumFromTo lo hi
  Array values

-- WRONG: let..in
range lo hi =
  let values = enumFromTo lo hi
  in Array values

-- WRONG: where clause
range lo hi = Array values
  where values = enumFromTo lo hi
```

## Rule 3: Case Expressions Only for Pattern Matching

All pattern matching happens in `case..of`. Never pattern match in function definitions.

```haskell
-- CORRECT: case expression on constructors
withDefault fallback result =
  case result of
    Ok value -> value
    Err _ -> fallback

-- WRONG: pattern matching in function definition
withDefault fallback (Ok value) = value
withDefault fallback (Err _) = fallback
```

## Rule 4: If-Then-Else for Bool Conditionals

Use `if..then..else` for Bool conditions. Never `case` match on `True`/`False`.

```haskell
-- CORRECT: if-then-else
validate input =
  if Text.isEmpty input
    then Err EmptyInput
    else Ok (ValidatedInput input)

-- WRONG: case matching on Bool
validate input =
  case Text.isEmpty input of
    True -> Err EmptyInput
    False -> Ok (ValidatedInput input)
```

## Rule 5: Descriptive Type Parameters

Type parameters must be descriptive. Never use single letters.

```haskell
-- CORRECT: descriptive names
map :: forall element result. (element -> result) -> Array element -> Array result
foldl :: forall element accumulator. (element -> accumulator -> accumulator) -> accumulator -> Array element -> accumulator

-- WRONG: single-letter parameters
map :: forall a b. (a -> b) -> Array a -> Array b
```

## Rule 6: Qualified Import Convention

Import types unqualified, then import the module qualified.

```haskell
-- CORRECT: Type unqualified, module qualified
import Array (Array)
import Array qualified
import Result (Result (..))
import Result qualified

-- CORRECT: GHC/base modules with full path
import Data.Vector qualified
import Control.Monad qualified

-- WRONG: Unqualified module import
import Array
import Data.Text
```

## Rule 7: NeoHaskell Modules First

Always prefer nhcore modules over base/hackage equivalents.

| Need | Use (nhcore) | NOT (base/hackage) |
|------|-------------|-------------------|
| Strings | `Text` | `Data.Text` |
| Arrays/Lists | `Array` | `Data.Vector`, `Data.List` |
| Key-value maps | `Map` | `Data.Map.Strict` |
| Optional values | `Maybe` | `Data.Maybe` |
| Error handling | `Result` | `Data.Either` |
| IO with errors | `Task` | `IO` |
| UUIDs | `Uuid` | `Data.UUID` |
| Binary data | `Bytes` | `Data.ByteString` |

## Rule 8: String Interpolation with `[fmt|...|]`

Use the `fmt` quasi-quoter for all string construction. Interpolation uses `#{expression}` syntax.

```haskell
-- CORRECT: fmt quasi-quoter with #{} interpolation
greet name = [fmt|Hello #{name}!|]
logRetry count = [fmt|Retrying command (attempt #{count})|]

-- WRONG: String concatenation
greet name = "Hello " <> name <> "!"
```

## Rule 9: Result Over Either

NeoHaskell uses `Result error value` instead of `Either`. Constructors are `Ok` and `Err`.

```haskell
-- CORRECT: Result type
validate :: Text -> Result ValidationError ValidatedInput

-- WRONG: Either type
validate :: Text -> Either ValidationError ValidatedInput
```

## Rule 10: Task Over IO

NeoHaskell uses `Task err val` instead of `IO`.

```haskell
-- CORRECT: Task with yield
readConfig :: Task ConfigError Config

-- WRONG: IO
readConfig :: IO Config
```

## Rule 11: Task.yield, Not pure/return

```haskell
-- CORRECT
Task.yield value

-- WRONG
pure value
return value
```

## Test Conventions

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
```

### Test Rules

1. **`spec :: Spec Unit`** return type
2. **`\_ ->` for unused test parameter** in `it` blocks
3. **`|>` pipes in assertions** — `value |> shouldBe expected`
4. **Tests are IMMUTABLE** — once written, never modify test expectations

## GHC Extensions in Use

| Extension | Impact |
|-----------|--------|
| `NoImplicitPrelude` | Must import from nhcore (`Basics`, `Core`) |
| `Strict` | All fields/bindings strict by default; `~` for laziness; `!` is redundant |
| `OverloadedStrings` | String literals as Text |
| `OverloadedRecordDot` | `entity.fieldName` instead of `fieldName entity` |
| `QuasiQuotes` | `[fmt\|...\|]` syntax |

## Performance Annotations

```haskell
-- Add INLINE to small, frequently-called functions in hot paths
yield :: value -> Task _ value
yield value = Task (Applicable.pure value)
{-# INLINE yield #-}
```

**Note on Strict Fields**: The `Strict` extension is enabled project-wide, so all record fields are strict by default. Do NOT add redundant `!` bang patterns. Use `{-# UNPACK #-}` on primitive fields for unboxing.

## Reference Files

| File | Demonstrates |
|------|-------------|
| `core/core/Text.hs` | Pipe chains, do-blocks, qualified imports |
| `core/core/Array.hs` | Collection operations, type parameters |
| `core/core/Result.hs` | Case expressions, error handling |
| `core/core/Task.hs` | Task.yield, Task.when/unless |
| `core/test/IntSpec.hs` | Test structure, pipe assertions |
