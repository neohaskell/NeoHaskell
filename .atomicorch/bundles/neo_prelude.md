---
id: neo_prelude
target_max_tokens: 4000
---

# NeoHaskell Language Introduction

NeoHaskell is a curated dialect of GHC Haskell with a fixed set of conventions that make code readable, pipeable, and beginner-friendly. Every file in the project must follow these 12 rules.

## The 12 Rules

### Rule 1: Pipe over nesting

Use `|>` to chain operations left-to-right. Never use `$` for function application or nest calls right-to-left.

```haskell
-- CORRECT
result = input |> foo |> bar |> baz

-- WRONG
result = baz $ bar $ foo input
result = baz (bar (foo input))
```

### Rule 2: Do + let for bindings

Use `do` blocks with `let` for intermediate bindings. Do not use `let..in` expressions or `where` clauses.

```haskell
-- CORRECT
process :: Task Result
process = do
  let value = computeSomething input
  let transformed = value |> transform
  Task.yield transformed

-- WRONG
process = let value = computeSomething input
              transformed = transform value
          in Task.yield transformed

process = Task.yield transformed
  where
    value = computeSomething input
    transformed = transform value
```

### Rule 3: Case-only for pattern match

Use `case` expressions for all pattern matching. Do not use function-level pattern matching with multiple clauses.

```haskell
-- CORRECT
describeList :: Array Int -> Text
describeList xs = case xs of
  [] -> "empty"
  [_] -> "singleton"
  _ -> "many"

-- WRONG
describeList [] = "empty"
describeList [_] = "singleton"
describeList _ = "many"
```

### Rule 4: If-then-else for Booleans

Use `if`-`then`-`else` for boolean branching. `case` is reserved for data constructors.

```haskell
-- CORRECT
label = if count > 1 then "plural" else "singular"

-- WRONG
label = case count > 1 of
  True -> "plural"
  False -> "singular"
```

### Rule 5: Descriptive type parameters

Use full English words for type variables. Never use single-letter names.

```haskell
-- CORRECT
map :: forall element result. (element -> result) -> Array element -> Array result

-- WRONG
map :: forall a b. (a -> b) -> [a] -> [b]
```

### Rule 6: Qualified imports

Always import modules qualified. Do not use unqualified imports for anything beyond the NeoHaskell prelude.

```haskell
-- CORRECT
import Array qualified
import Text qualified
import Json qualified

value = Array.empty |> Array.push 1

-- WRONG
import Data.List (map, filter)
import Data.Text (pack, unpack)
```

### Rule 7: nhcore before base

Always prefer nhcore modules over base equivalents. Import nhcore modules first in the import list.

```haskell
-- CORRECT
import Core.Array qualified as Array
import Core.Text qualified as Text

-- WRONG
import Data.List qualified as List
import Data.Text qualified as Text
```

### Rule 8: Interpolation with [fmt|...|]

Use the `[fmt|...|]` quasi-quoter for all string interpolation. Never use string concatenation with `<>` for message construction.

```haskell
-- CORRECT
message = [fmt|User {userId} not found in {tableName}|]

-- WRONG
message = "User " <> show userId <> " not found in " <> tableName
```

### Rule 9: Result, not Either

Use `Result error value` instead of `Either left right` for error handling.

```haskell
-- CORRECT
parseAge :: Text -> Result Text Int

-- WRONG
parseAge :: Text -> Either Text Int
```

### Rule 10: Task, not IO

Use `Task error value` instead of `IO a` for effectful operations. Never write `IO` in new code.

```haskell
-- CORRECT
readConfig :: Task ConfigError Config

-- WRONG
readConfig :: IO Config
```

### Rule 11: Task.yield, not pure/return

Use `Task.yield` to lift pure values into `Task`. Never use `pure` or `return`.

```haskell
-- CORRECT
Task.yield computedValue

-- WRONG
pure computedValue
return computedValue
```

### Rule 12: INLINE in hot paths

Add `{-# INLINE functionName #-}` pragma immediately after any function that is in a hot path (called >10k times/sec or in tight loops).

```haskell
-- CORRECT
applyMiddleware :: Request -> Response
applyMiddleware req = req |> authenticate |> authorize |> rateLimit
{-# INLINE applyMiddleware #-}
```

---

## Type Map

When migrating from standard Haskell or reading base documentation, use this translation table:

| Base / Standard | NeoHaskell Equivalent |
|-----------------|----------------------|
| `IO a` | `Task error value` |
| `Either left right` | `Result error value` |
| `Data.Text.Text` | `Text` (from nhcore) |
| `[a]` / `Data.List` | `Array element` |
| `Data.Map.Map` | `Map key value` |
| `Data.Set.Set` | `Set element` |
| `Maybe a` | `Option value` |
| `pure` / `return` | `Task.yield` |
| `>>=` | `|> Task.andThen` |
| `>>` | `|> Task.andThen (\_ -> ...)` |
| `Data.ByteString` | `Bytes` |
| `Data.Aeson.Value` | `Json.Value` |
| `fromRight` / `fromLeft` | `Result.withDefault` |

---

## Anti-Patterns

These patterns will be rejected in code review:

```haskell
-- ANTI: dollar sign application
result = process $ transform $ input

-- ANTI: single-letter type vars
data Wrapper a = Wrapper a

-- ANTI: where clause
compute x = helper x
  where helper n = n * 2

-- ANTI: let..in expression
compute x = let y = x * 2 in y + 1

-- ANTI: function-level pattern matching
handle Nothing = defaultValue
handle (Just x) = process x

-- ANTI: string concat for messages
err = "Expected " <> show n <> " but got " <> show m

-- ANTI: IO usage
fetchData :: IO ByteString

-- ANTI: Either usage
validate :: Text -> Either ValidationError User

-- ANTI: pure/return
wrap x = pure x
```
