# NeoHaskell Core Library Reference

> **For AI Agents**: This is the authoritative API reference for `nhcore`. Do NOT use `base` or standard Haskell libraries. If a function you need doesn't exist here, create a GitHub issue at `github.com/neohaskell/neohaskell/issues`.

---

## Quick Reference: Haskell to NeoHaskell

| Standard Haskell | NeoHaskell | Notes |
|------------------|------------|-------|
| `IO a` | `Task err val` | Use `Task.yield` not `pure`/`return` |
| `Either a b` | `Result err val` | `Result.Ok`, `Result.Err` |
| `pure x` / `return x` | `Task.yield x` | Never use `pure`/`return` |
| `length xs` | `Array.length xs` | Qualified, type-specific |
| `sum xs` | `Array.sumIntegers xs` | Int-only, or use `reduce (+) 0` |
| `div a b` | `a // b` | Integer division |
| `<>` | `++` | Appendable typeclass |
| `"a" <> "b"` | `[fmt\|{a}{b}\|]` | Prefer string interpolation |
| `filter` | `Array.takeIf` | Keep elements matching predicate |
| `filterNot` | `Array.dropIf` | Remove elements matching predicate |
| `let x = y in z` | `do { let x = y; z }` | No `let..in` or `where` |
| `fmap` | `Task.map`, `Array.map` | Qualified per type |
| `Data.Text` | `Text` | Re-exported |
| `Data.Map` | `Map` | Re-exported from Data.Map.Strict |
| `Data.Vector` | `Array` | Vector-backed |

---

## Basics

**Module:** `Basics` (auto-imported)

### Operators
```haskell
(|>)  :: a -> (a -> b) -> b           -- Pipe: x |> f |> g
(<|)  :: (a -> b) -> a -> b           -- Reverse apply: f <| x
(.>)  :: (a -> b) -> (b -> c) -> (a -> c)  -- Forward compose
(<.)  :: (b -> c) -> (a -> b) -> (a -> c)  -- Backward compose
(++)  :: Appendable a => a -> a -> a  -- Append (replaces <>)
```

### Math
```haskell
(+), (-), (*)  :: Num n => n -> n -> n
(/)   :: Float -> Float -> Float      -- Float division
(//)  :: Int -> Int -> Int            -- Integer division
(^)   :: Float -> Float -> Float      -- Exponentiation
modBy :: Int -> Int -> Int            -- Modular arithmetic
remainderBy :: Int -> Int -> Int
isEven :: Integral a => a -> Bool
abs, negate :: Num n => n -> n
clamp :: Ord n => n -> n -> n -> n    -- clamp low high value
sqrt :: Float -> Float
round, floor, ceiling, truncate :: Float -> Int
```

### Comparison
```haskell
(==), (!=) :: Eq a => a -> a -> Bool
(<), (>), (<=), (>=) :: Ord a => a -> a -> Bool
max, min :: Ord a => a -> a -> a
compare :: Ord a => a -> a -> Ordering
```

### Booleans
```haskell
not :: Bool -> Bool
(&&), (||) :: Bool -> Bool -> Bool
xor :: Bool -> Bool -> Bool
```

### Functions
```haskell
identity :: a -> a                    -- Returns input unchanged
always :: a -> b -> a                 -- Ignores second arg
discard :: Mappable m => m a -> m ()  -- Void the result
panic :: HasCallStack => Text -> a    -- Crash with message
pass :: Applicable f => f ()          -- Do nothing, return unit
```

### String Interpolation
```haskell
[fmt|Hello {name}, you have {count} items|]  -- Quasi-quoter
```

### Types
```haskell
type Int = Prelude.Int
type Float = Prelude.Double
type Bool = Prelude.Bool
type Unit = ()
unit :: Unit                          -- The () value
type Never = Void                     -- Uninhabited type
never :: Never -> a                   -- Absurd
```

---

## Text

**Module:** `Text`

### Query
```haskell
isEmpty :: Text -> Bool
length :: Text -> Int
```

### Building
```haskell
append :: Text -> Text -> Text        -- Or use (++)
concat :: Array Text -> Text          -- Join array of texts
joinWith :: Text -> Array Text -> Text -- Join with separator
split :: Text -> Text -> Array Text   -- Split on separator
words :: Text -> Array Text           -- Split on whitespace
lines :: Text -> Array Text           -- Split on newlines
```

### Substrings
```haskell
slice :: Int -> Int -> Text -> Text   -- Supports negative indices
left :: Int -> Text -> Text           -- Take n from left
right :: Int -> Text -> Text          -- Take n from right
dropLeft :: Int -> Text -> Text
dropRight :: Int -> Text -> Text
```

### Search
```haskell
contains :: Text -> Text -> Bool      -- Is first in second?
startsWith :: Text -> Text -> Bool
endsWith :: Text -> Text -> Bool
indexes :: Text -> Text -> Array Int  -- All positions of needle
```

### Transform
```haskell
reverse :: Text -> Text
repeat :: Int -> Text -> Text         -- Repeat n times
replace :: Text -> Text -> Text -> Text -- replace old new text
toUpper, toLower :: Text -> Text
trim, trimLeft, trimRight :: Text -> Text
pad, padLeft, padRight :: Int -> Char -> Text -> Text
```

### Conversion
```haskell
toInt :: Text -> Maybe Int
fromInt :: Int -> Text
toFloat :: Text -> Maybe Float
fromFloat :: Float -> Text
toArray :: Text -> Array Char
fromArray :: Array Char -> Text
toBytes :: Text -> Bytes
fromBytes :: Bytes -> Text
```

### Case Conversion
```haskell
toPascalCase, toCamelCase, toTitleCase :: Text -> Text
toSnakeCase, toKebabCase :: Text -> Text
isPascalCase, isCamelCase, isTitleCase :: Text -> Bool
isSnakeCase, isKebabCase :: Text -> Bool
```

### Higher-Order
```haskell
map :: (Char -> Char) -> Text -> Text
filter :: (Char -> Bool) -> Text -> Text
foldl :: (Char -> b -> b) -> b -> Text -> b
foldr :: (Char -> b -> b) -> b -> Text -> b
any :: (Char -> Bool) -> Text -> Bool
all :: (Char -> Bool) -> Text -> Bool
```

---

## Array

**Module:** `Array`

### Creation
```haskell
empty :: Array a
wrap :: a -> Array a                  -- Single-element array
repeat :: Int -> a -> Array a         -- n copies of value
range :: Int -> Int -> Array Int      -- Inclusive range
initialize :: Int -> (Int -> a) -> Array a
fromLinkedList :: [a] -> Array a
```

### Query
```haskell
isEmpty :: Array a -> Bool
length :: Array a -> Int
get :: Int -> Array a -> Maybe a      -- Safe index access
first :: Array a -> Maybe a
last :: Array a -> Maybe a
contains :: Eq a => a -> Array a -> Bool
find :: (a -> Bool) -> Array a -> Maybe a
maximum :: Ord a => Array a -> Maybe a
minimum :: Ord a => Array a -> Maybe a
indices :: Array a -> Array Int
```

### Modify
```haskell
set :: Int -> a -> Array a -> Array a -- Update at index
push :: a -> Array a -> Array a       -- Add to end
append :: Array a -> Array a -> Array a
slice :: Int -> Int -> Array a -> Array a  -- Supports negative
take :: Int -> Array a -> Array a
drop :: Int -> Array a -> Array a
reverse :: Array a -> Array a
```

### Transform
```haskell
map :: (a -> b) -> Array a -> Array b
indexedMap :: (Int -> a -> b) -> Array a -> Array b
flatMap :: (a -> Array b) -> Array a -> Array b
flatten :: Array (Array a) -> Array a
```

### Filter
```haskell
takeIf :: (a -> Bool) -> Array a -> Array a    -- Keep matching
dropIf :: (a -> Bool) -> Array a -> Array a    -- Remove matching
takeWhile :: (a -> Bool) -> Array a -> Array a
dropWhile :: (a -> Bool) -> Array a -> Array a
partitionBy :: (a -> Bool) -> Array a -> (Array a, Array a)
getJusts :: Array (Maybe a) -> Array a         -- Extract Just values
```

### Reduce
```haskell
reduce :: (a -> b -> b) -> b -> Array a -> b   -- Fold right
foldl :: (a -> b -> b) -> b -> Array a -> b    -- Fold left
sumIntegers :: Array Int -> Int                -- Int-only sum
any :: (a -> Bool) -> Array a -> Bool
```

### Combine
```haskell
zip :: Array b -> Array a -> Array (a, b)
zipWith :: (a -> b -> c) -> Array b -> Array a -> Array c
chunksOf :: Int -> Array a -> Array (Array a)
```

### Convert
```haskell
toLinkedList :: Array a -> [a]
toIndexedLinkedList :: Array a -> [(Int, a)]
indexed :: Array a -> Array (Int, a)
splitFirst :: Array a -> Maybe (a, Array a)
```

---

## Maybe

**Module:** `Maybe`

```haskell
data Maybe a = Just a | Nothing

withDefault :: a -> Maybe a -> a      -- Extract with fallback
map :: (a -> b) -> Maybe a -> Maybe b
andThen :: (a -> Maybe b) -> Maybe a -> Maybe b  -- Monadic bind
flatten :: Maybe (Maybe a) -> Maybe a
getOrDie :: HasCallStack => Maybe a -> a  -- Unsafe extract (panics)
```

---

## Result

**Module:** `Result`

```haskell
data Result error value = Ok value | Err error

withDefault :: a -> Result e a -> a
map :: (a -> b) -> Result e a -> Result e b
mapError :: (e1 -> e2) -> Result e1 a -> Result e2 a
andThen :: (a -> Result e b) -> Result e a -> Result e b
toMaybe :: Result e a -> Maybe a
fromMaybe :: e -> Maybe a -> Result e a
isOk :: Result e a -> Bool
isErr :: Result e a -> Bool
fromEither :: Either e a -> Result e a
toEither :: Result e a -> Either e a
```

---

## Task

**Module:** `Task`

Task wraps IO with typed errors: `newtype Task err value`

### Creation
```haskell
yield :: value -> Task _ value        -- Success (NOT pure/return!)
throw :: err -> Task err _            -- Failure
fromIO :: IO a -> Task _ a            -- Lift IO
fromIOResult :: IO (Result e a) -> Task e a
fromFailableIO :: Exception e => IO a -> Task e a
```

### Transform
```haskell
map :: (a -> b) -> Task e a -> Task e b
mapError :: (e1 -> e2) -> Task e1 a -> Task e2 a
andThen :: (a -> Task e b) -> Task e a -> Task e b
recover :: (e -> Task e2 a) -> Task e a -> Task e2 a
asResult :: Task e a -> Task _ (Result e a)
```

### Run
```haskell
run :: (Result e a -> IO a) -> Task e a -> IO a
runOrPanic :: Show e => Task e a -> IO a    -- Crashes on error
runNoErrors :: Task Never a -> IO a          -- For infallible tasks
runResult :: Task e a -> IO (Result e a)
runMain :: Task Text Unit -> IO Unit         -- Entry point
```

### Control Flow
```haskell
when :: Bool -> Task e Unit -> Task e Unit
unless :: Bool -> Task e Unit -> Task e Unit
forever :: Task e Unit -> Task e Unit       -- Loop until error
while :: Task e Bool -> Task e Unit -> Task e Unit
finally :: Task e2 Unit -> Task e a -> Task e a  -- Cleanup
```

### Collections
```haskell
forEach :: (a -> Task e Unit) -> Array a -> Task e Unit
mapArray :: (a -> Task e b) -> Array a -> Task e (Array b)
```

### Utilities
```haskell
ignoreError :: Task e Unit -> Task _ Unit
errorAsResult :: Task e Unit -> Task Never (Maybe e)
```

---

## Map

**Module:** `Map`

```haskell
type Map key value = Data.Map.Strict.Map key value

empty :: Map k v
build :: Accumulator (Map k v) -> Map k v     -- Builder pattern
fromArray :: Ord k => Array (k, v) -> Map k v

get :: Ord k => k -> Map k v -> Maybe v
getOrElse :: Ord k => k -> v -> Map k v -> v
set :: Ord k => k -> v -> Map k v -> Map k v
remove :: Ord k => k -> Map k v -> Map k v
contains :: Ord k => k -> Map k v -> Bool

length :: Map k v -> Int
keys :: Map k v -> Array k
values :: Map k v -> Array v
entries :: Map k v -> Array (k, v)

merge :: Ord k => Map k v -> Map k v -> Map k v
mapValues :: (a -> b) -> Map k a -> Map k b
reduce :: acc -> (k -> v -> acc -> acc) -> Map k v -> acc

-- Builder operator
(-->) :: Ord k => k -> v -> Accumulator (Map k v)
-- Usage: Map.build do { "a" --> 1; "b" --> 2 }
```

---

## Concurrency

### AsyncTask

**Module:** `AsyncTask`

```haskell
run :: Show e => Task e a -> Task e (AsyncTask e a)  -- Start async
waitFor :: Show e => AsyncTask e a -> Task e a       -- Await result
cancel :: AsyncTask e a -> Task Text Unit            -- Cancel task
sleep :: Int -> Task _ Unit                          -- Milliseconds
process :: Show e => Task e a -> (AsyncTask e a -> Task e b) -> Task e b
runConcurrently :: Show e => (Task e a, Task e b) -> Task e (a, b)
runAllIgnoringErrors :: Show e => Array (Task e a) -> Task _ Unit
```

### Channel

**Module:** `Channel`

```haskell
new :: Task _ (Channel a)                 -- Unbounded channel
newBounded :: Int -> Task Text (Channel a) -- Bounded with capacity

read :: Channel a -> Task _ a             -- Blocks until available
tryRead :: Channel a -> Task _ (Maybe a)  -- Non-blocking
write :: a -> Channel a -> Task _ Unit    -- May block if bounded
tryWriteWithTimeout :: Int -> a -> Channel a -> Task Text (Result Text Unit)

isBounded :: Channel a -> Bool
```

### ConcurrentVar

**Module:** `ConcurrentVar`

```haskell
containing :: a -> Task _ (ConcurrentVar a)  -- Create with value
read :: ConcurrentVar a -> Task _ a
write :: a -> ConcurrentVar a -> Task _ Unit
modify :: (a -> a) -> ConcurrentVar a -> Task _ Unit
tryTake :: ConcurrentVar a -> Task _ (Maybe a)
```

### Lock

**Module:** `Lock`

```haskell
new :: Task _ Lock
acquire :: Lock -> Task _ Unit
release :: Lock -> Task _ Unit
withLock :: Lock -> Task e a -> Task e a  -- RAII pattern
```

---

## System

### File

**Module:** `File`

```haskell
data Error = NotFound | NotWritable | NotReadable

readText :: Path -> Task Error Text
writeText :: Path -> Text -> Task Error Unit
readBytes :: Path -> Task Error Bytes
writeBytes :: Path -> Bytes -> Task Error Unit
exists :: Path -> Task Error Bool
rename :: Path -> Path -> Task Error Unit
deleteIfExists :: Path -> Task Error Unit
```

### Directory

**Module:** `Directory`

```haskell
create :: Path -> Task Error Unit
list :: Path -> Task Error (Array Path)
exists :: Path -> Task Error Bool
```

### Path

**Module:** `Path`

```haskell
type Path = Text  -- Simplified, see actual module for full API
fromText :: Text -> Path
toText :: Path -> Text
(</>) :: Path -> Path -> Path  -- Join paths
```

### Environment

**Module:** `Environment`

```haskell
get :: Text -> Task _ (Maybe Text)     -- Get env var
getOrFail :: Text -> Task Text Text    -- Get or error
set :: Text -> Text -> Task _ Unit
```

---

## Service (Event Sourcing)

### QueryAction

**Module:** `Service.Query.Core`

```haskell
data QueryAction query
  = Update query  -- Store/update the query instance
  | Delete        -- Remove from store
  | NoOp          -- Take no action (NOT NoChange!)
```

### Decider

**Module:** `Decider`

```haskell
acceptNew :: [event] -> Decision entity event      -- Stream must not exist
acceptExisting :: [event] -> Decision entity event -- Stream must exist
acceptAfter :: StreamPosition -> [event] -> Decision entity event
reject :: Text -> Decision entity event

getCurrentTime :: Decision entity DateTime
generateUuid :: Decision entity Uuid
```

### Entity

```haskell
class Entity entity where
  type EventOf entity
  initialStateImpl :: entity
  updateImpl :: EventOf entity -> entity -> entity
```

### Command

```haskell
class Command cmd where
  type EntityOf cmd
  getEntityIdImpl :: cmd -> Uuid
  decideImpl :: cmd -> EntityOf cmd -> Decision (EntityOf cmd) (EventOf (EntityOf cmd))
```

### Query

```haskell
class Query query where
  canAccessImpl :: Maybe UserClaims -> Maybe QueryAuthError
  canViewImpl :: Maybe UserClaims -> query -> Maybe QueryAuthError

class QueryOf entity query where
  queryId :: entity -> Uuid
  combine :: entity -> Maybe query -> QueryAction query
```

---

## Traits (Typeclasses)

| Trait | Haskell Equivalent | Key Method |
|-------|-------------------|------------|
| `Mappable` | `Functor` | `map` |
| `Thenable` | `Monad` | `andThen`, `yield` |
| `Applicable` | `Applicative` | `apply` |
| `Appendable` | `Semigroup` | `(++)` |
| `Combinable` | `Monoid` | `empty` |
| `Default` | custom | `def` |
| `ToText` | `Show`-like | `toText`, `toPrettyText` |
| `Collection` | custom (31 methods) | See Array section |

---

## What's Missing?

If you need functionality that doesn't exist in nhcore:

1. **DO NOT** import from `base`, `containers`, `text`, `bytestring`, or other Haskell packages directly
2. **DO NOT** create workarounds using raw GHC primitives
3. **DO** create a GitHub issue: `github.com/neohaskell/neohaskell/issues`

### Issue Template

```markdown
**Title:** [nhcore] Add `functionName` to `ModuleName`

**Description:**
I need a function to [description of what it should do].

**Proposed API:**
```haskell
functionName :: InputType -> OutputType
```

**Use case:**
[Why this is needed, what problem it solves]

**Haskell equivalent:**
[If there's a standard Haskell function that does this]
```

### Common Missing Features to Request

- Generic numeric `sum` (currently only `sumIntegers` for `Array Int`)
- `all` for Array (exists for Text, not Array)
- `sortBy` for Array
- `groupBy` for Array
- Generic `foldMap`

---

## Import Style

Always use the NeoHaskell import pattern:

```haskell
-- Import type unqualified, module qualified
import Array (Array)
import Array qualified

import Text (Text)
import Text qualified

import Task (Task)
import Task qualified

-- Usage
processWords :: Array Text -> Task _ Int
processWords words =
  words
    |> Array.map Text.length
    |> Array.sumIntegers
    |> Task.yield
```

For base/GHC imports (when absolutely necessary):

```haskell
import Data.List qualified as GhcList  -- Prefix with Ghc
```
