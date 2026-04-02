# ADR-0046: Function Syntax — `fun` Keyword Parsing and Transpilation

## Status

Accepted

## Context

NeoHaskell's syntax specification (§1, Functions) defines the complete grammar for top-level function and
constant definitions. This is the second transpiler syntax feature after comments (ADR-0043) and the first
to handle blocks: all brace blocks transpile uniformly to Haskell `do` notation (GHC supports `do` in pure
context). Effectful blocks with `let!` bindings produce monadic `<-` in the output. The parser challenge
(`where` clause disambiguation with three forms) and codegen decisions require explicit design.

### Current State

`nhcore` has the `Syntax.Comment` module (ADR-0043) but no facility for parsing NeoHaskell function
definitions. The `Parser` module (ADR-0042) provides the necessary combinators (backtracking choice,
position tracking, delimited collections) but contains no language-specific knowledge.

No module in nhcore currently:
- Parses `fun`/`let` top-level declarations or captures their structure as an AST
- Represents function definitions, parameters, type constraints, or body forms
- Distinguishes `let` (pure) from `let!` (effectful) bindings in block bodies
- Captures bare expressions (e.g. `putStrLn "lol"`) as block statements
- Captures the `return` visual marker in the AST (for future rewrite rules like early returns)
- Provides a `toHaskell` transpilation function for function declarations

### Use Cases

- **Transpiler core pipeline (#465)**: convert `fun add(x: Int, y: Int) : Int = x + y` to a Haskell
  type signature + function definition
- **Block transpilation**: `{ let dx = x2 - x1\nsqrt(dx) }` → `do { let dx = x2 - x1; sqrt dx }`
- **Effectful block transpilation**: `{ let! x = getX()\nTask.yield(x) }` → `do { x <- getX; Task.yield x }`
- **Constraint transpilation**: `where t: Show` → `(Show t) =>` prefix on the type signature
- **IDE tooling**: LSP hover for parameter types, constraint lists, and return types
- **Formatter**: re-emit canonical formatted function definitions with correct indentation

### Design Goals

1. **One file per syntactic construct**: follow the `Syntax.Comment` pattern — AST types, parsers, and
   `toHaskell` in a single module (`Syntax.Function`)
2. **Opaque expression text for phase 1**: expression parsing is a separate future concern; capture body
   expressions as raw `Text` to unblock the transpiler now without blocking on `Syntax.Expression`
3. **Exact `where` disambiguation**: three constraint forms are distinguished by their first two tokens
   with no global backtracking — safe, cheap, and predictable
4. **Uniform `do` block transpilation**: all brace blocks (`{ ... }`) transpile to Haskell `do` notation.
   GHC supports `do` in pure context, so no need to distinguish pure blocks (`let..in`) from effectful
   blocks — one codegen path handles both
5. **`return` captured but not transpiled**: the `return` keyword is parsed and stored as a
   `ReturnStatement` node in the `BlockStatement` sum type at the statement level. It is not emitted
   during transpilation (the expression is output as a bare expression for now), but its presence as a
   distinct AST node enables future rewrite rules (e.g. early returns via `if`, loop breaks)
6. **`toHaskell` produces `Blueprint`**: matches the `Syntax.Comment.toHaskell` signature; compose
   with `Layout.render` at the edge

### GitHub Issue

- [#465: Function Syntax (fun keyword)](https://github.com/neohaskell/NeoHaskell/issues/465)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — parent
  tracking issue; this ADR directly unblocks it

## Decision

### 1. Module Naming: `Syntax.Function`

The module lives in `core/syntax/Syntax/Function.hs`, following the `Syntax.*` namespace established in
ADR-0043. No new source directory is required — `syntax` is already registered in `nhcore.cabal`.

| Candidate | Verdict | Rationale |
| --------- | ------- | --------- |
| `Parser.Function` | Rejected | `Parser.*` is reserved for general combinators (ADR-0042). Language-specific AST + transpilation logic must not live under the general parsing library. |
| `Transpiler.Function` | Rejected | Implies a separate transpiler package. Sharing in nhcore lets the LSP proxy, formatter, and linter reuse the same types without a transpiler dependency. |
| `Language.Function` | Rejected | `Language.*` risks confusion with GHC's `Language.Haskell.*` hierarchy and implies a multi-language abstraction that is not intended here. |
| `Syntax.Function` | **Chosen** | Exact parallel to `Syntax.Comment`. The family scales naturally: `Syntax.Comment`, `Syntax.Function`, `Syntax.Enum`, `Syntax.Pattern`. |

**Directory structure:**

```text
core/
└── syntax/
    └── Syntax/
        ├── Comment.hs          -- existing (ADR-0043)
        └── Function.hs         -- new

core/test-core/
└── Syntax/
    ├── CommentSpec.hs          -- existing
    └── FunctionSpec.hs         -- new (auto-discovered by hspec-discover)
```

**Cabal additions** (`exposed-modules`, alphabetically after `Syntax.Comment`):

```text
Syntax.Function
```

**Cabal additions** (`nhcore-test-core` `other-modules`):

```text
Syntax.FunctionSpec
```

### 2. Type Definitions

Seven types represent the complete AST for NeoHaskell function and constant declarations. All types follow
NeoHaskell conventions: descriptive field names, strict fields (project-wide `Strict` extension), `Text`
not `String`, `ParsePosition` from `Parser` (ADR-0042).

#### `TopLevelDecl` — Entry-Point Union

```haskell
-- | A top-level declaration parsed from NeoHaskell source.
--
-- Two variants mirror the two definition keywords:
--
-- * 'FunctionDecl' wraps a 'FunctionDef' (parsed from @fun ...@)
-- * 'ConstantDecl' wraps a 'ConstantDef' (parsed from @let name : Type = expr@)
--
-- Obtain via 'topLevelDecl'. Transpile via 'toHaskell'.
data TopLevelDecl
  = FunctionDecl FunctionDef
  | ConstantDecl ConstantDef
  deriving (Eq, Prelude.Show, Generic)
```

#### `FunctionDef`

```haskell
-- | A parsed @fun@ function definition.
--
-- @
-- fun greet(name: Text) : Text = "Hello, " ++ name
--
-- fun distance(x1: Float, y1: Float, x2: Float, y2: Float) : Float {
--     let dx = x2 - x1
--     let dy = y2 - y1
--     sqrt(dx * dx + dy * dy)
-- }
--
-- fun show<t>(x: t) : Text where t: Show { toString(x) }
-- @
data FunctionDef = FunctionDef
  { position    :: ParsePosition
    -- ^ Start position of the @fun@ keyword (1-based line and column).
  , name        :: Text
    -- ^ The function name identifier, e.g. @"add"@, @"greet"@.
  , typeParams  :: Array Text
    -- ^ Generic type parameter names from @\<a, b\>@, e.g. @["a", "b"]@.
    -- Empty array for non-generic functions.
  , parameters  :: Array Parameter
    -- ^ Positional parameters in declaration order.
    -- Empty array for zero-argument effectful functions (declared with @()@).
  , returnType  :: Text
    -- ^ The return type as captured source text, e.g. @"Int"@, @"Task\<Text\>"@.
    -- Opaque for phase 1 — a future 'Syntax.Expression' parser will replace this field.
  , constraints :: Array TypeConstraint
    -- ^ Constraints from the @where@ clause, in source order.
    -- Empty array if no @where@ clause is present.
  , body        :: FunctionBody
    -- ^ The function body — pure expression or brace block.
  }
  deriving (Eq, Prelude.Show, Generic)
```

#### `ConstantDef`

```haskell
-- | A parsed top-level constant definition.
--
-- @
-- let pi : Float = 3.14
-- let maxRetries : Int = 3
-- let greeting : Text = "Hello, world!"
-- @
data ConstantDef = ConstantDef
  { position  :: ParsePosition
    -- ^ Start position of the @let@ keyword.
  , name      :: Text
    -- ^ The constant name identifier.
  , constType :: Text
    -- ^ The declared type as captured source text, e.g. @"Float"@, @"Int"@.
  , value     :: Text
    -- ^ The value expression as captured source text.
    -- Opaque for phase 1 — a future 'Syntax.Expression' parser will replace this field.
  }
  deriving (Eq, Prelude.Show, Generic)
```

#### `Parameter`

```haskell
-- | A function parameter with its type annotation.
--
-- @
-- (x: Int)       → Parameter { paramName = "x", paramType = "Int" }
-- (name: Text)   → Parameter { paramName = "name", paramType = "Text" }
-- (f: (a) -> b)  → Parameter { paramName = "f", paramType = "(a) -> b" }
-- @
data Parameter = Parameter
  { paramName :: Text
    -- ^ The parameter identifier.
  , paramType :: Text
    -- ^ The declared type as captured source text. Opaque for phase 1.
  }
  deriving (Eq, Prelude.Show, Generic)
```

#### `TypeConstraint`

```haskell
-- | A single constraint from a @where@ clause.
--
-- Three syntactic forms, disambiguated by the first two tokens:
--
-- * 'TraitConstraint'      — @LOWER_IDENT :@   — single-parameter typeclass sugar
-- * 'FieldConstraint'      — @LOWER_IDENT has@ — structural record field requirement
-- * 'MultiParamConstraint' — @UPPER_IDENT \<@   — multi-parameter typeclass application
--
-- @
-- where t: Show            → TraitConstraint { variable = "t", trait = "Show" }
-- where a has name: Text   → FieldConstraint { variable = "a", fieldName = "name", fieldType = "Text" }
-- where SomeClass\<Int, t\> → MultiParamConstraint { traitName = "SomeClass", typeArgs = ["Int", "t"] }
-- @
data TypeConstraint
  = TraitConstraint
      { variable :: Text
        -- ^ The type variable being constrained, e.g. @"t"@, @"a"@.
      , trait    :: Text
        -- ^ The typeclass name, e.g. @"Show"@, @"Eq"@, @"Ord"@.
      }
  | FieldConstraint
      { variable  :: Text
        -- ^ The type variable that must have the field, e.g. @"a"@.
      , fieldName :: Text
        -- ^ The required field name, e.g. @"name"@, @"age"@.
      , fieldType :: Text
        -- ^ The required field type as source text, e.g. @"Text"@, @"Positive\<Int\>"@.
      }
  | MultiParamConstraint
      { traitName :: Text
        -- ^ The multi-parameter typeclass name, e.g. @"SomeClass"@, @"Convertible"@.
      , typeArgs  :: Array Text
        -- ^ The type arguments in application order, e.g. @["Int", "Text", "a"]@.
      }
  deriving (Eq, Prelude.Show, Generic)
```

#### `FunctionBody`

```haskell
-- | The body of a NeoHaskell function definition.
--
-- Two forms corresponding to the two syntax variants in §1 of the spec:
--
-- * 'Expression' — @= expr@ (single expression after @=@, no bindings)
-- * 'Block'      — @{ statements }@ (brace block with bindings, bare expressions, and/or returns)
--
-- All blocks transpile uniformly to Haskell @do@ notation. GHC supports @do@ in
-- pure context, so no pure/effectful distinction is needed at the AST level.
--
-- The last statement in a 'Block' is the result. It may be a 'BareExpression'
-- or a 'ReturnStatement' — both emit the expression as the final @do@ line.
data FunctionBody
  = Expression
      { exprText :: Text
        -- ^ The expression as captured source text. Opaque for phase 1.
      }
  | Block
      { statements :: Array BlockStatement
        -- ^ All statements in source order (bindings, bare expressions, returns).
        -- The last element is the block's result expression.
      }
  deriving (Eq, Prelude.Show, Generic)
```

#### `BlockStatement`

```haskell
-- | A single statement in a brace block body.
--
-- * 'PureBinding'      — @let x = expr@       — pure let binding
-- * 'EffectfulBinding' — @let! x = expr@      — effectful (monadic) binding
-- * 'BareExpression'   — @expr@               — standalone expression (e.g. @putStrLn "lol"@)
-- * 'ReturnStatement'  — @return expr@         — visual return marker with its expression
--
-- 'ReturnStatement' is captured at the statement level so that future rewrite rules
-- (e.g. early returns inside @if@ or loops) know exactly which statement carries the
-- @return@ keyword. In phase 1, @return@ is not emitted during transpilation —
-- only the inner expression is emitted, identical to 'BareExpression'.
data BlockStatement
  = PureBinding
      { bindingName  :: Text
        -- ^ The bound identifier, e.g. @"x"@, @"dx"@, @"_"@.
      , bindingValue :: Text
        -- ^ The right-hand side expression as captured source text.
      }
  | EffectfulBinding
      { bindingName  :: Text
        -- ^ The bound identifier, e.g. @"content"@, @"_"@.
      , bindingValue :: Text
        -- ^ The right-hand side effectful expression as captured source text.
      }
  | BareExpression
      { exprText :: Text
        -- ^ The standalone expression as captured source text.
        -- Equivalent to GHC Haskell bare expressions in @do@ blocks.
      }
  | ReturnStatement
      { exprText :: Text
        -- ^ The expression following @return@.
        -- Currently transpiles identically to 'BareExpression' (return is stripped).
        -- Captured separately to enable future early-return rewrite rules.
      }
  deriving (Eq, Prelude.Show, Generic)
```

### 3. Expression Handling Strategy: Opaque `Text`

Expression parsing (arithmetic, function calls, string literals, pipe chains, lambdas) is a substantial
feature of its own and is explicitly deferred to a future `Syntax.Expression` ADR. For phase 1, all
expression positions are captured as raw `Text` using a **balanced-delimiter scan**:

| Position | Capture terminators |
| -------- | ------------------- |
| After `=` (pure expression body) | Newline not inside balanced `{}`, `()`, `[]` |
| After `let name =` in a block | Newline at block depth zero |
| After `let! name =` in a block | Newline at block depth zero |
| Result expression in a block | `}` at block depth one (the function body close) |
| Type annotation (parameter, return type, field type) | `:` (consumed), then `,`, `)`, `{`, `=`, or `where` |

The scanner treats quoted strings (`"..."`) as atomic units — `{` and `}` inside string literals do not
affect brace depth tracking.

This approach is deliberately minimal and explicitly documented as phase-1 scaffolding. All `Text`-typed
expression fields in the AST are candidates for replacement by a typed `Syntax.Expression` node in a
future ADR.

### 4. `where` Clause Disambiguation

The `where` clause accepts a comma-separated list of constraints. Three constraint forms are
syntactically distinct by their first two tokens:

| First token | Second token | Constraint form |
| ----------- | ------------ | --------------- |
| Lowercase identifier | `has` keyword | `FieldConstraint` — `a has name: Text` |
| Lowercase identifier | `:` | `TraitConstraint` — `t: Show` |
| Uppercase identifier | `<` | `MultiParamConstraint` — `SomeClass<Int, t>` |

**Implementation strategy**: `Parser.choice` with three `Parser.backtrack`-wrapped alternatives.
`FieldConstraint` must be tried before `TraitConstraint` because both begin with a lowercase identifier
— attempting `TraitConstraint` first would incorrectly consume `a` then fail on `has`:

```haskell
-- | Parse a single type constraint from a @where@ clause.
--
-- Tries three forms using backtracking:
--
-- 1. 'FieldConstraint'      — @a has field: Type@      (LOWER_IDENT has)
-- 2. 'TraitConstraint'      — @t: Trait@               (LOWER_IDENT :)
-- 3. 'MultiParamConstraint' — @SomeClass<Int, t>@      (UPPER_IDENT <)
--
-- Ordering is mandatory: 'FieldConstraint' and 'TraitConstraint' both start
-- with a lowercase identifier; @has@ must be checked before @:@.
typeConstraint :: Parser TypeConstraint
typeConstraint =
  Parser.choice
    [ fieldConstraint
    , traitConstraint
    , multiParamConstraint
    ]
{-# INLINE typeConstraint #-}
```

**Full `where` clause parser** (comma-separated, terminated by `{` or `=`):

```haskell
-- | Parse the optional @where@ clause of a function definition.
--
-- Returns an empty array if no @where@ keyword is found.
-- Constraints are comma-separated; a trailing comma before @{@ or @=@ is allowed.
whereClause :: Parser (Array TypeConstraint)
whereClause =
  Parser.choice
    [ do
        _ <- Parser.text "where"
        _ <- Parser.spaces
        typeConstraint
          |> Parser.separatedBy (Parser.char ',' |> Parser.keepRight Parser.spaces)
    , Parser.yield Array.empty
    ]
{-# INLINE whereClause #-}
```

### 5. Uniform Block Transpilation (`do` for all blocks)

All brace blocks transpile uniformly to Haskell `do` notation. GHC supports `do` in pure context
(e.g. `do { let x = 1; x + 2 }` is valid Haskell), so there is **no need to distinguish pure blocks
from effectful blocks** at the AST or codegen level. This simplifies the design:

- One `Block` constructor instead of two (`PureBlock`/`EffectfulBlock`)
- One codegen path instead of two (`do` vs `let..in`)
- Bare expressions (e.g. `putStrLn "lol"`) are naturally supported in `do` blocks

**Statement transpilation within `do`:**

| NeoHaskell statement | Haskell `do` output |
| --- | --- |
| `let x = expr` | `let x = expr` |
| `let! x = expr` | `x <- expr` |
| bare expression (e.g. `putStrLn "lol"`) | `putStrLn "lol"` |
| `return expr` | `expr` (return stripped, only expression emitted) |

Every statement is emitted **as-is** — no `pure` wrapping. The user explicitly writes
`Task.yield(x)` or `Result.ok(x)` when wrapping is needed. This matches the spec (§1): "the user
must explicitly wrap pure values into the effect type."

### 6. `return` Visual Marker Handling

The `return` keyword in NeoHaskell block bodies is a visual marker that compiles to nothing (§1,
"return is a visual marker"). It is **parsed and captured as a `ReturnStatement`** at the statement
level, but **not emitted** during transpilation — only the inner expression is emitted.

**Why statement-level, not block-level?** Future rewrite rules (e.g. early returns inside `if` or
loops) need to know *which specific statement* carries the `return` keyword. A block-level boolean
would lose positional information. A statement-level constructor preserves it.

**Parser rule**: when parsing a statement in a block, if the parser encounters the `return` keyword
(followed by whitespace), it wraps the following expression in a `ReturnStatement`. Otherwise the
expression becomes a `BareExpression`. Both transpile identically in phase 1.

| Option | Verdict | Rationale |
| ------ | ------- | --------- |
| Strip at parse time (discard entirely) | Rejected | Prevents future rewrite rules that need positional `return` information. |
| Capture as block-level boolean | Rejected | Loses position — can't distinguish `return` in middle of block (future early returns) from final-line `return`. |
| Capture as `ReturnStatement` in `BlockStatement` (this ADR) | **Chosen** | Statement-level granularity. Enables early-return rewrite rules. Transpiles identically to `BareExpression` in phase 1. |

### 7. Public API

```haskell
module Syntax.Function
  ( -- * Types
    TopLevelDecl (..)
  , FunctionDef (..)
  , ConstantDef (..)
  , Parameter (..)
  , TypeConstraint (..)
  , FunctionBody (..)
  , BlockStatement (..)
    -- * Parsers
  , topLevelDecl
  , functionDef
  , constantDef
    -- * Transpilation
  , toHaskell
  ) where
```

#### Parser Functions

| Function | Signature | Parses |
| -------- | --------- | ------ |
| `topLevelDecl` | `Parser TopLevelDecl` | `fun ...` or top-level `let ...` |
| `functionDef` | `Parser FunctionDef` | `fun name<params>(params) : Type where constraints body` |
| `constantDef` | `Parser ConstantDef` | `let name : Type = expr` |

**`topLevelDecl` ordering**: `functionDef` is tried before `constantDef`. Both use distinct keywords
(`fun` vs `let`) so no ordering conflict exists; `Parser.choice` handles the dispatch:

```haskell
-- | Parse a top-level NeoHaskell declaration.
--
-- Tries 'functionDef' before 'constantDef'. The keywords @fun@ and @let@ are
-- unambiguous first tokens — no ordering constraint is needed, but @fun@ is
-- more common and tried first for performance.
topLevelDecl :: Parser TopLevelDecl
topLevelDecl =
  Parser.choice
    [ functionDef |> Parser.map FunctionDecl
    , constantDef |> Parser.map ConstantDecl
    ]
{-# INLINE topLevelDecl #-}
```

#### `toHaskell` Transpilation Function

```haskell
-- | Convert a NeoHaskell top-level declaration to its Haskell equivalent.
--
-- Produces a 'Blueprint' containing:
--
-- * For 'FunctionDecl': the Haskell type signature on the first line,
--   followed by the function definition on the second line.
-- * For 'ConstantDecl': the Haskell type annotation on the first line,
--   followed by the constant definition on the second line.
--
-- @
-- toHaskell (FunctionDecl ...) |> Layout.render
--   == "add :: Int -> Int -> Int\nadd x y = x + y"
--
-- toHaskell (ConstantDecl ...) |> Layout.render
--   == "pi :: Float\npi = 3.14"
-- @
toHaskell :: TopLevelDecl -> Blueprint annotation
toHaskell decl =
  case decl of
    FunctionDecl def -> functionDefToHaskell def
    ConstantDecl def -> constantDefToHaskell def
{-# INLINE toHaskell #-}
```

### 8. Transpilation Rules

#### Type Signature Generation

The Haskell type signature is assembled from four components:

1. **Constraint prefix** (if any constraints): `(Constraint1, Constraint2) =>`
2. **Parameter types**: `Type1 -> Type2 ->`
3. **Return type**: `ReturnType`
4. **Zero-argument special case**: `()` parameter list is dropped; no `->` is emitted

**Constraint mapping:**

| NeoHaskell `where` constraint | Haskell type sig fragment |
| ----------------------------- | ------------------------- |
| `t: Show` | `Show t` |
| `t: Show, t: Ord` | `Show t, Ord t` |
| `a has name: Text` | `HasField "name" a Text` |
| `SomeClass<Int, Text, t>` | `SomeClass Int Text t` |
| `Convertible<a, b>` | `Convertible a b` |

**Generic type application conversion**: angle-bracket syntax `Task<Int>` must be converted to
space-separated Haskell type application `Task Int`. Nested generics (`Task<Result<Text, Int>>`) require
recursive conversion via a balanced-bracket pass, not a flat text replacement. This conversion is
implemented as a `convertTypeText :: Text -> Text` helper inside the module.

#### Body Transpilation

All blocks transpile to `do` notation. Newlines in source become newlines in output (no semicolons).

| NeoHaskell body form | Haskell definition output |
| --- | --- |
| `= x + y` (expression) | `f x y = x + y` |
| `{ let dx = x2 - x1\nlet dy = y2 - y1\nsqrt(dx * dx + dy * dy) }` (pure block) | `f x1 y1 x2 y2 = do\n  let dx = x2 - x1\n  let dy = y2 - y1\n  sqrt (dx * dx + dy * dy)` |
| `{ let! content = readFile(path)\nTask.yield(content) }` (effectful block) | `f path = do\n  content <- readFile path\n  Task.yield content` |
| `{ let! x = op()\nlet y = x + 1\nTask.yield(y) }` (mixed) | `f = do\n  x <- op\n  let y = x + 1\n  Task.yield y` |
| `{ putStrLn("lol")\nTask.yield(unit) }` (bare expression) | `f = do\n  putStrLn "lol"\n  Task.yield unit` |

#### Complete Transpilation Table

| NeoHaskell source | Haskell type signature | Haskell definition |
| --- | --- | --- |
| `fun add(x: Int, y: Int) : Int = x + y` | `add :: Int -> Int -> Int` | `add x y = x + y` |
| `let pi : Float = 3.14` | `pi :: Float` | `pi = 3.14` |
| `fun identity<t>(x: t) : t = x` | `identity :: t -> t` | `identity x = x` |
| `fun show<t>(x: t) : Text where t: Show { toString(x) }` | `show :: (Show t) => t -> Text` | `show x = do\n  toString x` |
| `fun getTime() : Task<Time> { let! now = Clock.now()\nreturn Task.yield(now) }` | `getTime :: Task Time` | `getTime = do\n  now <- Clock.now\n  Task.yield now` |
| `fun compare<t>(a: t, b: t) : Ordering where t: Ord = Ord.compare(a, b)` | `compare :: (Ord t) => t -> t -> Ordering` | `compare a b = Ord.compare a b` |
| `fun convert<a, b>(x: a) : b where Convertible<a, b> { Convertible.convert(x) }` | `convert :: (Convertible a b) => a -> b` | `convert x = do\n  Convertible.convert x` |
| `fun greet<a>(s: a) : Text where a has name: Text { "Hello, " ++ s.name }` | `greet :: (HasField "name" a Text) => a -> Text` | `greet s = do\n  "Hello, " ++ name s` |

**Key token transformations (parser → emitter):**

- `fun` → two output lines: type signature + definition
- `(x: Type, y: Type)` → parameter names in definition, types in sig (arrow-separated)
- `<a, b>` → type variable binders (appear in constraints and type sig, not in definition)
- `where t: Trait` → `(Trait t) =>` prefix on type signature
- `where SomeClass<a, b>` → `(SomeClass a b) =>` prefix
- `where a has field: Type` → `(HasField "field" a Type) =>` prefix
- `= expr` (expression body) → `= expr` directly
- `{ stmts }` (block body) → `= do\n  stmts` (uniform `do` notation)
- `let x = e` in block → `let x = e` in `do`
- `let! x = e` in block → `x <- e` in `do`
- bare expression in block → expression line in `do`
- `return expr` → captured as `ReturnStatement`, only `expr` emitted (return keyword stripped)
- `fun getTime()` (zero args) → `getTime` in definition (no arguments applied)
- `Task<Int>` → `Task Int` (recursive angle-bracket conversion)
- `#(a, b)` tuple type → `(a, b)` in Haskell output

### 9. Cabal Changes

No new source directory is required. The `syntax` source directory was added in ADR-0043 and is
already listed in `nhcore.cabal` under `hs-source-dirs`.

**`exposed-modules`** (library stanza) — add after `Syntax.Comment`:

```text
Syntax.Function
```

**`other-modules`** (`nhcore-test-core` stanza) — add after `Syntax.CommentSpec`:

```text
Syntax.FunctionSpec
```

### 10. Usage Example

#### Quick Start (Jess at 10 PM)

```haskell
import Parser qualified
import Syntax.Function (TopLevelDecl (..))
import Syntax.Function qualified
import Layout qualified

-- Parse a single function definition and emit Haskell:
transpileFunction :: Text -> Result Parser.ParseError Text
transpileFunction source = do
  let result = source |> Parser.run Syntax.Function.topLevelDecl
  case result of
    Err err ->
      Err err
    Ok decl ->
      decl
        |> Syntax.Function.toHaskell
        |> Layout.render
        |> Ok
```

#### In a Transpiler Pipeline

```haskell
import Array qualified
import Parser qualified
import Result qualified
import Syntax.Function (TopLevelDecl)
import Syntax.Function qualified
import Layout qualified
import Text (Text)

-- | Transpile a list of NeoHaskell top-level declarations to Haskell source.
transpileDeclarations :: Array Text -> Result Parser.ParseError Text
transpileDeclarations sources = do
  let parseOne source = source |> Parser.run Syntax.Function.topLevelDecl
  let results = sources |> Array.map parseOne
  case results |> Array.sequence of
    Err err ->
      Err err
    Ok decls ->
      decls
        |> Array.map Syntax.Function.toHaskell
        |> Layout.stack
        |> Layout.render
        |> Ok
```

#### In Test Assertions

```haskell
module Syntax.FunctionSpec (spec) where

import Core
import Parser qualified
import Result qualified
import Syntax.Function (FunctionDef (..), FunctionBody (..), TypeConstraint (..))
import Syntax.Function qualified
import Layout qualified
import Test

spec :: Spec Unit
spec = do
  describe "Syntax.Function" do

    describe "functionDef" do
      it "parses a pure expression function" \_ -> do
        let result =
              "fun add(x: Int, y: Int) : Int = x + y"
                |> Parser.run Syntax.Function.functionDef
        case result of
          Err err ->
            fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
          Ok (FunctionDef { name = n, body = Expression {} }) ->
            n |> shouldBe "add"
          Ok other ->
            fail [fmt|Expected Expression body, got: #{toText other}|]

      it "parses a trait constraint" \_ -> do
        let source = "fun show<t>(x: t) : Text where t: Show { toString(x) }"
        let result = source |> Parser.run Syntax.Function.functionDef
        case result of
          Err err ->
            fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
          Ok (FunctionDef { constraints = cs }) ->
            case cs |> Array.head of
              Nothing ->
                fail [fmt|Expected a constraint, got none|]
              Just (TraitConstraint { variable = v, trait = tr }) -> do
                v |> shouldBe "t"
                tr |> shouldBe "Show"
              Just other ->
                fail [fmt|Expected TraitConstraint, got: #{toText other}|]
          Ok other ->
            fail [fmt|Expected FunctionDef, got: #{toText other}|]

    describe "toHaskell" do
      it "transpiles pure expression function" \_ -> do
        let source = "fun add(x: Int, y: Int) : Int = x + y"
        let result = source |> Parser.run Syntax.Function.topLevelDecl
        case result of
          Err err ->
            fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
          Ok decl ->
            decl
              |> Syntax.Function.toHaskell
              |> Layout.render
              |> shouldBe "add :: Int -> Int -> Int\nadd x y = x + y"
```

## Consequences

### Positive

1. **Unblocks transpiler (#465)**: the transpiler pipeline can parse and emit valid Haskell for all
   NeoHaskell function and constant forms — expression, block (pure and effectful), generic,
   and constrained.

2. **Extends the `Syntax.*` namespace**: `Syntax.Function` is the second module in the family. The
   pattern (`TopLevelDecl` union, named parsers, `toHaskell`) is established and will be replicated by
   `Syntax.Enum`, `Syntax.Pattern`, `Syntax.Lambda`, etc.

3. **Uniform `do` block transpilation**: all blocks transpile to Haskell `do` notation. No need to
   detect pure vs effectful at parse time or maintain two codegen paths. GHC handles `do` in pure
   context natively, simplifying both the AST and the emitter.

4. **`where` disambiguation is deterministic and tested**: three constraint forms have distinct AST
   constructors and a fixed ordering in `Parser.choice`. Ordering regressions are immediately detected
   by a dedicated test case (`a has field: Text` must not parse as `TraitConstraint`).

5. **`return` captured at statement level for future extensibility**: the `ReturnStatement` constructor
   preserves both the user's intent and its position in the block. When early returns gain semantic
   meaning (future ADR — e.g. inside `if` or loops), the information is already in the AST. For now
   it transpiles identically to `BareExpression`.

6. **Reuses Parser and Layout (ADRs 0042, 0044)**: no new package dependencies are introduced.
   `Syntax.Function` builds entirely on established nhcore primitives.

7. **`{-# INLINE #-}` on all public functions**: following the ADR-0042 pattern, all public functions
   that delegate to inner parsers or Layout functions are annotated for zero call-site overhead.

### Negative

1. **Opaque `Text` for expressions is a phase-1 gap**: `returnType`, `paramType`, `constType`,
   `bindingValue`, `result`, and `exprText` fields are all raw `Text`. The transpiler cannot validate
   or transform expression content inside function bodies — type errors, pipe-chain structure, and
   function call syntax pass through unmodified.

2. **Type annotation text conversion requires a balanced-bracket pass**: converting `Task<Result<Text, Int>>`
   → `Task (Result Text Int)` requires recursive nesting-aware substitution, not flat text replacement.
   This is a required subtask of #465, not optional.

3. **Balanced-delimiter scan for expressions is O(n)**: the scan cannot detect semantic errors (undefined
   variables, type mismatches). It produces syntactically plausible but semantically unverified output.

4. **`ReturnStatement` is a no-op constructor in phase 1**: it transpiles identically to `BareExpression`.
   If early-return semantics are never implemented, the constructor remains dead weight. Acceptable
   tradeoff for future-proofing the AST.

### Risks

1. **`where` ordering regression**: if `traitConstraint` were tried before `fieldConstraint` in
   `Parser.choice`, `a has name: Text` would be silently misidentified as `TraitConstraint { variable = "a", trait = "has" }` before failing. Any future reordering of the constraint parsers introduces this regression without a compile-time warning.

2. **Nested generic type conversion brittle for edge cases**: `convertTypeText` must handle
   `Task<Result<Text, Int>>` and `Array<#(Text, Int)>` correctly. A naive implementation produces
   incorrect Haskell for types with tuple type arguments (`Array (Text, Int)` vs `Array (#(Text, Int))`).

3. **Expression boundary in string literals**: the balanced-delimiter scan treats `"{"` as a literal
   character inside a string, but does not yet handle escape sequences (`\"`). A `\"` inside a string
   literal could prematurely terminate quote-context tracking.

4. **Zero-argument parsing ambiguity**: `fun getTime() : Task<Time>` must not be confused with a
   potential future declaration form `fun getTime : Task<Time>` (no parentheses). The parser must fail
   on the no-paren form with a clear error, not silently produce a `FunctionDef` with an empty parameter
   list and an incorrect return type of `()`.

### Mitigations

1. **Ordering regression test**: `FunctionSpec.hs` includes a test that parses
   `fun greet<a>(s: a) : Text where a has name: Text { s.name }` and asserts the single constraint is
   `FieldConstraint`, not `TraitConstraint`. This test is documented as a critical ordering guard.

2. **`convertTypeText` implemented as recursive descent**: the type-text converter uses a character-by-character pass tracking angle-bracket depth, not `Text.replace`. It is tested independently with nested generic inputs. Tracked as a required subtask of #465.

3. **String literal awareness in expression scanner**: the balanced-delimiter scanner enters a
   "string mode" on `"` and exits on the matching unescaped `"`, treating all characters (including
   `{`, `}`, `(`, `)`) inside string mode as non-structural. Escape sequence handling is documented
   as a known edge case in the module Haddock.

4. **Zero-argument error message**: if the parser encounters `fun name :` (no parentheses before `:`),
   it emits a `Parser.problem` with a targeted message:

   ```text
   function 'name' has no parameter list
   hint: use 'name()' for a zero-argument function
   ```

5. **`{-# INLINE #-}` on all thin wrappers**: `topLevelDecl`, `functionDef`, `constantDef`,
   `toHaskell`, `typeConstraint`, and `whereClause` are all annotated
   `{-# INLINE #-}`, following the ADR-0042 §13 pattern.

## References

- [#465: Function Syntax (fun keyword)](https://github.com/neohaskell/NeoHaskell/issues/465)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — parent issue
- [transpiler/design/syntax.md §1 — Functions](../../transpiler/design/syntax.md) — authoritative
  spec with all grammar, examples, transpilation table, and DX Council decisions
- [ADR-0042: Parser Library](0042-parser-library.md) — combinators used by all parsers in this module
- [ADR-0043: NeoHaskell Comment Syntax Parsing](0043-neohaskell-comment-syntax-parsing.md) — `Syntax.*`
  namespace precedent, `toHaskell` pattern, module structure
- [ADR-0044: Layout Library](0044-layout-library.md) — `Blueprint` type used by `toHaskell`
- [core/syntax/Syntax/Comment.hs](../../core/syntax/Syntax/Comment.hs) — implementation pattern to follow
- [core/test-core/Syntax/CommentSpec.hs](../../core/test-core/Syntax/CommentSpec.hs) — test pattern to follow
- [core/nhcore.cabal](../../core/nhcore.cabal) — cabal configuration showing multi-source-dir pattern
