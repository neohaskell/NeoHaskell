# NeoHaskell Syntax Specification

**Status:** In Progress  
**Last updated:** 2026-02-25

---

## Syntax Philosophy

Decided via DX Council review (12 language design experts, Feb 2025).

### Core Insight: Code Is Now AI-Generated, Human-Reviewed

The traditional tradeoff of "easy to write vs easy to read" has collapsed.
AI has infinite patience for writing; humans are the bottleneck at review.
Every syntax decision optimizes for **scanning, verifying, and auditing** — not typing.

### Overall Style: Kotlin-Structural

Kotlin-style brace syntax, chosen for the target audience (Java/C#/JS developers):

- **Braces everywhere, consistently** — no optional forms, no indentation-sensitivity, no exceptions
- **Postfix type annotations** — `name: Type` not `Type name` (greppable, scannable)
- **Explicit over implicit** — visible types, visible block boundaries, visible effect markers
- **Mandatory formatter** — ships with the language, enforces canonical style
- **No Kotlin baggage** — no `companion object`, no `sealed class`, no `var`, no nullable `?` types

### Novelty Budget

Every unfamiliar concept costs adoption. Spend the budget on semantics, not syntax.

| Feature | Cost | Why It's Worth It |
|---------|------|-------------------|
| `let!` for effects | Medium | Makes effectful code explicit — the whole point |
| `\|>` pipe | Low | Growing mainstream (Elixir, F#, shell) |
| `trait/impl` | Low | Rust-familiar, fast-growing |
| `@attr` | Very Low | Java/C#/Python all have decorators |
| Domain keywords | Medium | Self-documenting, core differentiator |
| Braces + familiar structure | **Zero** | Home for target audience |

### Guiding Quotes

> "Familiarity serves the writer. Simplicity serves the reviewer." — Rich Hickey (paraphrased)
>
> "The code that gets written is the code that's easier to write." — Fernando Borretti
>
> "Functional semantics in imperative clothing." — Richard Feldman

---

## Design Principles

1. **Elm as baseline** — Proven simplicity, limited feature set
2. **Rust naming** — Familiar concepts: `trait`, `impl`
3. **Domain keywords** — Event-sourced systems: `command`, `event`, `query`, `entity`
4. **Braces over layout** — Simpler parsing, familiar to Java/C#/JS devs
5. **Hackage compatible** — Transpiles to standard Haskell
6. **Pit of success** — Obvious path for noobs, trapdoors for power users

---

## Feature Roadmap

Priority order decided by DX Council (10 experts, Feb 2025).
Features marked ✅ are decided. Features marked ❌ are pending design.

### Foundations (already decided)

| Feature | Status | Summary |
|---------|--------|---------|
| Records & Domain Keywords | ✅ | `record`, `entity`, `command`, `query`, `agent` |
| Attributes | ✅ | `@attr` syntax |
| Generics | ✅ | `<t>` definition, `:<t>` application, lowercase type vars |
| Arrays | ✅ | `Array a` type, `[1, 2, 3]` literals |
| Tuples | ✅ | `#(a, b)` syntax |
| Pipes | ✅ | `\|>` left-to-right flow |
| Blocks | ✅ | `{ }` instead of `do` |
| Monadic Bind | ✅ | `let!` for effectful binding |

### Design Queue (in priority order)

| # | Feature | Status | Council Priority |
|---|---------|--------|-----------------|
| 1 | Functions | ✅ | Council-reviewed (R1: 12 experts, R2: 10 experts) |
| 2 | Union Types / ADTs | ✅ | Council-reviewed (12 experts) |
| 3 | Pattern Matching | ❌ | 10/10 Tier 1 — unanimous |
| 4 | Lambdas | ❌ | 7/10 Tier 1 |
| 5 | Error Handling | ❌ | 8/10 Tier 1-2 |
| 6 | Imports | ✅ | Council-reviewed (8 experts) |
| 7 | Comments | ❌ | 5/10 Tier 1-2 |
| 8 | String Interpolation | ❌ | 10/10 Tier 1-2 |
| 9 | Type Aliases | ✅ | Council-reviewed (5 experts) |
| 10 | Newtypes | ✅ | Council-reviewed (8 experts) |
| 11 | Destructuring | ❌ | 7/10 Tier 2 |
| 12 | Traits & Impl | ❌ | 6/10 Tier 2 |
| 13 | Visibility | ❌ | 6/10 Tier 2 |
| 14 | Guards | ❌ | 4/10 Tier 2-3 |
| 15 | Doc Comments | ❌ | 5/10 Tier 2-3 |

### Explicitly Deferred (post-1.0 or never)

| Feature | Council Verdict |
|---------|----------------|
| Operator definition | 7/10 skip — `\|>` is enough |
| Anonymous records | 6/10 skip — named records are better for domain modeling |
| Where clauses | 4/10 skip — `let` covers 90% of cases |
| Handler syntax | Design later — needs real usage to inform |
| Macros / metaprogramming | 10/10 unanimous skip |
| HKT / dependent types | 10/10 unanimous skip |
| Separate `enum` keyword | 8/10 skip — ADTs cover this |
| Implicit conversions | 9/10 skip |
| Block comments `/* */` | Skip — destroys line-by-line lexing |

---

## Foundations ✅

### Records

Braces for all record definitions. Commas separate fields (trailing allowed).

```neohaskell
record Person {
  name: Text,
  age: Int,
}

-- One-liner form
record Point { x: Float, y: Float }

-- Generic records (lowercase type params — required for Haskell transpilation)
record Result<ok, err> {
  value: ok,
  error: err,
}
```

**Construction** uses the same colon syntax with values instead of types:

```neohaskell
let p = Person { name: "Scott", age: 47 }

-- Multi-line construction
let r = Result {
  value: 42,
  error: "none",
}
```

### Domain Keywords

All domain keywords use braces, consistent with records and functions.

```neohaskell
entity Product {
  name: Text,
  @embedded description: Text,
  price: Money,
}

command CreateProduct {
  name: Text,
  price: Money,
}

query GetProduct {
  id: ProductId,
}

agent InvoiceAssistant {
  model: openai "gpt-4o",
  can:
    CreateInvoice
    SendEmail,
  prompt: """
    You are a billing assistant for small businesses.
  """,
}
```

### Attributes

`@attr` syntax, universal convention (Java/Python/TS/Kotlin):

```neohaskell
record Product {
  name: Text,
  @embedded description: Text,
  @optional price: Money,
  @indexed @unique sku: Text,
}
```

### Generics

Definition: `<t>` (lowercase — type variables MUST be lowercase, uppercase = concrete types).
Application: `:<t>`.

```neohaskell
record Result<ok, err> {
  value: ok,
  error: err,
}

result = parse:<Money> input
items = empty:<Array<Product>>
```

**Why lowercase?** Haskell's type system uses case to distinguish type variables (`a`, `ok`) from
type constructors (`Int`, `Text`). The transpiler cannot reliably convert `<T>` → `t` because
uppercase identifiers in Haskell are constructors, not variables. Lowercase is non-negotiable.

**Convention:** Use semantic names (`ok`, `err`, `item`, `key`, `value`) over single letters.
Single letters (`a`, `b`, `f`) are valid but discouraged outside generic utility functions.

### Primitives

**Arrays:** `Array a` type, `[1, 2, 3]` literals.  
**Tuples:** `#(a, b)` — visually distinct from function calls.  
**Pipes:** `|>` left-to-right data flow.  
**Blocks:** `{ }` for sequencing/effects (replaces `do`).  
**Monadic bind:** `let!` marks effectful binding. `return` wraps the result.

### Call Syntax

```neohaskell
foo(arg1, arg2)      -- canonical
foo arg1 arg2        -- whitespace (valid, normalized by formatter)
```

`neo fmt` normalizes to `foo(arg1, arg2)`.

### Equivalence Table (for Java/C# devs)

| NeoHaskell | Mental Equivalent |
|-----------|-------------------|
| `foo(a, b) { ... }` | Method with body |
| `let! x = op` | `var x = await op` |
| `x \|> f \|> g` | `x.f().g()` |
| `#(a, b)` | `new Tuple<>(a, b)` |
| `Person { name: "X" }` | `new Person(name: "X")` |
| `record Foo { x: Int }` | `class Foo { int x; }` |
| `{ }` | `{ }` |

---

## 1. Functions ✅

Designed via DX Council review (12 experts Round 1, 10 experts Round 2, Feb 2025).

### Summary

| Decision | Choice | Council Support |
|-|-|-|
| Definition keyword | `fun` | 11/12 |
| Constant keyword | `let` | 9/10 |
| Type signatures | Inline (same line) | User decision |
| Body forms | `= expr` and `{ block }` | Accepted |
| Effectful binding | `let!` | Unanimous |
| Effectful return | `return` (monadic pure) | 5/10 (contested — see notes) |
| Pure block return | Implicit (last expression) | Unanimous |
| Type constraints | `where` clause, no parens | 10/12 |
| Generic variables | Lowercase required (`<t>`, not `<T>`) | User decision (non-negotiable) |
| Zero-arg effectful | Require `()` | Majority |

### Constants

```neohaskell
let pi : Float = 3.14
let maxRetries : Int = 3
let greeting : Text = "Hello, world!"
```

- `let` at top level, matching `let` inside function blocks
- Type annotation required at top level
- Immutable always (no `var`, no `mut`)

### Pure Functions

**Expression form** — single expression, `= expr`:

```neohaskell
fun add(x: Int, y: Int) : Int = x + y

fun negate(x: Bool) : Bool = not(x)

fun greet(name: Text) : Text = "Hello, " ++ name
```

**Block form** — multiple bindings, implicit return (last expression):

```neohaskell
fun distance(x1: Float, y1: Float, x2: Float, y2: Float) : Float {
    let dx = x2 - x1
    let dy = y2 - y1
    sqrt(dx * dx + dy * dy)
}

fun clamp(value: Int, lo: Int, hi: Int) : Int {
    let clamped = max(lo, value)
    min(hi, clamped)
}
```

**Rules:**
- `= expr` → pure, single expression. No `let` bindings, no `let!`, no `return`.
- `{ block }` with only `let` → pure multi-line. Last expression is the return value.
- No explicit `return` in pure functions — it is a compile error.

### Effectful Functions

Block form with `let!` for effectful binding and `return` for the result:

```neohaskell
fun readAndPrint(path: Text) : Task<Text> {
    let! content = readFile(path)
    let! _ = print(content)
    return content
}

fun getTime() : Task<Time> {
    let! now = Clock.now()
    return now
}

fun fetchUser(id: UserId) : Task<User> {
    let! response = http.get("/users/" ++ show(id))
    let! user = parseJson(response.body)
    return user
}
```

**Rules:**
- `{ block }` with any `let!` → effectful. Must end with `return`.
- `return` lifts a pure value into the effect context (Haskell's `pure`/`return`).
- `return` is ONLY valid inside effectful blocks — compile error elsewhere.
- Zero-argument effectful functions require `()`: `fun getTime()`, not `fun getTime`.

**Error: `return` in pure context:**

```
error: `return` is only valid in effectful blocks
  --> src/Main.nh:3:5
  |
3 |     return x + y
  |     ^^^^^^ unnecessary in pure blocks
  |
  = hint: remove `return` — in pure blocks, the last expression is returned
```

**Error: `let!` in expression form:**

```
error: `let!` requires a block body { }
  --> src/Main.nh:5:1
  |
5 | fun fetch(url: Text) : Text = let! resp = http(url)
  |                               ^^^^ effectful binding requires block syntax
  |
  = hint: change `= ...` to `{ ... }` for effectful functions
```

### Generic Functions

Type variables are **lowercase** (like Haskell/Elm). Concrete types are uppercase.
The scanner distinguishes `LOWER_IDENT` (type variable) from `UPPER_IDENT` (type constructor) —
zero-lookahead disambiguation.

```neohaskell
fun identity<t>(x: t) : t = x

fun map<a, b>(f: a -> b, list: Array<a>) : Array<b> {
    Array.map(f, list)
}

fun pair<a, b>(x: a, y: b) : #(a, b) = #(x, y)
```

### Type Constraints (`where` clause)

Typeclass constraints go in a `where` clause after the return type.
Comma-separated, no parentheses.

**Single-parameter constraints** use `variable: Constraint` sugar:

```neohaskell
fun show<t>(x: t) : Text where t: Show {
    toString(x)
}

fun compare<t>(a: t, b: t) : Ordering where t: Ord {
    Ord.compare(a, b)
}

-- Multiple constraints on the same variable:
fun showAndCompare<t>(a: t, b: t) : Text where t: Show, t: Ord {
    let ord = Ord.compare(a, b)
    toString(ord)
}

-- Expression form with constraints:
fun isEqual<t>(a: t, b: t) : Bool where t: Eq = Eq.equal(a, b)
```

**Multi-parameter constraints** use bare type-application form:

```neohaskell
-- Haskell: foo :: (Show a, SomeClass Int Text a) => a -> Text
fun foo<a>(x: a) : Text where a: Show, SomeClass<Int, Text, a> {
    doSomething(x)
}

-- Haskell: convert :: (Convertible a b) => a -> b
fun convert<a, b>(x: a) : b where Convertible<a, b> {
    Convertible.convert(x)
}
```

**Grammar:** The `where` clause accepts a comma-separated list of constraints.
Each constraint is either:
- `lowercase: UpperType` — single-parameter sugar (most common)
- `UpperType<args>` — multi-parameter type application (power-user)

The parser disambiguates by the first token: `LOWER_IDENT :` → sugar form;
`UPPER_IDENT <` → type-application form.

**Council note:** Multi-parameter typeclass constraints are a power-user feature.
Most application code uses only single-parameter constraints. If your `where` clause
has more than 2-3 constraints, consider whether explicit function passing would be clearer.

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-|-|
| `fun f(x: Int) : Int = x + 1` | `f :: Int -> Int` | 
| | `f x = x + 1` |
| `fun f(x: Int) : Int { let y = x + 1; y * 2 }` | `f :: Int -> Int` |
| | `f x = let y = x + 1 in y * 2` |
| `fun f(x: Int) : Task<Int> { let! y = get(); return y }` | `f :: Int -> Task Int` |
| | `f x = do { y <- get; pure y }` |
| `let pi : Float = 3.14` | `pi :: Float` |
| | `pi = 3.14` |
| `fun show<t>(x: t) : Text where t: Show { toString(x) }` | `show :: (Show t) => t -> Text` |
| | `show x = toString x` |

### What Functions Do NOT Include

These are designed in later features:

- Pattern matching in arguments → Feature #3
- Guards → Feature #14
- Lambdas → Feature #4
- Default arguments → Not planned
- Overloading → Via traits (Feature #12)
- Variadic arguments → Never
- Multi-equation definitions → Use `case..of` (Feature #3)
- Point-free / operator sections → Not supported
- Infix function syntax → Not supported

### Design Rationale

**Why `fun`?** Cognitive anchor for scanning. Greppable (`fun name` finds definitions).
Resilient parsing (commit at token 1). Matches Kotlin. (matklad, Klabnik — 11/12 support)

**Why `let` for constants?** Consistency with `let` inside blocks. Same keyword, same semantics
(immutable binding), regardless of scope. F# does this successfully. (9/10 support)

**Why `return` and not `yield`?** `yield` was unanimously rejected by the council (12/12) — it
collides with generator semantics in JS, C#, Python, and Kotlin. `return` matches F# computation
expressions. Council was split 5/10 on `return` vs alternatives (`pure`, implicit). The `return`
choice was made by the design lead for familiarity with the target audience.

**Why lowercase generics?** Scanner-level disambiguation: `LOWER_IDENT` = type variable,
`UPPER_IDENT` = concrete type. Zero-lookahead parser benefit. Matches Haskell/Elm convention.
Design lead decision (non-negotiable).

**Why no multi-equation pattern matching?** Use `case..of` instead. Keeps function definition
syntax uniform. One definition per function name. Avoids the Haskell pitfall of scattered equations.

**Why `where` without parens?** Matches Rust, Kotlin, Swift convention. Parens suggest
tuple/grouping that isn't semantically meaningful. The comma is unambiguous in context —
`where` starts the constraints, `{` or `=` ends them. (10/12 support)

---

## 2. Union Types / ADTs ✅

Designed via DX Council review (12 experts, Feb 2025).

### Summary

| Decision | Choice | Council Support |
|-|-|-|
| Definition keyword | `type` | 5/12 (Elm-aligned, tie-break: Elm proximity) |
| Variant separator | `\|` (pipe) | 12/12 — unanimous |
| Positional data | `Variant(args)` — paren syntax | 7/12 |
| Record variants | `Variant { field: Type }` | 7/12 |
| Generic parameters | Lowercase `<a>`, `<ok, err>` | Non-negotiable (locked) |
| Recursive types | No special syntax | 12/12 — unanimous |
| Auto-derived traits | `Show`, `Eq` auto-derived for all types | 4/12 explicit, tie-break: Elm proximity |
| Additional deriving | `deriving Ord, Bounded` explicit | Accepted |
| Field accessors | No auto-generated accessors for variant records | Consensus (safety) |
| Simple enums | `type Name = A \| B \| C` | 12/12 — unanimous |

### Simple Enums (Nullary Constructors)

```neohaskell
type Direction = North | South | East | West

type Color = Red | Green | Blue

-- Multi-line form (leading pipe on all variants)
type HttpMethod =
  | Get
  | Post
  | Put
  | Delete
  | Patch
```

No data carried. Each variant is a distinct value of the type.
The formatter enforces leading `|` on all variants in multi-line form for visual consistency.

### Data-Carrying Variants (Positional)

```neohaskell
type Maybe<a> = Nothing | Just(a)

type Result<ok, err> = Ok(ok) | Err(err)

type Shape = Circle(Float) | Rectangle(Float, Float)

-- Multi-field positional
type Expr =
  | Literal(Int)
  | Add(Expr, Expr)
  | Multiply(Expr, Expr)
```

Parenthesized fields after the variant name. Reads as a constructor call —
familiar to Java/C#/JS developers. Use positional form for 1–2 fields with obvious roles.

### Record Variants (Named Fields)

```neohaskell
type Shape =
  | Circle { radius: Float }
  | Rectangle { width: Float, height: Float }

type PaymentMethod =
  | CreditCard { number: Text, expiry: Date }
  | BankTransfer { iban: Text }
  | Cash
```

Braces with named fields, consistent with `record` syntax. Use record variants when
fields have non-obvious roles or there are 3+ fields.

**No auto-generated field accessors.** Record variant fields are accessed only via
pattern matching (Feature #3). This prevents Haskell's partial accessor problem where
`shape.radius` would crash on a `Rectangle`. (King — critical safety concern)

### Mixed Variants

```neohaskell
type Token =
  | Identifier(Text)
  | Number(Int)
  | StringLit(Text)
  | Operator { symbol: Text, precedence: Int }
  | EOF
```

Nullary, positional, and record variants can be mixed freely in the same type.

### Generic Union Types

```neohaskell
type Maybe<a> = Nothing | Just(a)

type Result<ok, err> = Ok(ok) | Err(err)

type Either<a, b> = Left(a) | Right(b)

type Tree<a> = Leaf(a) | Node(Tree<a>, Tree<a>)
```

Type parameters are **lowercase** (non-negotiable — matches Haskell's type variable convention).
Use semantic names: `ok`, `err`, `item`, `key`, `value` over single letters where meaningful.
Single letters (`a`, `b`) are valid for generic utility types.

**Note:** The `record Result<ok, err>` example in Foundations demonstrates generic record syntax.
The standard `Result` type is defined as a union type: `type Result<ok, err> = Ok(ok) | Err(err)`.

### Recursive Types

```neohaskell
type List<a> = Nil | Cons(a, List<a>)

type Expr =
  | Literal(Int)
  | Add(Expr, Expr)
  | If { condition: Expr, thenBranch: Expr, elseBranch: Expr }

type Json =
  | JsonNull
  | JsonBool(Bool)
  | JsonNumber(Float)
  | JsonString(Text)
  | JsonArray(Array<Json>)
  | JsonObject(Array<#(Text, Json)>)
```

No special syntax needed. Self-reference is natural — the type name is in scope
within its own definition. No `indirect` keyword (Swift) or `Box` wrapper (Rust)
needed — Haskell handles recursion natively via lazy evaluation.

### Deriving

`Show` and `Eq` are **auto-derived** for all union types. Additional derivations use
explicit `deriving`:

```neohaskell
-- Show and Eq are automatic — no annotation needed
type Direction = North | South | East | West

-- Add Ord for ordering
type Priority = Low | Medium | High
  deriving Ord

-- Multiple additional derivations
type Color = Red | Green | Blue
  deriving Ord, Bounded
```

The transpiler always emits `deriving (Show, Eq)` plus any user-specified additions.
If the user writes `deriving Ord`, the Haskell output is `deriving (Show, Eq, Ord)`.

### Construction

```neohaskell
-- Nullary
let d = North
let method = Get

-- Positional
let m = Just(42)
let r = Ok("success")
let shape = Circle(5.0)

-- Record variant
let payment = CreditCard { number: "4111...", expiry: Date(2025, 12) }
let rect = Rectangle { width: 10.0, height: 5.0 }

-- In expressions
let result = if valid then Ok(value) else Err("invalid")
```

Construction syntax mirrors the definition syntax:
- Nullary: bare name
- Positional: `Name(args)`
- Record: `Name { field: value }`

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-|-|
| `type Direction = North \| South \| East \| West` | `data Direction = North \| South \| East \| West` |
| | `  deriving (Show, Eq)` |
| `type Maybe<a> = Nothing \| Just(a)` | `data Maybe a = Nothing \| Just a` |
| | `  deriving (Show, Eq)` |
| `type Result<ok, err> = Ok(ok) \| Err(err)` | `data Result ok err = Ok ok \| Err err` |
| | `  deriving (Show, Eq)` |
| `type Shape = Circle { radius: Float } \| Rect { w: Float, h: Float }` | `data Shape = Circle { radius :: Float } \| Rect { w :: Float, h :: Float }` |
| | `  deriving (Show, Eq)` |
| `type Priority = Low \| Medium \| High deriving Ord` | `data Priority = Low \| Medium \| High` |
| | `  deriving (Show, Eq, Ord)` |
| `Just(42)` | `Just 42` |
| `Circle { radius: 5.0 }` | `Circle { radius = 5.0 }` |
| `Ok("success")` | `Ok "success"` |

### `record` vs `type` Relationship

| Construct | Use Case | Haskell Output |
|-|-|-|
| `record Person { name: Text, age: Int }` | Single-constructor, named fields | `data Person = Person { name :: Text, age :: Int }` |
| `type Maybe<a> = Nothing \| Just(a)` | Multi-constructor (sum type) | `data Maybe a = Nothing \| Just a` |
| `type CustomerId = CustomerId(Int)` | Single-constructor wrapper (ADT) | `data CustomerId = CustomerId Int` |

`record` is for single-constructor types with named fields (domain modeling).
`type` is for multi-constructor types (sum types / unions).
Single-constructor `type` declarations are valid for wrapper types, but `record` is
preferred when fields have names.

### What Union Types Do NOT Include

These are designed in later features:

- Pattern matching on variants → Feature #3
- Exhaustive case analysis → Feature #3
- Destructuring bindings → Feature #11
- Trait implementations for variants → Feature #12
- Guards on variant matching → Feature #14
- GADTs / indexed types → Not planned (post-1.0)
- Existential types → Not planned
- Strict fields → Not planned (Haskell default is lazy)
- Newtype (zero-cost wrapper) → Feature #10

### Design Rationale

**Why `type`?** Elm uses `type` for custom types and it's the most neutral keyword for
the target audience. `enum` (2/12) conflicts with the deferred "no separate enum keyword"
decision (8/10 skip). `union` (2/12) sounds like C's untagged union to some developers.
`data` is Haskell-specific. `type` names the *thing being created*, not the mechanism.
(Czaplicki, Wlaschin, Syme, Feldman, Nystrom — 5/12 support, Elm proximity tie-break)

**Why `Just(a)` not `Just a`?** Parenthesized constructor syntax reads as a function call —
immediately familiar to Java/C#/JS developers. `Just a` (Haskell/Elm style) requires
understanding curried application. The paren form is a deliberate strangeness budget savings.
(Wlaschin, Syme, Feldman, Klabnik, Breslav, King, Nystrom — 7/12 support)

**Why record variants?** `Circle { radius: Float }` enables domain modeling with named
fields directly in union types. This is the "making illegal states unrepresentable" pattern
(King, Wlaschin). Record variants use the same brace syntax as `record` declarations for
consistency. (7/12 support)

**Why no field accessors?** Haskell's record syntax in sum types generates partial functions
(`radius :: Shape -> Float`) that crash on the wrong variant. NeoHaskell prevents this by
not generating accessors — fields are accessed only via pattern matching, which is exhaustive
and total. (King — critical safety concern, consensus support)

**Why auto-derive `Show` and `Eq`?** Elm auto-derives structural equality. Kotlin
auto-generates `equals`/`toString` for data classes. Explicit `deriving Show, Eq` on every
type is Haskell ceremony that confuses beginners who expect `==` to "just work." Auto-deriving
saves strangeness budget. Additional traits (`Ord`, `Bounded`) require explicit `deriving`.
(Czaplicki, Syme, Feldman, Breslav — tie-break: Elm proximity, Kotlin precedent)

**Why `record` and `type` are separate?** `record` is for single-constructor types with
named fields (transpiles to single-constructor `data` with record syntax). `type` is for
multi-constructor types (sum types). They serve different purposes and the distinction is
clean. (Hickey's orthogonality concern addressed)

**Why no special recursive syntax?** Self-reference is natural in type definitions.
`type List<a> = Nil | Cons(a, List<a>)` is self-evidently recursive. No `indirect` keyword
(Swift) or `Box` wrapper (Rust) needed — Haskell handles recursion natively via lazy
evaluation. (12/12 unanimous)

**Why leading `|` in multi-line form?** Every variant gets the same visual prefix, making
it easy to add, remove, or reorder variants. The formatter enforces this for multi-line
declarations. Single-line declarations omit the leading pipe.
(Hermans — visual chunking reduces cognitive load)

---

## 3. Pattern Matching ✅

Designed via DX Council review (11 experts, Feb 2025).

### Summary

| Decision | Choice | Council Support |
|-|-|-|
| Match keyword | `match` | 10/11 |
| Branch arrow | `=>` | 9/11 |
| Braces | `match x { ... }` | 11/11 — unanimous |
| Exhaustiveness | Compile error (not warning) | 11/11 — unanimous |
| Wildcard | `_` | 11/11 — unanimous |
| Nested patterns | No depth limit | 11/11 — unanimous |
| As-patterns | `as` keyword (not `@`) | 9/11 |
| Literal patterns | Numbers, strings, booleans | 11/11 — unanimous |
| Guards in arms | `if` keyword | 9/11 |
| OR patterns | `\|` separator | Accepted |
| Arm separators | Commas (trailing allowed) | Consistent with records |
| `let` patterns | Defer refutable to Feature #11 | 8/11 |

### Basic Syntax

`match` is an expression — it always returns a value. All arms must have the same type.
Arms are separated by commas (trailing comma allowed), consistent with records and arrays.

```neohaskell
match expr {
  Pattern1 => result1,
  Pattern2 => result2,
  _ => fallback,
}
```

**Single-line form** (formatter allows for simple matches):

```neohaskell
let label = match direction { North => "N", South => "S", East => "E", West => "W" }
```

**In expressions:**

```neohaskell
let area = match shape {
  Circle { radius: r } => pi * r * r,
  Rectangle { width: w, height: h } => w * h,
}
```

### Constructor Patterns

Match on union type constructors. Syntax mirrors construction — parenthesized args for
positional variants, braces for record variants.

```neohaskell
-- Nullary constructors
match direction {
  North => handleNorth(),
  South => handleSouth(),
  East  => handleEast(),
  West  => handleWest(),
}

-- Positional data
match result {
  Ok(value)  => process(value),
  Err(error) => log(error),
}

-- Record variants (field punning: bare name binds to same-named variable)
match shape {
  Circle { radius }          => pi * radius * radius,
  Rectangle { width, height } => width * height,
}

-- Record variants with renaming
match shape {
  Circle { radius: r }              => pi * r * r,
  Rectangle { width: w, height: h } => w * h,
}
```

**Field punning:** `Circle { radius }` binds the `radius` field to a variable named `radius`.
This is shorthand for `Circle { radius: radius }`. Consistent with record construction syntax.

### Literal Patterns

Match on concrete values. Numbers, strings, and booleans are supported.

```neohaskell
match statusCode {
  200 => "OK",
  404 => "Not Found",
  500 => "Internal Server Error",
  _   => "Unknown",
}

match answer {
  True  => "yes",
  False => "no",
}

match command {
  "quit" => exit(),
  "help" => showHelp(),
  _      => run(command),
}
```

**Note:** Literal patterns on infinite domains (Int, Text) always require a wildcard or
catch-all arm for exhaustiveness. Boolean patterns (`True`/`False`) are exhaustive without
a wildcard because `Bool` is a two-variant union type.

### Wildcard and Variable Patterns

`_` matches any value and binds nothing. A bare lowercase name matches any value and binds it.

```neohaskell
-- Wildcard: match but don't bind
match pair {
  #(x, _) => x,   -- ignore second element
}

-- Variable: match and bind
match maybeUser {
  Just(user) => greet(user),
  Nothing    => "anonymous",
}

-- Multiple wildcards are independent (unlike variables)
match triple {
  #(_, _, z) => z,  -- only bind the third element
}
```

**Convention:** Prefer explicit variant names over `_` on union types. Using `_` as a
catch-all on a union type silences the exhaustiveness checker when new variants are added.
The linter warns: *"Consider matching all variants explicitly so the compiler can help
you when new variants are added."*

### Nested Patterns

Patterns can be nested to arbitrary depth. No artificial limit.

```neohaskell
-- Nested Maybe
match nestedMaybe {
  Just(Just(value)) => value,
  Just(Nothing)     => defaultValue,
  Nothing           => defaultValue,
}

-- Nested constructors
match expr {
  Add(Literal(a), Literal(b)) => Literal(a + b),
  Multiply(Literal(0), _)     => Literal(0),
  other                       => other,
}

-- Mixed nesting: constructor + tuple
match result {
  Ok(#(name, age)) => greet(name, age),
  Err(message)     => log(message),
}
```

### Tuple Patterns

Use `#()` syntax, consistent with tuple literals.

```neohaskell
match point {
  #(0, 0)   => "origin",
  #(x, 0)   => "on x-axis",
  #(0, y)   => "on y-axis",
  #(x, y)   => "general point at " ++ show(x) ++ ", " ++ show(y),
}

-- Nested tuple
match pair {
  #(#(a, b), #(c, d)) => a + b + c + d,
}
```

### As-Patterns

Bind the entire matched value while also destructuring it. Uses the `as` keyword
(not `@`, which is reserved for attributes).

```neohaskell
match list {
  Cons(head, _) as whole => #(head, whole),
  Nil                    => #(0, Nil),
}

match maybeUser {
  Just(user) as original => {
    let processed = transform(user)
    #(original, processed)
  },
  Nothing => #(Nothing, defaultUser),
}
```

**Syntax:** `Pattern as name` — the `as` binds the entire value that matched `Pattern`
to `name`. The `as` keyword reads naturally: "match this as `name`."

### OR Patterns

Match multiple patterns in a single arm using `|`. All alternatives must bind the
same set of variables with the same types.

```neohaskell
match color {
  Red | Blue   => "cool",
  Green        => "warm",
  Yellow | Orange => "hot",
}

match token {
  Identifier(name) | StringLit(name) => process(name),
  Number(n)                          => processNum(n),
  EOF                                => done(),
}
```

**Rule:** All alternatives in an OR pattern must bind the same variables. This is a
compile error:

```
error: OR pattern alternatives bind different variables
  --> src/Main.nh:3:3
  |
3 |   Just(x) | Nothing => ...
  |   ^^^^^^^ binds `x`   ^^^^^^^ binds nothing
  |
  = hint: use separate arms for patterns that bind different variables
```

### Guards

Refine a pattern with a boolean condition using `if`. Guards are checked after the
pattern matches. If the guard fails, matching continues with the next arm.

```neohaskell
match value {
  Just(n) if n > 0  => "positive",
  Just(n) if n < 0  => "negative",
  Just(_)           => "zero",
  Nothing           => "absent",
}

-- Guards with record variants
match shape {
  Circle { radius: r } if r > 100.0 => "large circle",
  Circle { radius: r }              => "small circle with radius " ++ show(r),
  Rectangle { width: w, height: h } if w == h => "square",
  Rectangle { width, height }       => "rectangle",
}
```

**Exhaustiveness and guards:** A guarded arm does NOT count as exhaustive coverage.
The compiler treats `Just(n) if n > 0` as potentially non-matching. You must provide
an unguarded arm or wildcard to cover the remaining cases:

```
error: non-exhaustive patterns
  --> src/Main.nh:2:1
  |
2 | match value {
  | ^^^^^ missing coverage for: Just(_) without guard
  |
  = hint: add an unguarded `Just(_)` arm or a wildcard `_` arm
```

### Exhaustiveness Checking

All `match` expressions must cover every possible value of the scrutinee type.
Non-exhaustive matches are a **compile error**, not a warning.

```neohaskell
-- ✅ Exhaustive: all variants covered
match maybe {
  Just(x) => x,
  Nothing => 0,
}

-- ✅ Exhaustive: wildcard covers remaining cases
match statusCode {
  200 => "OK",
  404 => "Not Found",
  _   => "Other",
}

-- ❌ Compile error: missing Nothing
match maybe {
  Just(x) => x,
}
```

**Error message quality** is critical. The compiler lists missing patterns in NeoHaskell
syntax (not Haskell syntax):

```
error: non-exhaustive patterns
  --> src/Main.nh:5:1
  |
5 | match direction {
  | ^^^^^ missing patterns
  |
  = the following patterns are not covered:
      - East
      - West
  = hint: add the missing arms, or use `_` as a catch-all
```

**Algorithm:** Based on Maranget's "Warnings for Pattern Matching" (2007) — the same
algorithm used by Elm, Rust, and GHC. Handles nested patterns, OR patterns, and
literal patterns correctly.

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-|-|
| `match x { Just(v) => v, Nothing => 0 }` | `case x of { Just v -> v; Nothing -> 0 }` |
| `match s { Circle { radius: r } => r, _ => 0 }` | `case s of { Circle { radius = r } -> r; _ -> 0 }` |
| `match x { Just(v) if v > 0 => v, _ => 0 }` | `case x of { Just v \| v > 0 -> v; _ -> 0 }` |
| `match c { Red \| Blue => 1, _ => 2 }` | `case c of { Red -> 1; Blue -> 1; _ -> 2 }` |
| `match x { Just(v) as whole => whole, _ => x }` | `case x of { whole@(Just v) -> whole; _ -> x }` |
| `match p { #(a, b) => a + b }` | `case p of { (a, b) -> a + b }` |
| `match n { 0 => "zero", _ => "other" }` | `case n of { 0 -> "zero"; _ -> "other" }` |

**Key transformations:**
- `match` → `case...of`
- `=>` → `->`
- `Just(v)` → `Just v` (remove parens, space-separated)
- `Circle { radius: r }` → `Circle { radius = r }` (colon → equals)
- `Pattern as name` → `name@(Pattern)` (flip order, `as` → `@`)
- `Pat1 \| Pat2 => body` → duplicate arms: `Pat1 -> body; Pat2 -> body`
- `Pattern if guard` → `Pattern | guard` (Haskell guard syntax)
- `#(a, b)` → `(a, b)` (remove `#` prefix)
- Commas between arms → semicolons (Haskell explicit layout)

### What Pattern Matching Does NOT Include

These are designed in later features:

- Destructuring in `let`/`fun` arguments → Feature #11
- Full guard syntax (multi-way guards, pattern guards) → Feature #14
- View patterns (function application in pattern position) → Not planned
- Pattern synonyms → Not planned
- Bang patterns (strict matching) → Not planned (Haskell-specific)
- OR patterns with different bindings → Never (type safety)
- Range patterns (`1..10`) → Not planned (use guards)
- Multi-equation function definitions → Never (use `match` inside `fun`)

### Design Rationale

**Why `match`?** Single keyword, greppable (`match x` finds all match sites), clean with
braces (`match x { ... }`). `case...of` is a two-keyword construct where `of` adds nothing
when braces delimit the body. `when` (Kotlin) reads as a conditional, not structural
decomposition. `match` is what Rust and F# use — growing mainstream familiarity.
(Klabnik, Nystrom, Czaplicki, matklad, Wlaschin — 10/11 support)

**Why `=>`?** NeoHaskell uses `->` for function types (`a -> b`). Reusing `->` in match
arms creates visual collision — the reader must track two meanings for one symbol.
`=>` is the "maps to" arrow in JavaScript, C#, Kotlin, and Scala. It creates clean visual
distinction: `->` means type, `=>` means branch. (Klabnik, Nystrom, matklad — 9/11 support)

**Why `as` not `@`?** `@` is already used for attributes (`@embedded`, `@optional`).
Reusing `@` for as-patterns creates a "wat" moment (Bernhardt). `as` reads as English
("match this *as* name"), is used in F#, Python, C#, and TypeScript for similar binding
purposes, and has no competing meaning in NeoHaskell. (Klabnik, Nystrom, Czaplicki — 9/11)

**Why `if` for guards?** `if` is the universal conditional keyword. Rust, Swift, and Scala
all use `if` for pattern guards. `when` (F#) conflicts with potential future use. `where`
is already used for type constraints. `if` reads naturally: "match Just(v) *if* v > 0."
(Klabnik, Nystrom, matklad, Wlaschin — 9/11 support)

**Why exhaustiveness as error?** This is the entire value proposition of union types +
pattern matching. A warning that developers ignore defeats the purpose. Elm, Rust, and
Swift all make non-exhaustive matches a compile error. The error message quality is
critical — list missing patterns in NeoHaskell syntax, not Haskell syntax.
(11/11 unanimous — strongest consensus of any decision)

**Why commas between arms?** Consistent with NeoHaskell's brace style: records use commas,
arrays use commas, function parameters use commas. Match arms are items in a braced
container. Rust also uses commas between match arms. Trailing comma allowed.

**Why OR patterns?** Low complexity, high utility. `Red | Blue => "cool"` avoids
duplicating arm bodies. The `|` is already the variant separator in union type definitions,
so the semantic connection is natural (disjunction). Rust and F# both support OR patterns.

**Why defer `let` patterns?** `let Just(x) = expr` is a refutable pattern — it can fail
at runtime if the value is `Nothing`. This violates NeoHaskell's safety guarantee.
Irrefutable patterns (tuples, records) in `let` are safe and belong in Feature #11
(Destructuring). Refutable patterns need a clear failure story before being allowed.
(Czaplicki, Nystrom, Klabnik — 8/11 defer)

**Why no multi-equation functions?** NeoHaskell requires one definition per function name
(locked decision from Feature #1). Use `match` inside the function body instead. This
keeps function definitions uniform and scannable — `fun name` always finds the definition.

---

## 4. Lambdas ❌

_To be designed._

---

## 5. Error Handling ❌

_To be designed._

---

## 6. Imports ✅

Designed via DX Council review (8 experts, Feb 2025).

### Summary

| Decision | Choice | Council Support |
|-|-|-|
| Keyword | `import` | 8/8 — unanimous |
| Selective imports | Braces `{ }` | 8/8 — unanimous |
| Qualified/aliased | `import Foo as F` (no `qualified`) | 8/8 — unanimous |
| Hiding | Omitted (Elm approach) | 6/8 — Elm tie-break |
| Module = file | One module per file | 8/8 — unanimous |
| Re-exports | `export import` modifier | 8/8 — unanimous |
| Path style | Dot-path (`Data.Map`) | 8/8 — unanimous |

### Module Declaration

Every `.nh` file is exactly one module. The module name matches the file path:

```neohaskell
-- File: src/Data/Map.nh
module Data.Map
```

- Module declaration is the first line of the file (before imports)
- Module name must match the file path (enforced by compiler)
- One module per file, strictly — no exceptions
- If omitted, the compiler infers the module name from the file path

### Import Everything (Unqualified)

```neohaskell
import Data.List
import Text.Printf
```

Imports all exported names from the module into the current scope, unqualified.
This is the simplest form — use it for small modules or when name clashes are unlikely.

### Selective Imports

```neohaskell
import Data.Map { Map, fromList, toList }
import Data.Maybe { Maybe(..) }
import Html { div, text, Html }
```

Braces enclose the specific names to import. Commas separate items (trailing comma allowed).
Consistent with NeoHaskell's "braces everywhere" principle and familiar to TypeScript developers.

**Importing type constructors:**

```neohaskell
-- Import type + all constructors
import Data.Maybe { Maybe(..) }

-- Import type + specific constructors
import Data.Maybe { Maybe(Just, Nothing) }

-- Import type only (no constructors)
import Data.Maybe { Maybe }
```

The `(..)` syntax means "all constructors of this type." Specific constructors
can be listed in parentheses after the type name.

### Aliased Imports (Qualified)

```neohaskell
import Data.Map as Map
import Data.Text as T
import Collections.Internal.Helpers as Helpers
```

The `as` keyword creates a qualified alias. When using `as`:
- All names are accessed via the alias: `Map.lookup`, `Map.fromList`
- No unqualified access — `lookup` alone is a compile error
- No `qualified` keyword needed — `as` implies qualified-only access

### Combined: Selective + Alias

```neohaskell
import Data.Map { Map } as Map
import Data.Text { Text, pack } as T
```

The most common pattern in real code. Brings specific names into scope unqualified
(typically the type) while making the full module available via the alias (for functions).

```neohaskell
-- After: import Data.Map { Map } as Map
fun buildIndex(items: Array<Text>) : Map<Text, Int> {
    let pairs = Array.mapWithIndex(items, \item, i -> #(item, i))
    Map.fromList(pairs)
}
```

Here `Map` (the type) is used unqualified in the signature, while `Map.fromList`
uses the qualified alias for the function.

### Re-exports

The `export import` modifier re-exports names from another module through the current module.
This enables facade modules that present a clean public API.

```neohaskell
module Collections

-- Re-export everything from submodules
export import Collections.Map
export import Collections.Set

-- Re-export specific items
export import Collections.Internal { fromList, toList }

-- Re-export with alias
export import Collections.Map as Map
```

Consumers can then write `import Collections` to get everything, or
`import Collections { Map }` for specific items.

**Note:** Full export control (which names a module exposes by default) is
designed in Feature #13 (Visibility). `export import` covers the re-export case only.

### Prelude

NeoHaskell has an implicit prelude import. Common types and functions are available
without explicit import:

```neohaskell
-- These are always in scope (no import needed):
-- Types: Int, Float, Text, Bool, Maybe, Result, Array, Task
-- Functions: map, filter, print, show, not, fst, snd, identity
-- Operators: +, -, *, /, ==, /=, <, >, <=, >=, ++, |>
```

The prelude is designed to avoid name clashes with common module names.
If a prelude name conflicts with a local definition, the local definition wins.

### Formatting Rules

The formatter (`neo fmt`) enforces:

1. Module declaration first (line 1)
2. Blank line after module declaration
3. Imports sorted alphabetically by module path
4. `export import` lines grouped before regular `import` lines
5. One import per line (no multi-line imports — use multiple lines instead)
6. Blank line between imports and code

```neohaskell
module MyApp.Users

export import MyApp.Users.Types { User, UserId }

import Data.Map { Map } as Map
import Data.Text { Text }
import MyApp.Database as DB
import MyApp.Users.Types { User, UserId }

fun findUser(id: UserId) : Task<Maybe<User>> {
    let! conn = DB.connect()
    let! result = DB.query(conn, "SELECT * FROM users WHERE id = ?", id)
    return result
}
```

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-|-|
| `import Data.Map` | `import Data.Map` |
| `import Data.Map { Map, fromList }` | `import Data.Map (Map, fromList)` |
| `import Data.Map as Map` | `import qualified Data.Map as Map` |
| `import Data.Map { Map } as Map` | `import Data.Map (Map)` |
| | `import qualified Data.Map as Map` |
| `import Data.Maybe { Maybe(..) }` | `import Data.Maybe (Maybe(..))` |
| `import Data.Maybe { Maybe(Just) }` | `import Data.Maybe (Maybe(Just))` |
| `export import Foo` | Adds `module Foo` to module export list |
| `export import Foo { bar }` | Adds `bar` to module export list + `import Foo (bar)` |
| `module Data.Map` | `module Data.Map where` (or with export list from #13) |

**Combined import transpilation:** `import Data.Map { Map } as Map` generates
two Haskell import lines — one for the unqualified selective import and one for
the qualified alias. This is the standard Haskell idiom for "type unqualified,
functions qualified."

### Error Messages

**Error: unqualified access with alias import:**

```
error: `lookup` is not in scope
  --> src/Main.nh:5:12
  |
5 |     let! v = lookup(key, m)
  |              ^^^^^^ not imported unqualified
  |
  = hint: you imported Data.Map as Map — use `Map.lookup` instead
  = hint: or add `lookup` to the selective import: `import Data.Map { lookup } as Map`
```

**Error: module name doesn't match file path:**

```
error: module name does not match file path
  --> src/Data/Map.nh:1:1
  |
1 | module Data.HashMap
  |        ^^^^^^^^^^^^ expected `Data.Map` based on file path
  |
  = hint: rename the file to `src/Data/HashMap.nh` or change the module name to `Data.Map`
```

**Error: ambiguous name (no hiding, use alias instead):**

```
error: ambiguous name `map`
  --> src/Main.nh:8:5
  |
8 |     map(f, xs)
  |     ^^^ defined in both `Data.Map` and `Prelude`
  |
  = hint: use a qualified import: `import Data.Map as Map` then `Map.map(f, xs)`
  = hint: or use a selective import: `import Data.Map { map }`
```

### What Imports Do NOT Include

These are designed in later features or explicitly excluded:

- `hiding` syntax — omitted (use selective imports or aliases to resolve clashes)
- Wildcard selective imports (`import Foo { * }`) — `import Foo` already imports everything
- Relative imports (`import ../Foo`) — all paths are absolute from project root
- String-based paths (`import Foo from "package"`) — dot-paths are identifiers, not strings
- Full export control (which names a module exposes) → Feature #13 (Visibility)
- Package manager / dependency resolution → out of scope
- Circular import resolution → handled by Haskell backend
- Conditional imports / platform-specific imports → not planned

### Design Rationale

**Why `import`?** Zero strangeness budget cost. Universal across Java, C#, TypeScript,
Kotlin, Python, Go, Elm, and Haskell. `use` (Rust) carries Rust-specific mental model
baggage and is unfamiliar to the target audience. (8/8 unanimous)

**Why braces for selective imports?** NeoHaskell uses braces everywhere — records,
blocks, domain keywords. Using parens for imports would be the only place parens delimit
a list of names, creating an inconsistency. TypeScript developers write `import { useState }
from "react"` daily — braces for import selection is already in their muscle memory.
(8/8 unanimous — Klabnik: "free win", Bernhardt: "parens would be a wat moment")

**Why `as` without `qualified`?** `qualified` is Haskell jargon that means nothing to
Java/C#/JS developers. Every other language uses `as` alone: Kotlin (`import x as Y`),
TypeScript (`import * as M`), Python (`import x as y`), Elm (`import X as Y`).
The behavior is obvious from context — if you alias a module, you use the alias.
(8/8 unanimous — Nystrom: "`qualified` adds syllables without adding meaning")

**Why no `hiding`?** `hiding` creates negative knowledge — the reader must know what's
in the module, mentally subtract the hidden names, and reason about what's left.
This is the opposite of explicit. Name clashes are solved by selective imports
(`import Foo { bar }`) or aliases (`import Foo as F`). Haskell's `hiding` exists
primarily to work around Prelude conflicts — NeoHaskell solves this at the prelude
design level instead. (6/8 — Borretti and Breslav favored including it; Elm tie-break
applied. Nystrom: "every `hiding` I've seen in Haskell is a smell")

**Why one module per file?** Go, Elm, Rust, and modern TypeScript all enforce or
strongly encourage this. Benefits: the filesystem IS the module system (no mental model
gap), tooling is trivially predictable, new contributors never ask "where is this module
defined?", and error messages can reference file paths directly. Haskell's flexible
module placement is a known pain point. (8/8 unanimous)

**Why dot-paths?** Module paths are logical identifiers, not filesystem strings.
Kotlin (`import kotlin.collections.Map`), Java (`import java.util.Map`), and Haskell
(`import Data.Map`) all use dot-separated paths. String paths (`import Map from "data"`)
carry filesystem semantics — relative vs absolute, resolution algorithms, build tool
configuration. Dot-paths are unambiguous in a module=file system. (8/8 unanimous)

**Why `export import` for re-exports?** Re-exports should be syntactically loud, not
silent (Nystrom). `export import` reads naturally as "export this import" and mirrors
TypeScript's `export { } from` pattern conceptually. It composes with the existing
import syntax — anything you can `import`, you can `export import`. Full export
control is deferred to Feature #13. (8/8 consensus)

---

## 7. Comments ❌

_To be designed._

---

## 8. String Interpolation ❌

_To be designed._

---

## 9. Type Aliases ✅

Designed via DX Council review (5 experts, Feb 2025).

### Summary

| Decision | Choice | Council Support |
|----------|--------|-----------------|
| Definition keyword | `alias` | 5/5 — unanimous |
| Transparency | Fully transparent (interchangeable with original) | 5/5 — unanimous |
| Generic parameters | Lowercase `<a>`, `<ok, err>` | Locked (consistent with generics) |
| Distinction from newtypes | Aliases are transparent; newtypes are opaque wrappers | 5/5 — unanimous |

### Simple Type Aliases

```neohaskell
alias UserId = Int
alias Email = Text
alias Timestamp = Int
```

Fully transparent — `UserId` and `Int` are interchangeable at compile time.
No runtime distinction. Useful for domain modeling and documentation.

### Generic Type Aliases

```neohaskell
alias Pair<a, b> = #(a, b)
alias Dict<k, v> = Array<#(k, v)>
alias Callback<a> = a -> Task<Unit>

-- Multi-line form (optional)
alias Result<ok, err> =
  | Ok(ok)
  | Err(err)
```

Generic parameters are lowercase (required for Haskell transpilation).
Aliases can be parameterized just like records and types.

### Transparency: Alias vs. Newtype

**Type aliases are fully transparent:**

```neohaskell
alias UserId = Int

fun getUserId(user: User) : UserId = user.id  -- Returns Int, type-checked as UserId
fun add(a: UserId, b: UserId) : UserId = a + b  -- Int + Int works directly
```

**Newtypes are opaque wrappers** (Feature #10, designed later):

```neohaskell
newtype UserId = UserId(Int)  -- Opaque: UserId ≠ Int at compile time

fun getUserId(user: User) : UserId = UserId(user.id)  -- Must wrap
fun add(a: UserId, b: UserId) : UserId = ???  -- Cannot add directly; must unwrap
```

Aliases are for documentation and domain clarity. Newtypes are for type safety.

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-----------|----------------|
| `alias UserId = Int` | `type UserId = Int` |
| `alias Pair<a, b> = #(a, b)` | `type Pair a b = (a, b)` |
| `alias Callback<a> = a -> Task<Unit>` | `type Callback a = a -> Task ()` |

### What Type Aliases Do NOT Include

These are designed in later features:

- Opaque wrappers → Feature #10 (Newtypes)
- Type constraints → Feature #12 (Traits & Impl)
- Associated types → Not planned
- Phantom types → Not planned

### Design Rationale

**Why `alias` and not `typealias`?** The keyword `type` is already used for Union Types (Feature #2).
Using `alias` creates clear visual separation: `type` for ADTs, `alias` for synonyms.
`alias` also minimizes strangeness budget (Klabnik) — it's shorter and simpler than `typealias`.
Target audience (Java/C#/JS developers) is familiar with type aliases from TypeScript and Kotlin,
so the keyword choice is less critical than clarity.

**Why fully transparent?** Haskell's `type` is transparent; Elm's `type alias` is transparent.
Transparency is the standard for type aliases across languages. Opaque wrappers (newtypes) are
a separate feature for when you need type safety. This separation is clearer than Haskell's
distinction between `type` (transparent) and `newtype` (opaque).

**Why lowercase generics?** Consistency with all other generic features in NeoHaskell.
Haskell's type system uses case to distinguish type variables (`a`) from type constructors (`Int`).
The transpiler cannot reliably convert `<T>` → `t` because uppercase identifiers in Haskell are
constructors, not variables. Lowercase is non-negotiable.

**Why no field accessors?** Type aliases are transparent — they don't create new types.
Field accessors would only make sense for newtypes (Feature #10).

---

## 10. Newtypes ✅

Designed via DX Council review (8 experts, Feb 2026).

### Summary

| Decision | Choice | Council Support |
|-|-|-|
| Definition keyword | `newtype` | 8/8 — unanimous |
| Syntax | `newtype Name(Type)` — tuple-style, no repeated name | 8/8 — unanimous |
| Unwrapping | Pattern matching only | 8/8 — unanimous |
| Auto-derived traits | `Show`, `Eq` auto-derived (same as union types) | 8/8 — unanimous (locked) |
| Additional deriving | `deriving Trait` delegates to wrapped type | 7/8 |
| Zero-cost guarantee | Yes — transpiles to Haskell `newtype` | 8/8 — unanimous |
| Field accessors | None — unwrap via pattern matching | 8/8 — unanimous |

### Basic Newtypes

```neohaskell
newtype Dollars(Float)

newtype CustomerId(Int)

newtype Email(Text)

newtype Name(Text)
```

Zero-cost type-safe wrappers. Each `newtype` creates a distinct type that is represented
identically to the wrapped type at runtime. `Dollars` and `Float` have different types
but zero runtime overhead.

### Generic Newtypes

```neohaskell
newtype Id<a>(a)

newtype NonEmpty<a>(Array<a>)

newtype Validated<a>(a)
```

Type parameters are lowercase (locked convention). Use semantic names
(`item`, `key`, `value`) over single letters where meaningful.

### Construction

```neohaskell
-- Direct construction
let price = Dollars(9.99)
let userId = CustomerId(42)
let email = Email("user@example.com")

-- Generic newtypes (type inferred)
let orderId = Id(1001)           -- Id<Int>
let items = NonEmpty([1, 2, 3])   -- NonEmpty<Array<Int>>

-- Explicit type application when needed
let emptyId = Id:<Text>("")
```

Construction syntax mirrors ADT positional syntax: `Name(value)`. The type
name IS the constructor — no separate constructor name like Haskell.

### Unwrapping (Pattern Matching)

```neohaskell
-- Via let binding
let Dollars(amount) = price         -- amount : Float
let CustomerId(raw) = userId        -- raw : Int
let Email(address) = email          -- address : Text

-- In function bodies
fun showPrice(price: Dollars) : Text {
    let Dollars(amount) = price
    "$" ++ show(amount)
}

fun addDollars(a: Dollars, b: Dollars) : Dollars {
    let Dollars(x) = a
    let Dollars(y) = b
    Dollars(x + y)
}

-- Generic unwrapping
fun unwrapId<a>(id: Id<a>) : a {
    let Id(value) = id
    value
}
```

Pattern matching is the **only** mechanism for unwrapping newtypes.
No `.value` accessor — tuple-style newtypes have no named field.
This is consistent with ADT positional variant destructuring (Feature #3/11).

### Deriving

`Show` and `Eq` are **auto-derived** for all newtypes. Additional derivations use
explicit `deriving`, which **delegates to the wrapped type's implementation**:

```neohaskell
-- Show and Eq are automatic — no annotation needed
newtype Dollars(Float)

-- Delegate Num and Ord to Float's implementations
newtype Dollars(Float)
  deriving Num, Ord

-- With Num derived, arithmetic just works:
-- Dollars(1.50) + Dollars(2.50)  →  Dollars(4.00)

-- Delegate Ord to Text's Ord
newtype Name(Text)
  deriving Ord

-- Multiple derivations
newtype Score(Int)
  deriving Num, Ord, Bounded
```

The `deriving` clause on newtypes differs from union types: it **delegates** to the
wrapped type's trait implementation rather than structurally deriving. This maps directly
to Haskell's `GeneralizedNewtypeDeriving` extension.

### Transpilation Rules

| NeoHaskell | Haskell Output |
|-|-|
| `newtype Dollars(Float)` | `newtype Dollars = Dollars Float` |
| | `  deriving (Show, Eq)` |
| `newtype Id<a>(a)` | `newtype Id a = Id a` |
| | `  deriving (Show, Eq)` |
| `newtype Dollars(Float) deriving Num, Ord` | `{-# LANGUAGE GeneralizedNewtypeDeriving #-}` |
| | `newtype Dollars = Dollars Float` |
| | `  deriving (Show, Eq, Num, Ord)` |
| `Dollars(9.99)` | `Dollars 9.99` |
| `let Dollars(x) = price` | `let (Dollars x) = price` |

The transpiler automatically:
- Adds `= TypeName` as the constructor name (type name reused)
- Converts paren syntax to Haskell's space-separated form
- Always includes `Show, Eq` in the `deriving` clause
- Emits `GeneralizedNewtypeDeriving` pragma when additional traits are derived

### `newtype` vs `record` vs `type`

| Construct | Use Case | Zero-Cost | Field Accessors | Haskell Output |
|-|-|-|-|-|
| `newtype Dollars(Float)` | Type safety wrapper | Yes | No | `newtype Dollars = Dollars Float` |
| `record Amount { value: Float }` | Structured data (one field) | No | Yes (`.value`) | `data Amount = Amount { value :: Float }` |
| `type Currency = Dollar \| Euro` | Sum type / enum | N/A | No | `data Currency = Dollar \| Euro` |
| `alias Price = Float` | Transparent synonym | N/A | N/A | `type Price = Float` |

**When to use `newtype`:**
- Wrapping a primitive for type safety (`CustomerId`, `Email`, `Dollars`)
- Zero-cost is important (no boxing overhead at runtime)
- Single wrapped value, no named field needed
- Want to derive traits from the wrapped type

**When to use `record`:**
- Single-constructor type with named fields
- Field accessors needed (`.fieldName`)
- Domain model entity, even with one field
- Documentation value of named fields outweighs brevity

**When to use `alias`:**
- No type safety needed — just a shorter name
- Fully transparent — interchangeable with original type

### What Newtypes Do NOT Include

These are designed in later features:

- Pattern matching in function arguments → Feature #3
- Full destructuring bindings → Feature #11
- Trait implementations for newtypes → Feature #12
- Deriving mechanism internals → Feature #12
- `coerce` / safe coercion between newtype and wrapped type → Not planned (post-1.0)
- Newtype deriving for multi-parameter typeclasses → Not planned
- Smart constructors (validation on construction) → Via module visibility (Feature #13)
- Named-field newtypes → Use `record` instead

### Error Messages

```
error: `newtype` must wrap exactly one type
  --> src/Domain.nh:5:1
  |
5 | newtype Point(Float, Float)
  |              ^^^^^^^^^^^^^ expected single type, found 2
  |
  = hint: use `record Point { x: Float, y: Float }` for multiple fields
  = hint: or use `type Point = Point(Float, Float)` for an ADT wrapper
```

```
error: `newtype` cannot have variants
  --> src/Domain.nh:3:1
  |
3 | newtype Result = Ok(Int) | Err(Text)
  |                         ^ variants not allowed in newtype
  |
  = hint: use `type Result = Ok(Int) | Err(Text)` for a union type
```

```
error: cannot access fields on a newtype
  --> src/Main.nh:8:20
  |
8 |     let x = price.value
  |                   ^^^^^ newtypes have no field accessors
  |
  = hint: unwrap via pattern matching: `let Dollars(x) = price`
```

```
error: `newtype` cannot use brace syntax
  --> src/Domain.nh:3:1
  |
3 | newtype Dollars { amount: Float }
  |                 ^^^^^^^^^^^^^^^^ use parentheses, not braces
  |
  = hint: use `newtype Dollars(Float)` for a zero-cost wrapper
  = hint: use `record Dollars { amount: Float }` if you need field access
```

### Design Rationale

**Why `newtype`?** It's the established term in the Haskell ecosystem (the transpilation target).
Klabnik: low strangeness budget — Haskell/Rust developers recognize it, and others can infer
"new type wrapper." matklad: greppable (`newtype Name` finds definitions), parser-friendly
(commit at token 1, same as `fun`, `type`, `record`). Bernhardt: consistent naming pattern —
`type` (sum), `record` (product), `newtype` (wrapper), `alias` (synonym). (8/8 unanimous)

**Why `newtype Dollars(Float)` not `newtype Dollars = Dollars(Float)`?** The repeated name is
Haskell ceremony. "Why do I name it twice?" — every beginner asks this. In NeoHaskell, the type
name IS the constructor, same as `record`. The transpiler adds the constructor name automatically.
Borretti: less ceremony → more newtypes → more type safety. Breslav: similar to Kotlin's
`value class Dollars(val amount: Double)` but even shorter. (8/8 unanimous)

**Why pattern matching only, no `.value`?** Tuple-style `newtype Dollars(Float)` has no named
field, so `.value` would be an ad-hoc invention inconsistent with the syntax. Pattern matching
is consistent with ADT destructuring (Feature #3). One mechanism for all unwrapping.
Czaplicki: one way to do it, fewer concepts. (8/8 unanimous)

**Why auto-derive Show/Eq?** Locked convention — all NeoHaskell types auto-derive Show and Eq.
(8/8 unanimous, non-negotiable)

**Why `deriving` delegates to wrapped type?** This is the killer feature of newtypes. Without
delegation, `newtype Dollars(Float)` can't participate in arithmetic without manual `impl`
blocks — making newtypes burdensome for numeric wrappers. The transpiler emits
`GeneralizedNewtypeDeriving` when needed. Syme: pragmatic, avoids over-abstraction.
Czaplicki: conditional acceptance (would prefer no typeclasses at all, but accepts explicit
`deriving` as the least-bad option). (7/8)

**Why zero-cost guarantee?** Without zero-cost, `newtype` is just `type Name = Name(T)` with a
different keyword. The guarantee is the defining semantic difference — it's why the feature
exists. Haskell's `newtype` is erased at compile time; the transpiler preserves this.
(8/8 unanimous)

---

## 11. Destructuring ❌

_To be designed._

---

## 12. Traits & Impl ❌

_To be designed._

---

## 13. Visibility ❌

_To be designed._

---

## 14. Guards ❌

_To be designed._

---

## 15. Doc Comments ❌

_To be designed._

---

_Last updated: 2026-02-25_
