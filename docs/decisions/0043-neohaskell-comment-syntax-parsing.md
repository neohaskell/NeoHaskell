# ADR-0043: NeoHaskell Comment Syntax Parsing

## Status

Proposed

## Context

NeoHaskell's syntax specification (§7, finalised March 2025) defines three comment syntaxes:

1. **Line comments** — `// content` (extend to end of line)
2. **Block comments** — `/* content */` (nest correctly: `/* outer /* inner */ outer */`)
3. **Doc comments** — `/** content */` (attach to the declaration immediately following them; target for documentation tooling and IDE hover)

The transpiler must convert these to their Haskell equivalents before passing output to GHC. This requires a module that **captures** comment content and source position — the transpiler cannot operate on comments that are silently discarded.

### Current State

`nhcore` already provides comment primitives in the `Parser` module (introduced in ADR-0042):

| Function                    | Signature              | Behaviour                              |
| --------------------------- | ---------------------- | -------------------------------------- |
| `Parser.lineComment`        | `Text -> Parser Unit`  | **Skips** from prefix to end of line   |
| `Parser.blockComment`       | `Text -> Text -> Parser Unit` | **Skips** non-nested block content |
| `Parser.blockCommentNested` | `Text -> Text -> Parser Unit` | **Skips** nested block content    |

All three return `Parser Unit` — they consume and discard comment text. This is intentional for
the general-purpose whitespace-skipping use case (see ADR-0042 §6 "Custom Whitespace Mode"). It is
**not usable for transpilation**, where the content must be preserved to produce valid Haskell
output.

No module in nhcore currently:
- captures comment text instead of skipping it
- distinguishes line, block, and doc comment variants
- tracks nesting depth in block comments
- provides a `toHaskell` transpilation function

### Use Cases

- **Transpiler comment conversion** (issue #471): convert `// comment` → `-- comment`,
  `/* block */` → `{- block -}`, `/** doc */` → `-- | Haddock doc comment`
- **IDE hover / documentation extraction**: extract doc comment content for LSP hover
  information and auto-generated API docs
- **Formatter preservation**: a NeoHaskell auto-formatter must retain comment text and
  position while restructuring surrounding code
- **Static analysis / linting**: a linter checking comment quality or TODO tracking
  needs to read and classify comment content

### Design Goals

1. **Capture, not skip**: all parsers in this module return `Parser Comment`, never `Parser Unit`
2. **Build on `Parser.*`**: extend combinators from ADR-0042 — do not reach into raw megaparsec
3. **Live in nhcore**: the transpiler, LSP proxy, and formatter can share one implementation
   without a separate package
4. **Position-aware**: every `Comment` carries a `ParsePosition` for source mapping and
   precise error reporting
5. **Transpilation explicit in the type**: `toHaskell` is the primary exit point — not
   a utility buried in a transpiler module

### GitHub Issue

- [#471: NeoHaskell comment syntax parsing](https://github.com/neohaskell/NeoHaskell/issues/471)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — this ADR
  unblocks comment handling for the transpiler

## Decision

### 1. Module Naming: `Syntax.Comment`

This is the **first** syntax-level module in nhcore. The chosen name must scale gracefully as
the transpiler grows to cover string interpolation, function declarations, pattern matching, and
other syntactic constructs.

| Candidate           | Verdict        | Rationale                                                                                                                                                                                                                                 |
| ------------------- | -------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `Parser.Comment`    | Rejected       | The `Parser.*` namespace is reserved for general-purpose parsing combinators (ADR-0042). Language-specific syntax nodes — which carry an AST structure plus transpilation logic — must not live under the general parsing library. Conflates parsing machinery with language representation. |
| `Language.Comment`  | Rejected       | `Language.*` is too broad and risks confusion with GHC's `Language.Haskell.*` hierarchy. Also implies a multi-language abstraction; these modules are NeoHaskell-specific.                                                               |
| `Transpiler.Comment`| Rejected       | Implies this belongs in a separate transpiler package. The decision is to house syntax modules in nhcore so tooling (LSP, formatter, linter) can consume them without depending on the transpiler build.                                  |
| `NhSyntax.Comment`  | Rejected       | Prefix disambiguation is unnecessary — the `syntax` source directory in nhcore already scopes these modules unambiguously within the package.                                                                                            |
| `Syntax.Comment`    | **Chosen**     | "Syntax" precisely names the domain: syntactic constructs bundled with their AST nodes, parsers, and transpilers. The family scales naturally: `Syntax.Comment`, `Syntax.StringInterpolation`, `Syntax.Function`, `Syntax.PatternMatch`. A new `syntax` source directory follows the nhcore multi-source-dir pattern established in ADR-0042 §1. |

**Directory structure:**

```text
core/
└── syntax/
    └── Syntax/
        └── Comment.hs          -- sole module (exposed)

core/test-core/
└── Syntax/
    └── CommentSpec.hs          -- auto-discovered by hspec-discover
```

**Cabal additions** (library stanza in `core/nhcore.cabal`):

```text
-- hs-source-dirs (add alongside parser, neoql, etc.)
syntax

-- exposed-modules (add alphabetically near "S")
Syntax.Comment
```

**Cabal additions** (nhcore-test-core stanza, `other-modules`):

```text
Syntax.CommentSpec
```

### 2. Type Definitions

All types follow NeoHaskell conventions: descriptive field names, strict primitive fields
(project-wide `Strict` extension), `Text` not `String`. `ParsePosition` from `Parser`
(ADR-0042 §3) is **reused** — it is already the project-standard source-location type
and covers the `sourceName`, `line`, `column`, and `offset` fields needed here.

#### Comment Type

```haskell
-- | A NeoHaskell source comment, captured with its content and source position.
--
-- The three constructors correspond to the three comment syntaxes defined in
-- the NeoHaskell specification (§7):
--
-- * 'LineComment' for @\/\/ content@ (single-line)
-- * 'BlockComment' for @\/* content *\/@ (block, correct nesting supported)
-- * 'DocComment' for @\/** content *\/@ (documentation, Haddock target)
--
-- Obtain a 'Comment' by running one of the parsers in this module:
--
-- @
-- import Syntax.Comment (Comment)
-- import Syntax.Comment qualified
-- import Parser qualified
--
-- result :: Result ParseError Comment
-- result = "\/\/ hello world" |> Parser.run Syntax.Comment.lineComment
-- @
data Comment
  = LineComment
      { position :: ParsePosition
        -- ^ Start position of the @\/\/@ token (1-based line and column).
      , content  :: Text
        -- ^ Text from after @\/\/ @ to end of line (one leading space stripped if present).
      }
  | BlockComment
      { position :: ParsePosition
        -- ^ Start position of the opening @\/*@ token.
      , content  :: Text
        -- ^ Raw inner text, including any nested @\/* ... *\/@ markers verbatim.
        -- 'toHaskell' substitutes these to produce valid Haskell @{- ... -}@ nesting.
      , depth    :: {-# UNPACK #-} Int
        -- ^ Maximum nesting depth observed during parsing.
        -- @1@ = flat block comment (no inner @\/*@).
        -- @2+@ = contains nested block comments.
        -- Tracked by a counter incremented on each @\/*@ and decremented on each @*\/@ seen
        -- while consuming the content.
      }
  | DocComment
      { position :: ParsePosition
        -- ^ Start position of the opening @\/**@ token.
      , content  :: Text
        -- ^ Inner text. Leading @* @ decoration is stripped from each line.
        -- The opening @**@ is not included.
      }
  deriving (Eq, Show, Generic)
```

#### Why Reuse `ParsePosition` (not a new `SourcePosition`)

`ParsePosition` from `Parser` carries `sourceName`, `line`, `column`, and `offset` — exactly the
fields needed for source mapping and error messages. Reusing it avoids a second position record
with identical semantics, keeps coordinate types consistent across the codebase, and allows
tools that already work with `ParseError.position` to use the same type for `Comment.position`
without conversion.

#### Required Extension to `Parser` Public API

`Syntax.Comment` parsers must capture position at the start of each comment. This requires
`Parser.position :: Parser ParsePosition`, which is not currently in the `Parser` public API
(ADR-0042). The implementation of `Syntax.Comment` adds this one function to `Parser.hs` as a
thin wrapper over `GhcMegaparsec.getSourcePos`, following the same delegation pattern used by
every other `Parser.*` function:

```haskell
-- | Return the current source position without consuming any input.
--
-- Use this to capture the start position of a construct before parsing it:
--
-- @
-- myParser :: Parser MyNode
-- myParser = do
--   pos <- Parser.position
--   content <- ...
--   Parser.yield (MyNode { position = pos, ... })
-- @
position :: Parser ParsePosition
position = Internal.wrap do
  sourcePos <- GhcMegaparsec.getSourcePos
  let pos = ParsePosition
        { sourceName = sourcePos |> GhcMegaparsec.sourceName |> Text.fromLinkedList
        , line       = sourcePos |> GhcMegaparsec.sourceLine |> GhcMegaparsec.unPos
        , column     = sourcePos |> GhcMegaparsec.sourceColumn |> GhcMegaparsec.unPos
        , offset     = 0   -- offset populated separately via getOffset if needed
        }
  Applicable.pure pos
{-# INLINE position #-}
```

This addition to `Parser.hs` is tracked in the same issue (#471) and requires adding `position`
to the `Parser` module export list.

### 3. Public API

```haskell
module Syntax.Comment (
  -- * Types
  Comment (..),
  -- * Parsers
  lineComment,
  blockComment,
  docComment,
  comment,
  -- * Transpilation
  toHaskell,
) where
```

#### Parser Functions

| Function      | Signature        | Input pattern         | Result constructor |
| ------------- | ---------------- | --------------------- | ------------------ |
| `lineComment` | `Parser Comment` | `// content`          | `LineComment`      |
| `blockComment`| `Parser Comment` | `/* content */`       | `BlockComment`     |
| `docComment`  | `Parser Comment` | `/** content */`      | `DocComment`       |
| `comment`     | `Parser Comment` | Any of the three above| (dispatched)       |

**Critical ordering constraint**: `comment` must try `docComment` before `blockComment`.
The `/**` prefix begins with `/*`, so if `blockComment` is tried first, a doc comment is
silently parsed as an ordinary block comment. The `choice` array fixes the order explicitly:

```haskell
-- | Parse any NeoHaskell comment (doc, block, or line — in that order).
--
-- 'docComment' must be tried before 'blockComment' because @\/**@ starts with @\/*@.
-- Both are wrapped in 'Parser.backtrack' so a failed attempt does not consume input.
--
-- @
-- import Syntax.Comment qualified
-- import Parser qualified
--
-- parseComments :: Parser (Array Comment)
-- parseComments =
--   Syntax.Comment.comment |> Parser.zeroOrMore
-- @
comment :: Parser Comment
comment =
  Parser.choice
    [ docComment
    , blockComment
    , lineComment
    ]
```

Each element in `Parser.choice` is automatically wrapped in `Parser.backtrack` (see ADR-0042
§4 "Labels, Failure, Branching"), so this is safe.

#### `lineComment` Parser

```haskell
-- | Parse a NeoHaskell line comment (@\/\/ ...@) and capture its content.
--
-- Consumes from @\/\/@ to (but not including) the newline character.
-- One leading space after @\/\/@ is stripped from the captured content.
--
-- __Note__: This CAPTURES comment text. To skip a line comment without capturing,
-- use 'Parser.lineComment "//"' from the general-purpose 'Parser' module.
--
-- @
-- "\/\/ hello world\n" |> Parser.run Syntax.Comment.lineComment
-- -- Ok (LineComment { content = "hello world", ... })
-- @
lineComment :: Parser Comment
lineComment = do
  pos <- Parser.position
  _ <- Parser.text "//"
  _ <- Parser.optional (Parser.char ' ')
  chars <- Parser.anyCharExcept '\n' |> Parser.zeroOrMore
  let capturedContent = chars |> Text.fromArray
  Parser.yield (LineComment { position = pos, content = capturedContent })
{-# INLINE lineComment #-}
```

#### `blockComment` Parser

```haskell
-- | Parse a NeoHaskell block comment (@\/* ... *\/@) and capture its content.
--
-- Supports nesting: @\/* outer \/* inner *\/ outer *\/@ is valid and the
-- inner @\/* *\/@ markers are preserved verbatim in 'BlockComment.content'.
-- The 'BlockComment.depth' field records the maximum nesting depth seen.
--
-- __Note__: This CAPTURES comment text. To skip a block comment, use
-- 'Parser.blockCommentNested "\/*" "*\/"' from the 'Parser' module.
blockComment :: Parser Comment
blockComment = do
  pos <- Parser.position
  _ <- Parser.text "/*"
  let go currentDepth accumulator =
        Parser.choice
          [ do
              _ <- Parser.text "*/"
              if currentDepth == 1
                then Parser.yield (accumulator, 1)
                else do
                  let newContent = accumulator |> Text.append "*/"
                  go (currentDepth - 1) newContent
          , do
              _ <- Parser.text "/*"
              let newContent = accumulator |> Text.append "/*"
              go (currentDepth + 1) newContent
          , do
              ch <- Parser.anyChar
              let newContent = accumulator |> Text.snoc ch
              go currentDepth newContent
          ]
  (capturedContent, maxDepth) <- go 1 ""
  Parser.yield
    BlockComment
      { position = pos
      , content  = capturedContent
      , depth    = maxDepth
      }
{-# INLINE blockComment #-}
```

#### `docComment` Parser

```haskell
-- | Parse a NeoHaskell doc comment (@\/** ... *\/@) and capture its content.
--
-- Doc comments attach to the declaration immediately following them and are
-- extracted by documentation tooling and IDE hover providers.
-- Leading @* @ decoration on each interior line is stripped from the content.
--
-- @
-- "\/\*\* Calculates distance. *\/" |> Parser.run Syntax.Comment.docComment
-- -- Ok (DocComment { content = "Calculates distance.", ... })
-- @
docComment :: Parser Comment
docComment = do
  pos <- Parser.position
  _ <- Parser.text "/**"
  chars <- Parser.anyChar |> Parser.collectUntil (Parser.text "*/")
  let rawContent = chars |> Text.fromArray
  let capturedContent =
        rawContent
          |> Text.lines
          |> Array.map (Text.dropPrefix "* " >> Text.strip)
          |> Text.joinWith "\n"
          |> Text.strip
  Parser.yield (DocComment { position = pos, content = capturedContent })
{-# INLINE docComment #-}
```

#### `toHaskell` Transpilation Function

```haskell
-- | Convert a NeoHaskell 'Comment' to its Haskell equivalent.
--
-- Transpilation rules (NeoHaskell syntax spec §7):
--
-- * 'LineComment' @\/\/ content@ → @-- content@
-- * 'BlockComment' @\/* content *\/@ → @{- content -}@  (nesting preserved)
-- * 'DocComment' @\/** content *\/@ → @-- | content@  (Haddock format)
--
-- __Nesting__: nested @\/* ... *\/@ markers inside 'BlockComment.content' are
-- replaced with @{- ... -}@ to produce valid GHC nested block comments
-- (@{- outer {- inner -} outer -}@).
--
-- __Multi-line doc comments__: each content line becomes a separate @-- |@ line.
--
-- @
-- toHaskell (LineComment { content = "hello", ... })
--   == "-- hello"
--
-- toHaskell (BlockComment { content = " world ", depth = 1, ... })
--   == "{- world -}"
--
-- toHaskell (BlockComment { content = " outer \/* inner *\/ outer ", depth = 2, ... })
--   == "{- outer {- inner -} outer -}"
--
-- toHaskell (DocComment { content = "Calculates distance.", ... })
--   == "-- | Calculates distance."
-- @
toHaskell :: Comment -> Text
toHaskell commentNode =
  case commentNode of
    LineComment { content = capturedContent } ->
      [fmt|-- #{capturedContent}|]
    BlockComment { content = capturedContent } ->
      let innerHaskell =
            capturedContent
              |> Text.replace "/*" "{-"
              |> Text.replace "*/" "-}"
      in [fmt|{-#{innerHaskell}-}|]
    DocComment { content = capturedContent } ->
      let contentLines = capturedContent |> Text.lines
      in
        case contentLines of
          [] ->
            "-- |"
          _ ->
            contentLines
              |> Array.map (\line -> [fmt|-- | #{line}|])
              |> Text.joinWith "\n"
```

### 4. Transpilation Rules

Complete mapping from NeoHaskell syntax spec §7:

| NeoHaskell source              | Haskell output                          | Notes                                      |
| ------------------------------ | --------------------------------------- | ------------------------------------------ |
| `// hello world`               | `-- hello world`                        | One leading space stripped after `//`      |
| `/* hello */`                  | `{- hello -}`                           | Flat block comment                         |
| `/* outer /* inner */ outer */`| `{- outer {- inner -} outer -}`         | GHC supports native `{- -}` nesting        |
| `/** Calculates distance. */`  | `-- | Calculates distance.`             | Haddock single-line doc comment            |
| Multi-line `/** ... */`        | Multiple `-- |` lines (one per line)    | Each content line becomes a Haddock line   |

**Multi-line doc comment example:**

```neohaskell
/**
 * Calculates the distance between two points.
 *
 * Returns the Euclidean distance as a Float.
 */
```

Transpiles to:

```haskell
-- | Calculates the distance between two points.
-- |
-- | Returns the Euclidean distance as a Float.
```

### 5. Usage Examples

#### In a Transpiler Pipeline

```haskell
module Transpiler.Pass.Comments where

import Array (Array)
import Array qualified
import Parser qualified
import Result (Result (..))
import Syntax.Comment (Comment (..))
import Syntax.Comment qualified
import Text (Text)
import Text qualified

-- | Collect all comments from a NeoHaskell source fragment.
--
-- Returns them in source order (left-to-right, top-to-bottom).
collectComments :: Text -> Result ParseError (Array Comment)
collectComments source = do
  let commentParser =
        Syntax.Comment.comment
          |> Parser.separatedOrTerminatedBy Parser.spaces
  source |> Parser.run commentParser

-- | Strip comments from source and return their Haskell equivalents separately.
--
-- Used by the transpiler to re-attach comments after reformatting declarations.
extractAndTranspile :: Array Comment -> Array Text
extractAndTranspile comments =
  comments |> Array.map Syntax.Comment.toHaskell
```

#### In Test Assertions

```haskell
module Syntax.CommentSpec where

import Syntax.Comment (Comment (..))
import Syntax.Comment qualified
import Parser qualified
import Test qualified

spec :: Test.Spec
spec = do
  Test.describe "Syntax.Comment" do

    Test.describe "lineComment" do
      Test.it "captures content after //" do
        let result = "// hello world\n" |> Parser.run Syntax.Comment.lineComment
        result |> shouldSatisfy Result.isOk

      Test.it "transpiles to Haskell -- comment" do
        let input = "// hello"
        let result = input |> Parser.runMaybe Syntax.Comment.lineComment
        case result of
          Nothing ->
            Test.fail [fmt|Expected parse success for: #{input}|]
          Just parsed ->
            Syntax.Comment.toHaskell parsed |> shouldBe "-- hello"

    Test.describe "docComment" do
      Test.it "is parsed before blockComment in choice" do
        let result = "/** doc */" |> Parser.run Syntax.Comment.comment
        case result of
          Err err ->
            Test.fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
          Ok (DocComment { content = c }) ->
            c |> shouldBe "doc"
          Ok other ->
            Test.fail [fmt|Expected DocComment, got: #{toText other}|]

    Test.describe "toHaskell" do
      Test.it "transpiles nested block comment" do
        let commentNode =
              BlockComment
                { position = def
                , content  = " outer /* inner */ outer "
                , depth    = 2
                }
        Syntax.Comment.toHaskell commentNode
          |> shouldBe "{- outer {- inner -} outer -}"
```

## Consequences

### Positive

1. **Unblocks transpiler (#471)**: the transpiler can parse and convert all three NeoHaskell
   comment syntaxes to Haskell using a single import (`Syntax.Comment`).

2. **Establishes the `Syntax.*` namespace**: `Syntax.Comment` is the first module in a family
   that will grow to cover string interpolation, function declarations, pattern matching, and
   other syntactic constructs. Each module follows the same pattern: one file per syntactic
   construct, containing the AST type + parser(s) + `toHaskell`.

3. **Reuses `Parser` library (ADR-0042)**: the implementation delegates to `Parser.*` combinators
   rather than raw megaparsec. No new package dependencies are introduced.

4. **Position-aware for tooling**: `ParsePosition` on every `Comment` enables IDE hover,
   source-map generation, formatter round-trips, and precise error messages pointing to the
   exact comment that caused a problem.

5. **Nesting depth tracked**: `BlockComment.depth` lets tools detect deeply nested comments
   without re-parsing content; also a useful invariant for property-based tests.

6. **`toHaskell` is a pure, isolated function**: transpilation logic is testable independently
   of the parser, and future changes to Haskell comment syntax only require updating one function.

7. **`Parser.position` added to public API**: the new `Parser.position :: Parser ParsePosition`
   function needed by `Syntax.Comment` is useful to all future syntax parsers — a permanent
   contribution to the general library.

### Negative

1. **Two parallel comment representations**: `Parser.lineComment` / `blockComment` /
   `blockCommentNested` (skip) and `Syntax.Comment.*` (capture) coexist. Developers must
   choose correctly. Mitigated by clear Haddock on both sides distinguishing "skip" from "capture".

2. **`toHaskell` uses `Text.replace` for nesting**: replacing `/*` → `{-` and `*/` → `-}` in
   block comment content is a text substitution. If the comment content itself contains the
   literal sequence `*/` as text (e.g., a comment about C pointer dereferencing), the substitution
   will incorrectly alter it. This is an edge case for typical NeoHaskell code.

3. **Multi-line doc comments produce one `-- |` per line**: this matches the Haddock convention
   but may look verbose for long doc blocks. The alternative block-Haddock format (`{- | ... -}`)
   is not used, trading compactness for uniformity with line-comment style.

4. **`Parser.position` requires one new export in `Parser.hs`**: a minor, backward-compatible
   extension to ADR-0042 that was not planned in that ADR's scope.

### Risks

1. **`/**` vs `/*` ordering bug**: if `comment` tries `blockComment` before `docComment`, doc
   comments are silently parsed as `BlockComment`. Any reordering of the `choice` array introduces
   this regression silently at runtime.

2. **`Text.replace` over-substitution**: block comment content containing literal `*/` (e.g., a
   comment reading `/* returns a T* pointer */`) would produce `{- returns a T{- pointer -}`,
   which is malformed Haskell.

3. **Content strip edge cases in `docComment`**: stripping `* ` prefix per line may mangle
   non-JavaDoc-style doc comments that do not use the `*` decoration convention.

4. **Nesting counter in `blockComment` implementation**: the `go` loop tracks depth through
   recursion. A pathologically malformed input (unclosed `/*`) could cause the parser to
   consume the rest of the source file before reporting an error.

### Mitigations

1. **Regression test for `/**` ordering**: `CommentSpec.hs` includes a case that parses `/** doc */`
   through `Syntax.Comment.comment` and asserts the result is `DocComment`, not `BlockComment`.

2. **Document `Text.replace` limitation**: the Haddock for `toHaskell` notes the `*/`-in-content
   edge case. A future revision can replace text substitution with a structured AST walk
   (replacing each `BlockComment.content` sub-comment node) if this becomes a real problem.

3. **`docComment` decoration stripping is lenient**: lines that do not start with `* ` are kept
   as-is (no data loss). The stripping is additive-safe.

4. **Unclosed `/*` detection**: the `go` loop will fail when it reaches `Parser.end` before
   finding the matching `*/`, producing a standard `ParseError` with the opening position in
   `BlockComment.position`. This is the correct behaviour for a malformed source file.

5. **`{-# INLINE #-}` on all thin wrappers**: following ADR-0042 §13, all public functions
   that delegate to inner parsers are annotated `{-# INLINE #-}`, ensuring no overhead at the
   call site.

## References

- [#471: NeoHaskell comment syntax parsing](https://github.com/neohaskell/NeoHaskell/issues/471)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — blocked on comment handling
- [transpiler/design/syntax.md §7 — Comments](../../transpiler/design/syntax.md)
- [ADR-0042: Parser Library — Beginner-Friendly megaparsec Wrapper](0042-parser-library.md) — `Parser.*` primitives used by this module
- [core/parser/Parser.hs](../../core/parser/Parser.hs) — public API including `lineComment`, `blockComment`, `blockCommentNested`
- [core/parser/Parser/Internal/Whitespace.hs](../../core/parser/Parser/Internal/Whitespace.hs) — existing SKIP implementations (returns `Unit`)
- [core/nhcore.cabal](../../core/nhcore.cabal) — multi-source-dir cabal configuration
