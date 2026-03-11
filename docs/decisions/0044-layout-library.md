# ADR-0044: Layout Library — Beginner-Friendly prettyprinter Wrapper

## Status

Proposed

## Context

NeoHaskell has no first-class layout/pretty-printing primitive for Jess. nhcore has strong text
primitives (`Text`, `[fmt|...|]`, console output), but no structured layout composition with
automatic line-breaking decisions.

### Current State

1. **No composable representation for "layout plans"** — there is no type that represents a tree of
   layout decisions that can be composed and then rendered in a single pass.

2. **No stable line-wrap strategy for code emission** — the NeoHaskell transpiler (#464) needs to
   emit formatted source code, but there is no shared formatting layer. Any emitter built today
   would make ad-hoc line-break decisions, producing inconsistent output.

3. **No shared formatting language for parser errors and CLI output** — `Parser.formatError` (#481)
   produces plain `Text` strings. A layout library would give parser diagnostics, CLI build
   summaries, and transpiler output a common composition model.

prettyprinter provides a proven Wadler-Lindig pretty-printing engine, but its default API names
(`Doc`, `hsep`, `flatAlt`, `layoutPretty`) are not newcomer-first. Jess should be able to use
this library without reading upstream docs.

### Use Cases

- **Transpiler code emission** (#464): the NeoHaskell transpiler generates formatted source code
  with proper indentation and wrapping.
- **Parser diagnostics** (alongside #481): format parse error messages with caret pointers,
  aligned columns, and proper indentation.
- **CLI output**: structured build summaries, status reports, and progress tables.
- **Structured debug views**: pretty-printed data for development and inspection.

### Design Goals

1. **Beginner-first naming** with plain English verbs and nouns.
2. **Opaque core type** to prevent constructor-level complexity leaking to users.
3. **Named functions as primary API** — no operators.
4. **High-quality automatic line-breaking** with sensible defaults (80 columns, smart layout).
5. **Clean interop path** for #464 transpiler output and #481 parser diagnostics.
6. **Separate structure from rendering** — build blueprints in pure code; render at the edge.
7. **Keep advanced internals hidden and swappable** — users are never blocked by prettyprinter
   internals.

### Non-Goals

1. Replacing `Text` manipulation APIs.
2. Exposing low-level prettyprinter internals (`SimpleDocStream`, `PageWidth`, `LayoutOptions`,
   `FusionDepth`).
3. Shipping a full styling/theming system in phase 1.
4. Building a full source formatter with idempotence guarantees.

### GitHub Issue

- [#482: nhcore: Layout library — beginner-friendly prettyprinter wrapper](https://github.com/neohaskell/NeoHaskell/issues/482)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — primary
  downstream consumer
- [#481: Parser library](https://github.com/neohaskell/NeoHaskell/issues/481) — sibling DevEx
  building block

## Decision

### 1. Module Placement: `core/layout/`

A new `layout` source directory in `nhcore`, following the existing multi-source-dir pattern:

```text
core/layout/
  Layout.hs                      # Public beginner API (only module Jess imports)
  Layout/Internal.hs             # Opaque types, wrap/unwrap, prettyprinter bridge
  Layout/Internal/Render.hs      # Rendering logic, options, layout algorithms
```

| Module                   | Exported?               | Purpose                                                     |
| ------------------------ | ----------------------- | ----------------------------------------------------------- |
| `Layout`                 | Yes (`exposed-modules`) | Jess's single import                                        |
| `Layout.Internal`        | No (`other-modules`)    | Prettyprinter wrapping, `Blueprint`/`RenderOptions` types   |
| `Layout.Internal.Render` | No (`other-modules`)    | Rendering, options construction, layout algorithm dispatch  |

**Why a separate source dir, not extending `core/`?**

| Candidate                              | Verdict    | Rationale                                                                                                                                                                                   |
| -------------------------------------- | ---------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Add `Layout.hs` directly to `core/core/` | Rejected | `core/core/` contains primitives (`Text`, `Array`, `Result`). A layout library is a higher-level abstraction and would increase the cognitive footprint of the `core` directory.           |
| Add to `core/neoql/`                   | Rejected   | NeoQL is a specific query language. A general-purpose layout library should not be subsumed by a domain-specific consumer.                                                                  |
| New `core/layout/` directory           | **Chosen** | Consistent with how nhcore already separates domains: `decimal`, `neoql`, `options-parser`, `parser` each get their own directory. Isolation makes the layout library testable and replaceable independently. |

### 2. Core Integration Strategy

**Phase 1** (this ADR): expose `Layout` in `exposed-modules`. Users must explicitly
`import Layout qualified`. Do not add to `Core.hs` re-exports yet.

**Phase 2** (post-stabilization, tracked separately): add `import Layout as Reexported (Blueprint)`
to `Core.hs` once the API is stable under real-world usage.

This avoids polluting the global nhcore prelude with a large API surface before real-world feedback
confirms the naming and ergonomics.

### 3. Type Definitions

All types follow NeoHaskell conventions: descriptive type parameters, strict fields (via
project-wide `Strict` extension), `Result` not `Either`, `Task` not `IO`. The
`Blueprint annotation` newtype is opaque — its constructor is not exported.

#### Blueprint Type

```haskell
-- Opaque — constructor NOT exported from Layout.hs
--
-- | An opaque layout blueprint for values annotated with @annotation@.
--
-- Build blueprints with 'Layout.text', 'Layout.stack', 'Layout.joinWords', etc.
-- Render to 'Text' with 'Layout.render' or 'Layout.renderWith'.
--
-- @
-- greeting :: Blueprint annotation
-- greeting =
--   Layout.joinWords
--     [ Layout.text "Hello,"
--     , Layout.text "world!"
--     ]
-- @
newtype Blueprint annotation
  = Blueprint (GhcPretty.Doc annotation)
  deriving (Functor)

-- Opaque — constructor NOT exported
--
-- | Options that control how a 'Blueprint' is rendered to 'Text'.
--
-- Build with 'Layout.defaultRenderOptions' and adjust with 'Layout.withMaxWidth',
-- 'Layout.withRibbonFraction', and 'Layout.withWrapMode'.
data RenderOptions = RenderOptions
  { maxWidth       :: {-# UNPACK #-} Int
  , ribbonFraction :: {-# UNPACK #-} Float
  , wrapMode       :: WrapMode
  }

-- | The line-break algorithm used when rendering a 'Blueprint'.
--
-- * 'Balanced' — avoids orphaned tokens by looking ahead (prettyprinter's @layoutSmart@). __Default.__
-- * 'Fast'     — greedy left-to-right algorithm (prettyprinter's @layoutPretty@). Faster for large documents.
-- * 'Compact'  — no line breaks; renders everything on one line (prettyprinter's @layoutCompact@).
data WrapMode
  = Balanced
  | Fast
  | Compact
  deriving (Eq, Show, Generic)

-- | Convert a value to a 'Blueprint'.
--
-- Instances are provided for 'Text', 'Int', 'Float', 'Bool',
-- 'Maybe value', and 'Result error value'.
class ToBlueprint value where
  toBlueprint :: forall annotation. value -> Blueprint annotation
```

### 4. Public API Reference (57 functions)

The complete public API of `Layout.hs`, organized by category. All operators from prettyprinter
(`<>`, `<+>`) are replaced by named functions.

#### Core Construction (10)

| Function       | Signature                                               | Description                                              |
| -------------- | ------------------------------------------------------- | -------------------------------------------------------- |
| `empty`        | `Blueprint annotation`                                  | Empty blueprint — the identity for `append`              |
| `text`         | `Text -> Blueprint annotation`                          | Convert `Text` to a blueprint                            |
| `value`        | `(ToBlueprint val) => val -> Blueprint annotation`      | Convert any `ToBlueprint` value to a blueprint           |
| `space`        | `Blueprint annotation`                                  | A single space character                                 |
| `line`         | `Blueprint annotation`                                  | Space in single-line mode; line break in multi-line      |
| `lineTight`    | `Blueprint annotation`                                  | Empty in single-line mode; line break in multi-line      |
| `softLine`     | `Blueprint annotation`                                  | Space in flat mode; break only when line is too wide     |
| `softLineTight`| `Blueprint annotation`                                  | Empty in flat mode; break only when line is too wide     |
| `hardLine`     | `Blueprint annotation`                                  | Unconditional line break — always starts a new line      |
| `blankLine`    | `Blueprint annotation`                                  | Two line breaks — inserts a blank line                   |

#### Composition and Joining (15)

| Function               | Signature                                                             | Description                                                      |
| ---------------------- | --------------------------------------------------------------------- | ---------------------------------------------------------------- |
| `append`               | `Blueprint ann -> Blueprint ann -> Blueprint ann`                     | Place two blueprints side-by-side                                |
| `appendWithSpace`      | `Blueprint ann -> Blueprint ann -> Blueprint ann`                     | Place two blueprints with a single space between                 |
| `joinWith`             | `Blueprint ann -> Array (Blueprint ann) -> Blueprint ann`             | Concatenate with a separator between each pair                   |
| `joinWords`            | `Array (Blueprint ann) -> Blueprint ann`                              | Space-separated; breaks entire group if too wide                 |
| `stack`                | `Array (Blueprint ann) -> Blueprint ann`                              | Vertical concatenation with single line breaks between           |
| `joinParagraph`        | `Array (Blueprint ann) -> Blueprint ann`                              | Fill each line with items before wrapping; adds spaces           |
| `joinAdaptive`         | `Array (Blueprint ann) -> Blueprint ann`                              | Uses `joinWords` if fits on one line; `stack` if not             |
| `joinTight`            | `Array (Blueprint ann) -> Blueprint ann`                              | Horizontal concatenation without any separators                  |
| `stackTight`           | `Array (Blueprint ann) -> Blueprint ann`                              | Vertical concatenation without blank lines between               |
| `joinDenseParagraph`   | `Array (Blueprint ann) -> Blueprint ann`                              | Like `joinParagraph` but without inter-item spaces               |
| `joinAdaptiveTight`    | `Array (Blueprint ann) -> Blueprint ann`                              | Uses `joinTight` if fits on one line; `stackTight` if not        |
| `addBetween`           | `Blueprint ann -> Array (Blueprint ann) -> Array (Blueprint ann)`     | Insert separator between each element (does not join)            |
| `commaSeparated`       | `Array (Blueprint ann) -> Blueprint ann`                              | Comma-and-space separated horizontal join                        |
| `semicolonSeparated`   | `Array (Blueprint ann) -> Blueprint ann`                              | Semicolon-and-space separated horizontal join                    |
| `pipeSeparated`        | `Array (Blueprint ann) -> Blueprint ann`                              | Pipe-and-space separated horizontal join                         |

#### Delimiters and Wrapping (9)

| Function         | Signature                                                                               | Description                                               |
| ---------------- | --------------------------------------------------------------------------------------- | --------------------------------------------------------- |
| `wrapBetween`    | `Blueprint ann -> Blueprint ann -> Blueprint ann -> Blueprint ann`                      | Wrap content between `open` and `close` delimiters        |
| `asList`         | `Array (Blueprint ann) -> Blueprint ann`                                                | Comma-separated items enclosed in `[` and `]`             |
| `asTuple`        | `Array (Blueprint ann) -> Blueprint ann`                                                | Comma-separated items enclosed in `(` and `)`             |
| `inParens`       | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `(` and `)`                                       |
| `inBrackets`     | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `[` and `]`                                       |
| `inBraces`       | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `{` and `}`                                       |
| `inAngles`       | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `<` and `>`                                       |
| `inSingleQuotes` | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `'` and `'`                                       |
| `inDoubleQuotes` | `Blueprint ann -> Blueprint ann`                                                        | Wrap in `"` and `"`                                       |

#### Layout Control (9)

| Function               | Signature                                               | Description                                                          |
| ---------------------- | ------------------------------------------------------- | -------------------------------------------------------------------- |
| `trySingleLine`        | `Blueprint ann -> Blueprint ann`                        | Try to render the entire blueprint on one line                       |
| `chooseWhenSingleLine` | `Blueprint ann -> Blueprint ann -> Blueprint ann`       | Use first blueprint in flat mode; second when broken                 |
| `indent`               | `Int -> Blueprint ann -> Blueprint ann`                 | Increase indentation of all line breaks in the blueprint by N        |
| `indentRelative`       | `Int -> Blueprint ann -> Blueprint ann`                 | Add N spaces of indentation from the current column position         |
| `alignToCurrentColumn` | `Blueprint ann -> Blueprint ann`                        | Align the blueprint to the current column position                   |
| `hangingIndent`        | `Int -> Blueprint ann -> Blueprint ann`                 | Indent all lines except the first by N                               |
| `padToWidth`           | `Int -> Blueprint ann -> Blueprint ann`                 | Pad to a minimum width with trailing spaces                          |
| `padOrBreakAt`         | `Int -> Blueprint ann -> Blueprint ann`                 | Pad to width, or break to a new line with indentation if wider       |
| `mergeAdjacentBreaks`  | `Int -> Blueprint ann -> Blueprint ann`                 | Merge consecutive break points (0 = shallow, 1+ = deep fusion)       |

#### Annotations (4)

| Function              | Signature                                                        | Description                                              |
| --------------------- | ---------------------------------------------------------------- | -------------------------------------------------------- |
| `withAnnotation`      | `ann -> Blueprint ann -> Blueprint ann`                          | Attach an annotation to a blueprint                      |
| `withoutAnnotations`  | `Blueprint ann -> Blueprint ignored`                             | Strip all annotations from a blueprint                   |
| `mapAnnotations`      | `(ann -> newAnn) -> Blueprint ann -> Blueprint newAnn`           | Transform annotations with a one-to-one function         |
| `editAnnotations`     | `(ann -> Array newAnn) -> Blueprint ann -> Blueprint newAnn`     | Transform annotations with a one-to-many function        |

#### Rendering and Options (10)

| Function              | Signature                                       | Description                                                          |
| --------------------- | ----------------------------------------------- | -------------------------------------------------------------------- |
| `defaultRenderOptions`| `RenderOptions`                                 | Default options: 80-column width, `Balanced` mode, 1.0 ribbon       |
| `withMaxWidth`        | `Int -> RenderOptions -> RenderOptions`         | Set the maximum column width                                         |
| `withRibbonFraction`  | `Float -> RenderOptions -> RenderOptions`       | Set the ribbon fraction (proportion of line for non-indent content)  |
| `withWrapMode`        | `WrapMode -> RenderOptions -> RenderOptions`    | Set the line-break algorithm                                         |
| `render`              | `Blueprint ann -> Text`                         | Render with default options (80 columns, `Balanced` mode)            |
| `renderWithWidth`     | `Int -> Blueprint ann -> Text`                  | Render with a custom column width, `Balanced` mode                   |
| `renderWith`          | `RenderOptions -> Blueprint ann -> Text`        | Render with fully custom options                                     |
| `renderBalanced`      | `Blueprint ann -> Text`                         | Render using the smart `Balanced` algorithm at default width         |
| `renderFast`          | `Blueprint ann -> Text`                         | Render using the greedy `Fast` algorithm at default width            |
| `renderCompact`       | `Blueprint ann -> Text`                         | Render with no line-breaking (compact single-line output)            |

### 5. Naming Philosophy

Names follow NeoHaskell's **intention-revealing** convention. Instead of jargon inherited from the
Wadler-Lindig algebra or Haskell operator culture, each name describes what it **does** in plain
English.

| prettyprinter name     | `Layout` name          | Reason for rename                                                              |
| ---------------------- | ---------------------- | ------------------------------------------------------------------------------ |
| `Doc`                  | `Blueprint`            | "Doc" is generic jargon; "Blueprint" evokes a plan awaiting rendering          |
| `Pretty` (typeclass)   | `ToBlueprint`          | Intention-revealing; "pretty" is too vague as a typeclass name                 |
| `pretty` (method)      | `toBlueprint`          | Matches typeclass rename; consistent with nhcore `toX` conventions             |
| `emptyDoc`             | `empty`                | Shorter; matches `Combinable` identity convention                              |
| `hardline`             | `hardLine`             | Consistent capitalization; "line" clarifies it produces a new line             |
| `line`                 | `line`                 | Already clear; kept as-is                                                      |
| `line'`                | `lineTight`            | "tight" conveys no space in flat mode                                          |
| `softline`             | `softLine`             | Consistent capitalization with `hardLine`                                      |
| `softline'`            | `softLineTight`        | "tight" pattern consistent with `lineTight`                                    |
| `flatAlt`              | `chooseWhenSingleLine` | Plain English conditional — "choose A when on a single line, B otherwise"      |
| `(<>)`                 | `append`               | Named function; no operators in public API                                     |
| `(<+>)`                | `appendWithSpace`      | Named function that describes the effect                                       |
| `hcat`                 | `joinTight`            | "tight" = no spaces; "join" = horizontal combination                           |
| `vcat`                 | `stackTight`           | "tight" = no blank lines; "stack" = vertical arrangement                       |
| `hsep`                 | `joinWords`            | "words" conveys space-separated items                                          |
| `vsep`                 | `stack`                | Short noun; "stack" = vertically arranged with separators                      |
| `sep`                  | `joinAdaptive`         | "adaptive" = adapts horizontal or vertical to available width                  |
| `cat`                  | `joinAdaptiveTight`    | Adaptive without spaces; consistent with `joinTight`                           |
| `fillSep`              | `joinParagraph`        | "paragraph" evokes word-wrap filling behaviour                                 |
| `fillCat`              | `joinDenseParagraph`   | Dense variant of paragraph filling without spaces                              |
| `concatWith`           | `joinWith`             | Consistent with nhcore `Array` style                                           |
| `punctuate`            | `addBetween`           | "add [separator] between [items]" — imperative, self-documenting               |
| `group`                | `trySingleLine`        | Plain English: "try to put this on a single line"                              |
| `nest`                 | `indent`               | "indent" is universally understood                                             |
| `align`                | `alignToCurrentColumn` | Full description; "current column" clarifies the reference point               |
| `hang`                 | `hangingIndent`        | Full term; "hanging" is a known typographic concept                            |
| `indent`               | `indentRelative`       | "relative" disambiguates from the `nest` rename above                          |
| `fill`                 | `padToWidth`           | "pad to width" is self-documenting                                             |
| `fillBreak`            | `padOrBreakAt`         | "pad or break at [width]" — describes both outcomes                            |
| `fuse`                 | `mergeAdjacentBreaks`  | Explains the effect in plain English                                           |
| `enclose`              | `wrapBetween`          | "wrap between open and close" — names all three arguments                      |
| `list`                 | `asList`               | Noun form; reads as "render as a list"                                         |
| `tupled`               | `asTuple`              | Consistent with `asList`; reads as "render as a tuple"                         |
| `parens`               | `inParens`             | "in parens" is self-documenting                                                |
| `brackets`             | `inBrackets`           | Consistent with `inParens`                                                     |
| `braces`               | `inBraces`             | Consistent with `inParens`                                                     |
| `angles`               | `inAngles`             | Consistent with `inParens`                                                     |
| `squotes`              | `inSingleQuotes`       | Plain English                                                                  |
| `dquotes`              | `inDoubleQuotes`       | Plain English                                                                  |
| `annotate`             | `withAnnotation`       | "with annotation" pattern matches nhcore `with*` convention                    |
| `unAnnotate`           | `withoutAnnotations`   | "without annotations" — negation pattern                                       |
| `reAnnotate`           | `mapAnnotations`       | nhcore style: `map` = one-to-one transform                                     |
| `alterAnnotations`     | `editAnnotations`      | "edit" = one-to-many transform; distinguishes from `mapAnnotations`            |
| `layoutPretty`         | (hidden; `renderFast`) | Hidden behind `renderFast`; `WrapMode` governs algorithm selection             |
| `layoutSmart`          | (hidden; `renderBalanced`) | Hidden behind `renderBalanced`                                             |
| `layoutCompact`        | (hidden; `renderCompact`) | Hidden; exposed as top-level `renderCompact`                                |
| `LayoutOptions`        | `RenderOptions`        | "Render" matches the user-facing action                                        |
| `PageWidth`            | (hidden; `Int`)        | Replaced by plain `Int` via `withMaxWidth`                                     |
| `FusionDepth`          | (hidden; `Int`)        | Mapped from `Int` in `mergeAdjacentBreaks`                                     |
| `SimpleDocStream`      | (hidden)               | Internal rendering stream; never user-facing                                   |

Precedents from nhcore for this style: `Array.takeIf`/`Array.dropIf` (not `filter`),
`Result.withDefault`/`Maybe.withDefault` (not `fromMaybe`), `Task.asResult` (not `observe`/`try`),
`Task.andThen`/`Maybe.andThen` (not `>>=`). ADR-0042 set the adjacent precedent in the parser
domain.

### 6. Trait Instances

`Blueprint annotation` derives `Functor` internally (over the annotation type parameter).
NeoHaskell-idiomatic trait instances:

| Instance                          | Method(s)            | nhcore function(s)                           |
| --------------------------------- | -------------------- | -------------------------------------------- |
| `Appendable (Blueprint ann)`      | `append`             | `Layout.append`                              |
| `Combinable (Blueprint ann)`      | `empty`, `append`    | `Layout.empty`, `Layout.append`              |
| `Mappable (Blueprint ann)`        | `map`                | `Layout.mapAnnotations` (maps annotation)    |
| `IsString (Blueprint annotation)` | `fromString`         | maps string literals to `Layout.text`        |

Default `ToBlueprint` instances:

| Type                                 | Renders as                                                            |
| ------------------------------------ | --------------------------------------------------------------------- |
| `Text`                               | The text verbatim                                                     |
| `Int`                                | Decimal integer                                                       |
| `Float`                              | Decimal floating-point                                                |
| `Bool`                               | `"True"` or `"False"`                                                 |
| `Maybe value`                        | `"Nothing"` or `"Just <value>"` (requires `ToBlueprint value`)       |
| `Result error value`                 | `"Err <error>"` or `"Ok <value>"` (requires both to be `ToBlueprint`) |

**Security note**: The `Result error value` and `Maybe value` instances render their inner values as-is. Do not pass values containing passwords, tokens, or PII — use `Redacted` (ADR-0016) for sensitive fields before converting to `Blueprint`.

Custom instance example:

```haskell
-- Custom instance for a domain type:
data Colour = Red | Green | Blue

instance ToBlueprint Colour where
  toBlueprint colour =
    case colour of
      Red -> Layout.text "red"
      Green -> Layout.text "green"
      Blue -> Layout.text "blue"
```

### 7. Integration Examples

#### Quick Start

```haskell
import Layout (Blueprint)
import Layout qualified

-- Simplest possible use:
greeting :: Text
greeting =
  [Layout.text "Hello,", Layout.text "world!"]
    |> Layout.joinWords
    |> Layout.render
-- => "Hello, world!"

-- Stacking lines with custom width:
twoLines :: Text
twoLines =
  [Layout.text "line one", Layout.text "line two"]
    |> Layout.stack
    |> Layout.renderWithWidth 40
-- => "line one\nline two"
```

#### Transpiler Code Emission (#464)

```haskell
module Transpiler.Emit where

import Array qualified
import Layout (Blueprint)
import Layout qualified
import Text (Text)

-- | Emit a type signature with arrow-separated types
emitTypeSig :: Text -> Array Text -> Blueprint annotation
emitTypeSig name types =
  Layout.joinWords
    [ Layout.text name
    , Layout.text "::"
    , types
        |> Array.map Layout.text
        |> Layout.addBetween (Layout.appendWithSpace (Layout.text "->") Layout.softLine)
        |> Layout.joinTight
        |> Layout.alignToCurrentColumn
    ]

-- | Emit a let-binding with indented body
emitLetBinding :: Text -> Blueprint annotation -> Blueprint annotation
emitLetBinding name body =
  Layout.trySingleLine
    ( Layout.stackTight
        [ Layout.joinWords [Layout.text "let", Layout.text name, Layout.text "="]
        , Layout.indent 2 body
        ]
    )

-- | Emit a module header with an explicit export list
emitModuleHeader :: Text -> Array Text -> Blueprint annotation
emitModuleHeader moduleName exports =
  Layout.joinWords
    [ Layout.text "module"
    , Layout.text moduleName
    , exports |> Array.map Layout.text |> Layout.asList
    , Layout.text "where"
    ]
```

#### Parser Diagnostics (alongside #481)

```haskell
module Parser.Diagnostics where

import Array qualified
import Layout (Blueprint)
import Layout qualified
import Parser (ParseError)
import Text (Text)

-- | Render a parse error as a structured diagnostic blueprint
formatDiagnostic :: ParseError -> Blueprint annotation
formatDiagnostic parseError =
  Layout.stackTight
    [ Layout.joinWords
        [ Layout.text "Parse error:"
        , Layout.text parseError.summary
        ]
    , Layout.hardLine
    , Layout.indent 4
        ( Layout.stackTight
            [ Layout.text parseError.contextLine
            , Layout.text parseError.pointerLine
            ]
        )
    , Layout.hardLine
    , Layout.joinWords
        [ Layout.text "Expected:"
        , parseError.expected
            |> Array.map Layout.text
            |> Layout.commaSeparated
        ]
    ]

-- | Render a diagnostic to text at a given column width
renderDiagnostic :: Int -> ParseError -> Text
renderDiagnostic columnWidth parseError =
  parseError
    |> formatDiagnostic
    |> Layout.renderWithWidth columnWidth
```

#### CLI Build Summary

```haskell
module Cli.Build.Report where

import Array qualified
import Layout (Blueprint)
import Layout qualified
import Text (Text)
import Text qualified

data TestResult
  = Passed
  | Failed

-- | Render a single test suite row with aligned columns
suiteRow :: Text -> TestResult -> Blueprint annotation
suiteRow suiteName result =
  Layout.joinWith (Layout.text "  ")
    [ Layout.padToWidth 40 (Layout.text suiteName)
    , case result of
        Passed -> Layout.text "[PASS]"
        Failed -> Layout.text "[FAIL]"
    ]

-- | Render a complete build report as a formatted table
buildReport :: Array (Text, TestResult) -> Blueprint annotation
buildReport results =
  Layout.stackTight
    [ Layout.text "Build Summary"
    , Layout.text (Text.replicate 50 '-')
    , Layout.joinWith (Layout.text "  ")
        [ Layout.padToWidth 40 (Layout.text "Suite")
        , Layout.text "Result"
        ]
    , Layout.text (Text.replicate 50 '-')
    , results
        |> Array.map
            ( \item -> case item of
                (name, result) -> suiteRow name result
            )
        |> Layout.stackTight
    , Layout.text (Text.replicate 50 '-')
    ]
```

### 8. What's Hidden from prettyprinter (and Why)

| Hidden                        | Why                                                                                                                          |
| ----------------------------- | ---------------------------------------------------------------------------------------------------------------------------- |
| `SimpleDocStream`             | Internal rendering stream — users compose `Blueprint` values, not stream nodes. Exposing this would require users to understand the two-phase rendering model. |
| `PageWidth`                   | Replaced by a plain `Int` in `withMaxWidth`. Wrapping a column count in a constructor adds indirection with no user benefit. |
| `LayoutOptions`               | Replaced by `RenderOptions` with builder functions (`withMaxWidth`, `withWrapMode`, etc.). Constructor-level access would expose `PageWidth` and `FusionDepth`. |
| `FusionDepth`                 | Mapped from `Int` in `mergeAdjacentBreaks` (0 = `Shallow`, 1+ = `Deep`). Beginners do not need to understand the `Shallow`/`Deep` distinction. |
| `Blueprint` constructor       | Direct construction would leak `GhcPretty.Doc`. All layout is built through named functions. |
| `RenderOptions` constructor   | Built via `defaultRenderOptions` + builder functions. Prevents coupling to field names that may change. |
| `Doc` type name               | Renamed to `Blueprint`. Users never see the raw prettyprinter type.                                                          |
| `Pretty` typeclass            | Renamed to `ToBlueprint`. Never imported from prettyprinter directly.                                                        |

Four reasons for this hiding strategy:

1. **Preserve the newcomer-safe mental model** — Jess thinks in `Blueprint` values, not rendering
   streams.
2. **Keep API stable while internals evolve** — prettyprinter's rendering pipeline can change
   without touching the `Layout` public surface.
3. **Avoid leaking upstream jargon** — `LayoutOptions`, `FusionDepth`, and `SimpleDocStream` would
   appear in error messages and autocomplete, confusing Jess.
4. **Keep one obvious beginner path** — without visible constructors, there is only one way to
   build a `Blueprint`: through the public API.

### 9. Cabal Changes

The following additions to `core/nhcore.cabal` are required:

**`hs-source-dirs`** (library stanza) — add `layout` alongside `parser`:

```text
parser
layout
```

**`exposed-modules`** (library stanza) — add `Layout` (alphabetically, after `Json`):

```text
Layout
```

**`other-modules`** (library stanza):

```text
Layout.Internal
Layout.Internal.Render
```

**`other-modules`** (`nhcore-test-core` test stanza):

```text
LayoutSpec
Layout.RenderSpec
```

**`build-depends`** (library stanza) — add prettyprinter:

```text
prettyprinter >=1.7.1 && <1.8
```

### 10. Test Spec Structure

Tests live in `core/test-core/` and are auto-discovered by `hspec-discover`:

```text
core/test-core/
  LayoutSpec.hs           # Core construction, composition, rendering
  Layout/RenderSpec.hs    # Rendering modes, options, width control
```

Minimum required test categories per suite:

- **Happy path** for every function in the public API
- **Edge cases**: `empty` document, single-character document, exactly-at-width rendering, zero-width
  rendering
- **Rendering modes**: `render`, `renderWith` (each `WrapMode`), `renderCompact` — all three must
  produce different output on a document containing `trySingleLine` and `line`
- **Options**: `withMaxWidth`, `withRibbonFraction`, `withWrapMode` — verify each affects output
  independently
- **Integration**: round-trip tests using the transpiler, diagnostics, and build report examples
  from this ADR

### 11. Implementation Notes

#### Performance

- `{-# INLINE #-}` on all thin wrappers. Every function that is a thin delegation to
  `Layout.Internal` MUST be annotated `{-# INLINE #-}`, following the pattern in
  `core/core/Task.hs`. At minimum: `text`, `append`, `appendWithSpace`, `trySingleLine`, `indent`,
  `alignToCurrentColumn`, `render`, `renderWith`.
- `{-# INLINE #-}` on additional thin wrappers: `stack`, `joinWords`, `joinWith`, `empty`, `space`, `line`, `hardLine`.
- Use `import Prettyprinter qualified as GhcPretty` for the core prettyprinter module.
- Use `import Prettyprinter.Render.Text qualified as GhcPrettyRender` for rendering to `Text`.
- `mergeAdjacentBreaks` maps its `Int` argument to `GhcPretty.FusionDepth`: `0` maps to
  `GhcPretty.Shallow`, any value `>= 1` maps to `GhcPretty.Deep`.

#### WrapMode and append

- **WrapMode constructors**: The `WrapMode` constructors (`Balanced`, `Fast`, `Compact`) ARE exported from `Layout.hs`.
- **append argument order**: Argument order is `append right left` — the first argument appears on the right, the piped/last argument on the left. In a pipe: `leftDoc |> Layout.append rightDoc`.
- **Advanced optimization**: `mergeAdjacentBreaks` is an advanced optimization. With depth `>= 1` (deep fusion), prettyprinter recursively traverses the entire document tree. For very deeply nested blueprints (> ~10,000 nodes), this may cause stack overflow. Mark as advanced in Haddock.
- **Input clamping**: All `Int` arguments controlling width or indent level (`padToWidth`, `indent`, `indentRelative`, `hangingIndent`, `padOrBreakAt`, `mergeAdjacentBreaks`) MUST be clamped to the range `0..10_000` at implementation time. Negative values are treated as `0`. `withRibbonFraction` MUST clamp its `Float` argument to `(0.0, 1.0]`, guarding against `NaN` and `Infinity`.

## Consequences

### Positive

1. **Unblocks transpiler (#464)**: the transpiler syntax MVP can use `Layout` directly in
   NeoHaskell style, without any prettyprinter coupling in transpiler code.

2. **Jess builds layout plans without learning prettyprinter**: one import, intention-revealing
   names, pipe-first style, and `render` defaults. The 15-minute rule is satisfied: Jess can format
   a CLI table or diagnostic message in under 15 minutes following the examples in this ADR.

3. **Consistent with nhcore conventions**: `Layout.render`, `Layout.append`, `Layout.stack` —
   everything works the same way as `Task`, `Result`, and `Array`. Pipe-first composition is
   natural.

4. **Shared layout language for parser errors, CLI, and transpiler output**: `Parser.formatError`
   (#481), CLI summaries, and transpiler emission all compose `Blueprint` values. Diagnostics,
   reports, and emitted code look consistent.

5. **Opaque type future-proofs**: because `Blueprint annotation` is opaque, the internal
   implementation (prettyprinter version, rendering algorithm) can change without breaking user
   code.

### Negative

1. **57 functions to maintain**: any prettyprinter API change that affects the underlying wrapping
   requires updating the corresponding nhcore function.

2. **No streaming support in v1**: `renderWith` reads the entire rendered document into a `Text`
   value. Large documents require proportional memory.

3. **prettyprinter naming mismatch with online documentation**: online tutorials and Haddock for
   prettyprinter use `hsep`, `nest`, `group`. Jess searching for prettyprinter examples will find
   names that do not match nhcore.

### Risks

1. **Naming divergence from prettyprinter tutorials**: Jess may find prettyprinter examples online
   and be confused by the name differences. Mitigated by the naming translation table in module
   Haddock.

2. **prettyprinter API changes across versions**: if prettyprinter makes breaking changes,
   `Layout.Internal` must be updated. Mitigated by the version pin `prettyprinter >=1.7.1 && <1.8`.

### Mitigations

1. **Naming translation table in Haddock**: the `Layout.hs` module Haddock includes the full table
   from §5, mapping prettyprinter names to nhcore `Layout` names. Users who find prettyprinter
   examples can translate them directly.

2. **Integration examples in Haddock**: the transpiler, diagnostics, and build report examples from
   this ADR are included verbatim in the `Layout.hs` module documentation, giving Jess
   copy-pasteable starting points.

3. **Version pin `prettyprinter >=1.7.1 && <1.8`**: locks to a stable major version. Any upgrade
   is explicit and reviewed.

## Future Considerations

1. **`Layout.Style` presets** (`error`, `warning`, `hint`, `muted`) backed by annotations — a
   follow-up ADR would define a standard annotation type and rendering adapter.
2. **Domain helpers** (`Layout.Json`, `Layout.Tree`) — composable helpers for common
   structured-data formats.
3. **Width presets** (`narrow`, `standard`, `wide`) as named `RenderOptions` values once usage
   patterns stabilise.
4. **Optional streaming renderer** for large outputs — a `renderToFileStreaming` that pages the
   output without holding the entire rendered string in memory.
5. **Transpiler-target-specific layout profiles** once #464 expands — e.g., a `NeoHaskell` target
   that applies NeoHaskell-specific indentation rules.

## References

- [#482: nhcore: Layout library — beginner-friendly prettyprinter wrapper](https://github.com/neohaskell/NeoHaskell/issues/482)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464)
- [#481: Parser library](https://github.com/neohaskell/NeoHaskell/issues/481)
- [ADR-0042: Parser Library](0042-parser-library.md) — structural precedent
- [ADR-0043: NeoHaskell Comment Syntax Parsing](0043-neohaskell-comment-syntax-parsing.md) — adjacent parsing ADR
- [core/nhcore.cabal](../../core/nhcore.cabal) — cabal config showing multi-source-dir pattern
- [prettyprinter on Hackage](https://hackage.haskell.org/package/prettyprinter)