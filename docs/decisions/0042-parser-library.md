# ADR-0042: Parser Library — Beginner-Friendly megaparsec Wrapper

## Status

Proposed

## Context

NeoHaskell has no first-class parsing primitive for Jess. The only parsing in nhcore today is `NeoQL.Parser`, which wraps megaparsec internally using the `GhcMegaparsec` prefix convention (per ADR-0040). That parser is bespoke — it cannot be reused, it exposes no public API, and any NeoHaskell module that needs parsing must repeat the same boilerplate.

The transpiler syntax MVP (#464) is blocked on this. It needs a parsing library that follows NeoHaskell conventions: `Result`-based errors, pipe-first style, friendly error messages, and a single import.

### Current State

1. **`NeoQL.Parser`** — Wraps `Text.Megaparsec` using `GhcMegaparsec` prefix. Returns `Result Text Expr`. Bespoke to NeoQL grammar; not usable as a general-purpose parser library.

2. **No public `Parser` type** — The internal `type Parser = GhcMegaparsec.Parsec GhcVoid.Void Text` alias in `NeoQL.Parser` is module-private. Jess cannot use megaparsec directly in NeoHaskell style without understanding multi-module structures (`Text.Megaparsec`, `Text.Megaparsec.Char`, `Text.Megaparsec.Char.Lexer`).

3. **megaparsec already in build-depends** — `megaparsec >= 9.0 && < 10` and `scientific >= 0.3 && < 0.4` are already in `nhcore.cabal`. No new dependencies are needed.

4. **nhcore multi-source-dir pattern** — nhcore uses 9+ `hs-source-dirs` in one package (`auth`, `config`, `core`, `concurrency`, `decimal`, `service`, `json`, `traits`, `system`, `http`, `meta`, `schema`, `testlib`, `options-parser`, `neoql`). A new `parser` directory follows this pattern exactly.

### Use Cases

- **Transpiler syntax MVP** (#464): the NeoHaskell transpiler needs to parse NeoHaskell source. Jess should be able to read and extend the parser without learning megaparsec.
- **Config file parsing**: a NeoHaskell app reads a `settings.ini` config file with `key = value` pairs and `#` comments. Jess writes a parser in 20 lines.
- **CSV ingestion**: a data pipeline reads CSV files uploaded via the file upload API. Jess writes a quoted-cell CSV parser without understanding `sepBy` or `lexeme`.
- **DSL authoring**: a NeoHaskell app defines its own small query or expression language. Jess builds the parser by composing `Parser.choice`, `Parser.zeroOrMore`, and `Parser.symbol`.
- **Test fixtures**: test authors parse structured text in test helpers without a separate parsing library.

### Design Goals

1. **One import**: `import Parser (Parser)` + `import Parser qualified` gives Jess everything. No `Text.Megaparsec.*` imports needed.
2. **No megaparsec knowledge required**: Jess never touches `Text.Megaparsec`, `Text.Megaparsec.Char`, or `Text.Megaparsec.Char.Lexer` directly.
3. **Result-based errors**: `Parser.run` returns `Result ParseError value`, matching the nhcore `Result` pattern used throughout the codebase.
4. **Friendly error messages**: `Parser.formatError` produces line/column, a visual caret pointer, expected/got descriptions, and heuristic hints — teaching errors, not panicking.
5. **Pipe-first**: `input |> Parser.run myParser` matches NeoHaskell's left-to-right data flow convention.
6. **Intention-revealing names**: `zeroOrMore` not `many`, `oneOrMore` not `some`, `separatedBy` not `sepBy`, `charWhere` not `satisfy`. Each name describes what it does in plain English.
7. **Opaque type**: the `Parser value` newtype does not expose its megaparsec internals. Users compose parsers; they cannot couple to `Parsec` constructors.

### GitHub Issue

- [#481: nhcore: Parser library — beginner-friendly megaparsec wrapper](https://github.com/neohaskell/NeoHaskell/issues/481)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — blocked by this ADR

## Decision

### 1. Module Placement: `core/parser/`

A new `parser` source directory in `nhcore`, following the existing multi-source-dir pattern:

```text
core/parser/
├── Parser.hs                      # Public API (only module Jess imports)
├── Parser/Internal.hs             # Megaparsec wrapping, newtype, conversions
├── Parser/Internal/Error.hs       # ParseError conversion and formatting
└── Parser/Internal/Whitespace.hs  # Default whitespace consumer + token internals

core/test-core/
├── ParserSpec.hs
├── Parser/ErrorSpec.hs
├── Parser/WhitespaceSpec.hs
└── Parser/FileSpec.hs
```

| Module                       | Exported?               | Purpose                                               |
| ---------------------------- | ----------------------- | ----------------------------------------------------- |
| `Parser`                     | Yes (`exposed-modules`) | Jess's single import                                  |
| `Parser.Internal`            | No (`other-modules`)    | Megaparsec wrapping, `runParser`, bundle conversion   |
| `Parser.Internal.Error`      | No (`other-modules`)    | `ParseError` construction from megaparsec bundles     |
| `Parser.Internal.Whitespace` | No (`other-modules`)    | Default `spaces` consumer, `token`/`symbol` internals |

**Why a separate source dir, not extending `core/`?**

| Candidate                                | Verdict    | Rationale                                                                                                                                                                                           |
| ---------------------------------------- | ---------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Add `Parser.hs` directly to `core/core/` | Rejected   | `core/core/` contains primitives (`Text`, `Array`, `Result`). A parser library is a higher-level abstraction and would increase the cognitive footprint of the `core` directory.                    |
| Add to `core/neoql/`                     | Rejected   | NeoQL is a specific query language. A general-purpose parser library should not be subsumed by a domain-specific consumer.                                                                          |
| New `core/parser/` directory             | **Chosen** | Consistent with how nhcore already separates domains: `decimal`, `neoql`, `options-parser` each get their own directory. Isolation makes the parser library testable and replaceable independently. |

### 2. Core Integration Strategy

**Phase 1** (this ADR): expose `Parser` in `exposed-modules`. Users must explicitly `import Parser qualified`. Do not add to `Core.hs` re-exports yet.

**Phase 2** (post-stabilization, tracked separately): add `import Parser as Reexported (Parser)` to `Core.hs` once the API is stable under real-world usage.

This avoids polluting the global nhcore prelude with a large API surface before real-world feedback confirms the naming and ergonomics.

### 3. Type Definitions

All types follow NeoHaskell conventions: descriptive type parameters, strict fields (via project-wide `Strict` extension), `Result` not `Either`, `Task` not `IO`. The `Parser value` newtype is opaque — its constructor is not exported.

#### Parser Type

```haskell
-- Opaque — constructor NOT exported from Parser.hs
-- Internal alias for megaparsec failure type (never exposed to users)
type InternalFailure = GhcVoid.Void

-- | An opaque parser for values of type @value@.
--
-- Compose parsers with 'Parser.map', 'Parser.andThen', 'Parser.choice'.
-- Run them with 'Parser.run', 'Parser.runNamed', or 'Parser.runOnFile'.
--
-- @
-- nameParser :: Parser Text
-- nameParser = do
--   first <- Parser.letter |> Parser.oneOrMore
--   Parser.yield (first |> Text.fromArray)
-- @
newtype Parser value
  = Parser (GhcMegaparsec.Parsec InternalFailure Text value)
  deriving (Functor, Applicative, Monad, Alternative)
```

#### Position and Error Types

```haskell
-- | Source position of a parse error.
--
-- Useful for editor integrations (line/column highlighting)
-- and for producing precise error messages.
data ParsePosition = ParsePosition
  { sourceName :: Text    -- ^ File name or "<input>" for inline parses
  , line       :: {-# UNPACK #-} Int  -- ^ 1-based line number
  , column     :: {-# UNPACK #-} Int  -- ^ 1-based column number
  , offset     :: {-# UNPACK #-} Int  -- ^ 0-based character offset from start of input
  }
  deriving (Eq, Show, Generic)

-- | Structured parse error returned by 'Parser.run' and friends.
--
-- Use 'Parser.formatError' to render a beginner-friendly multi-line message,
-- or 'Parser.formatErrorCompact' for a one-line log entry.
-- The 'rawMessage' field preserves the full megaparsec output for debugging.
data ParseError = ParseError
  { summary     :: Text          -- ^ One-line human description of the failure
  , position    :: ParsePosition -- ^ Where in the input the error occurred
  , expected    :: Array Text    -- ^ Deduplicated, sorted expected tokens/labels
  , unexpected  :: Maybe Text    -- ^ What was actually found (Nothing if at EOF)
  , contextLine :: Text          -- ^ Source line where the error occurred. __Security__: echoes user input verbatim — redact before logging.
  , pointerLine :: Text          -- ^ Caret indicator line (e.g. "           ^")
  , hints       :: Array Text    -- ^ Heuristic suggestions for common mistakes
  , rawMessage  :: Text          -- ^ Full megaparsec bundle output for debugging. __Security__: may contain user input verbatim — redact before logging.
  }
  deriving (Eq, Show, Generic)

-- | Error from 'Parser.runOnFile': either the file could not be read,
-- or the file was read but parsing failed.
--
-- @
-- case result of
--   Err (FileReadError fileErr) -> Task.throw [fmt|File error: #{fileErr}|]
--   Err (ParseFailure parseErr) -> Task.throw (Parser.formatError parseErr)
--   Ok ast -> processAst ast
-- @
data ParseFileError
  = FileReadError File.Error
  | ParseFailure ParseError
  deriving (Eq, Show, Generic)

-- | Alias for the result of running a parser.
type ParseResult value = Result ParseError value
```

### 4. Public API Reference (~70 functions)

The complete public API of `Parser.hs`, organized by category. All operators from megaparsec (`<|>`, `<?>`, `<*`, `*>`) are replaced by named functions.

#### Running and Error Rendering

| Function             | Signature                                                 | Description                                       |
| -------------------- | --------------------------------------------------------- | ------------------------------------------------- |
| `run`                | `Parser value -> Text -> Result ParseError value`         | Parse full input; source name `"<input>"`         |
| `runNamed`           | `Text -> Parser value -> Text -> Result ParseError value` | Parse with custom source name (for files/context) |
| `runOnFile`          | `Parser value -> Path -> Task ParseFileError value`       | Read file then parse (see §13: file size and path traversal)            |
| `runMaybe`           | `Parser value -> Text -> Maybe value`                     | Quick check, discards diagnostics                 |
| `formatError`        | `ParseError -> Text`                                      | Multi-line beginner-friendly error message        |
| `formatErrorCompact` | `ParseError -> Text`                                      | One-line summary for logs                         |

#### Transformations and Chaining

| Function  | Signature                                                   | Description                     |
| --------- | ----------------------------------------------------------- | ------------------------------- |
| `yield`   | `value -> Parser value`                                     | Always succeed with given value |
| `map`     | `(input -> output) -> Parser input -> Parser output`        | Transform parsed value          |
| `apply`   | `Parser (input -> output) -> Parser input -> Parser output` | Applicative application         |
| `andThen` | `(input -> Parser output) -> Parser input -> Parser output` | Dependent chaining              |

#### Labels, Failure, Branching

| Function      | Signature                               | Description                                        |
| ------------- | --------------------------------------- | -------------------------------------------------- |
| `expecting`   | `Text -> Parser value -> Parser value`  | Label parser for error messages                    |
| `problem`     | `Text -> Parser value`                  | Fail with custom message                           |
| `backtrack`   | `Parser value -> Parser value`          | Backtrack on failure so alternatives can be tried  |
| `choice`      | `Array (Parser value) -> Parser value`  | Try parsers in order (each wrapped in `backtrack`) |
| `optional`    | `Parser value -> Parser (Maybe value)`  | Parse zero or one times                            |
| `withDefault` | `value -> Parser value -> Parser value` | Use fallback value on failure                      |

#### Repetition and Sequencing

| Function                  | Signature                                                     | Description                            |
| ------------------------- | ------------------------------------------------------------- | -------------------------------------- |
| `zeroOrMore`              | `Parser value -> Parser (Array value)`                        | Zero or more repetitions (inner parser must consume; see §13)          |
| `oneOrMore`               | `Parser value -> Parser (Array value)`                        | One or more repetitions                |
| `exactly`                 | `Int -> Parser value -> Parser (Array value)`                 | Exactly N repetitions                  |
| `between`                 | `Parser open -> Parser close -> Parser value -> Parser value` | Between open and close parsers         |
| `keepLeft`                | `Parser trailing -> Parser value -> Parser value`             | Run `value` first, then discard `trailing`; "value" is left of trailing in stream   |
| `keepRight`               | `Parser leading -> Parser value -> Parser value`              | Discard `leading` first, then run `value`; "value" is right of leading in stream    |
| `pair`                    | `Parser left -> Parser right -> Parser (left, right)`         | Run both, keep both results            |
| `separatedBy`             | `Parser separator -> Parser value -> Parser (Array value)`    | 0+ values with separator               |
| `oneOrMoreSeparatedBy`    | `Parser separator -> Parser value -> Parser (Array value)`    | 1+ values with separator               |
| `separatedOrTerminatedBy` | `Parser separator -> Parser value -> Parser (Array value)`    | 0+ values, optional trailing separator |
| `collectUntil`            | `Parser ending -> Parser value -> Parser (Array value)`       | Collect until ending matches           |
| `collectUntilOneOrMore`   | `Parser ending -> Parser value -> Parser (Array value)`       | 1+ values until ending matches         |

#### Lookahead, Recovery, Control

| Function        | Signature                                                      | Description                               |
| --------------- | -------------------------------------------------------------- | ----------------------------------------- |
| `peek`          | `Parser value -> Parser value`                                 | Look ahead without consuming input        |
| `notFollowedBy` | `Parser value -> Parser Unit`                                  | Assert parser does not match              |
| `recover`       | `(ParseError -> Parser value) -> Parser value -> Parser value` | Error recovery                            |
| `asResult`      | `Parser value -> Parser (Result ParseError value)`             | Capture failure as a Result value         |
| `end`           | `Parser Unit`                                                  | Succeed only at end of input              |
| `debug`         | `Text -> Parser value -> Parser value`                         | Development trace output (label required) |

#### Text and Char Primitives

| Function           | Signature                       | Description                          |
| ------------------ | ------------------------------- | ------------------------------------ |
| `text`             | `Text -> Parser Text`           | Match exact text                     |
| `textIgnoringCase` | `Text -> Parser Text`           | Case-insensitive text match          |
| `char`             | `Char -> Parser Char`           | Match exact character                |
| `charIgnoringCase` | `Char -> Parser Char`           | Case-insensitive character match     |
| `anyChar`          | `Parser Char`                   | Any single character                 |
| `anyCharExcept`    | `Char -> Parser Char`           | Any character except the given one   |
| `charWhere`        | `(Char -> Bool) -> Parser Char` | Any character satisfying a predicate |
| `oneOfChars`       | `Array Char -> Parser Char`     | Any character from a set             |
| `noneOfChars`      | `Array Char -> Parser Char`     | Any character not in a set           |

#### Character Categories

| Function           | Signature     | Description                                   |
| ------------------ | ------------- | --------------------------------------------- |
| `letter`           | `Parser Char` | Any Unicode letter                            |
| `digit`            | `Parser Char` | Any decimal digit                             |
| `alphaNum`         | `Parser Char` | Any letter or digit                           |
| `upper`            | `Parser Char` | Any uppercase letter                          |
| `lower`            | `Parser Char` | Any lowercase letter                          |
| `space`            | `Parser Char` | A single space character                      |
| `whitespace`       | `Parser Unit` | One or more whitespace characters (discarded) |
| `newline`          | `Parser Char` | A newline character                           |
| `tab`              | `Parser Char` | A tab character                               |
| `hexadecimalDigit` | `Parser Char` | A hexadecimal digit `[0-9a-fA-F]`             |

#### Whitespace, Comments, Lexical Helpers

| Function             | Signature                                     | Description                                         |
| -------------------- | --------------------------------------------- | --------------------------------------------------- |
| `spaces`             | `Parser Unit`                                 | Zero or more whitespace characters (discarded)      |
| `lineComment`        | `Text -> Parser Unit`                         | Skip a line comment from prefix to end-of-line      |
| `blockComment`       | `Text -> Text -> Parser Unit`                 | Skip a non-nested block comment                     |
| `blockCommentNested` | `Text -> Text -> Parser Unit`                 | Skip a nested block comment                         |
| `token`              | `Parser value -> Parser value`                | Parse a token, consuming trailing spaces            |
| `tokenWith`          | `Parser Unit -> Parser value -> Parser value` | Parse a token, consuming trailing custom whitespace |
| `symbol`             | `Text -> Parser Text`                         | Match a text symbol and consume trailing spaces     |
| `symbolIgnoringCase` | `Text -> Parser Text`                         | Case-insensitive symbol + trailing spaces           |
| `symbolWith`         | `Parser Unit -> Text -> Parser Text`          | Symbol with custom whitespace consumer              |

#### Numeric Parsers

| Function      | Signature      | Description                                             |
| ------------- | -------------- | ------------------------------------------------------- |
| `decimal`     | `Parser Int`   | Unsigned decimal integer (trailing spaces consumed)     |
| `int`         | `Parser Int`   | Signed integer (trailing spaces consumed)               |
| `float`       | `Parser Float` | Signed floating-point number (trailing spaces consumed) |
| `hexadecimal` | `Parser Int`   | Unsigned hexadecimal integer (trailing spaces consumed) |

#### Convenience Wrappers

| Function        | Signature                      | Description                          |
| --------------- | ------------------------------ | ------------------------------------ |
| `parenthesized` | `Parser value -> Parser value` | Between `(` and `)` (using `symbol`) |
| `bracketed`     | `Parser value -> Parser value` | Between `[` and `]` (using `symbol`) |
| `braced`        | `Parser value -> Parser value` | Between `{` and `}` (using `symbol`) |

### 5. Naming Philosophy

Names follow NeoHaskell's **intention-revealing** convention. Instead of jargon inherited from parser combinator theory, each name describes what it **does** in plain English.

| megaparsec name | nhcore `Parser` name      | Reason for rename                                                    |
| --------------- | ------------------------- | -------------------------------------------------------------------- |
| `many`          | `zeroOrMore`              | `many` is ambiguous (many = 0? 1? 2+?)                               |
| `some`          | `oneOrMore`               | `some` is vague; `oneOrMore` is unambiguous                          |
| `sepBy`         | `separatedBy`             | Latin abbreviation; "separatedBy" is self-documenting                |
| `sepBy1`        | `oneOrMoreSeparatedBy`    | Matches nhcore prefix style (`oneOrMore`)                            |
| `sepEndBy`      | `separatedOrTerminatedBy` | Explains that trailing separator is OK                               |
| `manyTill`      | `collectUntil`            | Imperative "collect" verb matches nhcore style                       |
| `someTill`      | `collectUntilOneOrMore`   | Consistent with `oneOrMore`; non-empty variant of `collectUntil`     |
| `satisfy`       | `charWhere`               | `satisfy predicate` → `charWhere predicate`                          |
| `label` / `<?>` | `expecting`               | "I am expecting an X" — teaches users to write labels                |
| `customFailure` | `problem`                 | Short, imperative; aligns with `Task.throw`/`Result.Err`             |
| `try`           | `backtrack`               | "backtrack" explains the semantics; `try` is confusing for beginners |
| `lookAhead`     | `peek`                    | `peek` is familiar from other contexts                               |
| `observing`     | `asResult`                | Matches nhcore pattern (`Task.asResult`)                             |
| `eof`           | `end`                     | Short and plain; reads naturally in pipes                            |
| `option`        | `withDefault`             | Matches `Maybe.withDefault`, `Result.withDefault`                    |
| `lexeme`        | `token`                   | `lexeme` is Latin jargon; `token` is universally understood          |
| `dbg`           | `debug`                   | Full word; consistent with nhcore naming                             |

Precedents from nhcore for this style: `Array.takeIf` / `Array.dropIf` (not `filter`), `Result.withDefault` / `Maybe.withDefault` (not `fromMaybe`), `Task.asResult` (not `observe`/`try`), `Task.andThen` / `Maybe.andThen` (not `>>=`).

### 6. Whitespace Strategy

The `Parser` module has three whitespace modes, giving Jess the right level of control for each grammar style:

#### Explicit Mode (Default for Primitives)

`char`, `text`, `digit`, `letter`, `anyChar`, `charWhere`, etc. do **not** skip whitespace. Best for fine-grained formats (CSV, fixed-width records, binary-adjacent formats):

```haskell
-- CSV cell: no whitespace skipping — every character is intentional
plainCell :: Parser Text
plainCell = do
  characters <- Parser.noneOfChars [',', '\n', '\r'] |> Parser.zeroOrMore
  let value = characters |> Text.fromArray |> Text.trim
  Parser.yield value
```

#### Token Mode

`token`, `symbol`, and numeric parsers (`int`, `float`, `decimal`, `hexadecimal`) consume **trailing spaces** automatically. Best for language-like grammars where whitespace between tokens is expected:

```haskell
-- Tokens consume trailing whitespace automatically
keywordLet :: Parser Text
keywordLet =
  Parser.symbol "let"

-- "let x = 42" — spaces between each token are consumed implicitly
binding :: Parser (Text, Int)
binding = do
  _ <- keywordLet
  name <- Parser.token (Parser.letter |> Parser.oneOrMore |> Parser.map Text.fromArray)
  _ <- Parser.symbol "="
  value <- Parser.int
  Parser.yield (name, value)
```

#### Custom Whitespace Mode

`tokenWith` and `symbolWith` accept a custom space consumer. This lets Jess include comments in token spacing without wrapping everything manually:

```haskell
spacesAndComments :: Parser Unit
spacesAndComments = do
  _ <-
    Parser.choice
      [ Parser.whitespace
      , Parser.lineComment "#"
      ]
      |> Parser.zeroOrMore
  Parser.yield unit

configToken :: forall value. Parser value -> Parser value
configToken parser =
  parser |> Parser.tokenWith spacesAndComments
```

### 7. Error Formatting Specification

`Parser.formatError` produces a multi-line message that teaches Jess where the error is and how to fix it:

```
Parse error in settings.neo at line 4, column 12

port = 80x
           ^

Expected: integer
Found: "x"
Hint: Remove the extra character, or quote the value if it is text.
```

#### Formatting Rules

1. **First line**: `Parse error in <sourceName> at line <N>, column <M>`
2. **Blank line**, then the source line from the input (`contextLine` field)
3. **Pointer line** (`pointerLine` field) — spaces + `^` aligned to the error column
4. **Blank line**, then `Expected: <comma-separated expected list>` (deduplicated, sorted, human-readable labels)
5. **`Found: <unexpected>`** — `"x"` for unexpected characters, `end of input` when input ended unexpectedly
6. **`Hint: <hint>`** for each entry in `hints` — heuristic suggestions for common mistakes

#### Error Normalization Rules (Internal)

The `Parser.Internal.Error` module converts megaparsec's `ParseErrorBundle` to `ParseError`:

- **Expectation labels**: prefer `Parser.expecting` labels over megaparsec's raw `Tokens`/`Label` representations
- **Raw token normalization**: `Tokens "abc"` → `"\"abc\""`, `Label "digit"` → `"digit"`, `EndOfInput` → `"end of input"`
- **Deduplication**: sort and deduplicate the `expected` list
- **EOF detection**: if `unexpected` is empty and position is at end of input, set `Found: end of input`
- **Heuristic hints** (via `hints` field):
  - Unexpected `)` / `]` / `}` → `"Check for a missing opening delimiter earlier"`
  - Unexpected end of input with open delimiter context → `"Check for a missing closing delimiter"`
  - Expected separator but found value → `"Did you forget a comma or separator before this?"`
  - Trailing garbage → `"Remove the unexpected character after the valid expression"`

`formatErrorCompact` produces: `Parse error in <sourceName> at line <N>, column <M>: <summary>`

### 8. Integration Examples

#### CSV Parser

```haskell
module Example.Csv where

import Parser (Parser)
import Parser qualified
import Text (Text)
import Text qualified

quotedCell :: Parser Text
quotedCell = do
  _ <- Parser.char '"'
  characters <- Parser.anyCharExcept '"' |> Parser.zeroOrMore
  _ <- Parser.char '"'
  let value = characters |> Text.fromArray
  Parser.yield value

plainCell :: Parser Text
plainCell = do
  characters <- Parser.noneOfChars [',', '\n', '\r'] |> Parser.zeroOrMore
  let value = characters |> Text.fromArray |> Text.trim
  Parser.yield value

cell :: Parser Text
cell =
  Parser.choice [quotedCell, plainCell]

csvRow :: Parser (Array Text)
csvRow =
  cell |> Parser.separatedBy (Parser.char ',')

csvDocument :: Parser (Array (Array Text))
csvDocument = do
  rows <- csvRow |> Parser.separatedOrTerminatedBy Parser.newline
  _ <- Parser.end
  Parser.yield rows
```

#### Config File Parser (with `#` Comments)

```haskell
module Example.Config where

import Char qualified
import Parser (Parser)
import Parser qualified
import Text (Text)
import Text qualified

spacesAndComments :: Parser Unit
spacesAndComments = do
  _ <-
    Parser.choice
      [ Parser.whitespace
      , Parser.lineComment "#"
      ]
      |> Parser.zeroOrMore
  Parser.yield unit

configToken :: forall value. Parser value -> Parser value
configToken parser =
  parser |> Parser.tokenWith spacesAndComments

identifier :: Parser Text
identifier = do
  characters <-
    Parser.charWhere (\character -> Char.isAlphaNum character || character == '_')
      |> Parser.oneOrMore
  let value = characters |> Text.fromArray
  Parser.yield value

setting :: Parser (Text, Text)
setting = do
  key <- configToken identifier
  _ <- configToken (Parser.char '=')
  value <- configToken identifier
  Parser.yield (key, value)

configFile :: Parser (Array (Text, Text))
configFile = do
  items <- setting |> Parser.separatedOrTerminatedBy Parser.newline
  _ <- spacesAndComments
  _ <- Parser.end
  Parser.yield items
```

#### Arithmetic Expression Parser

```haskell
module Example.Expression where

import Array qualified
import Parser (Parser)
import Parser qualified

data Expr
  = Number Int
  | Add Expr Expr
  | Multiply Expr Expr
  deriving (Eq, Show)

factor :: Parser Expr
factor =
  Parser.choice
    [ Parser.int |> Parser.map Number
    , expression |> Parser.parenthesized
    ]

term :: Parser Expr
term = do
  first <- factor
  rest <- Parser.zeroOrMore do
    _ <- Parser.symbol "*"
    factor
  let combined =
        rest |> Array.foldl (\next current -> Multiply current next) first
  Parser.yield combined

expression :: Parser Expr
expression = do
  first <- term
  rest <- Parser.zeroOrMore do
    _ <- Parser.symbol "+"
    term
  let combined =
        rest |> Array.foldl (\next current -> Add current next) first
  Parser.yield combined
```

### 9. What's Hidden from megaparsec (and Why)

| Hidden                                          | Why                                                                                                                  |
| ----------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------- |
| `Parsec` / `ParsecT` constructors               | Less coupling, simpler types. Jess does not need to know she is using `Parsec GhcVoid.Void Text`.                    |
| `MonadParsec` typeclass                         | Jess does not need to understand MTL-style transformer stacks. All operations are concrete `Parser value` functions. |
| `Stream` / `VisualStream` / `TraversableStream` | `Parser` is Text-only in v1. No stream polymorphism.                                                                 |
| `Void` error type                               | Confusing for beginners. Handled internally as `InternalFailure`.                                                    |
| `ParseErrorBundle` / `ErrorItem` / `ErrorFancy` | Replaced by structured `ParseError` with friendly field names.                                                       |
| `<                                              | >` operator                                                                                                          | Replaced by `Parser.choice` (takes an `Array`). More beginner-friendly; avoids the `Alt` vs `Alternative` confusion. |
| `<?>` / `label` operator                        | Replaced by named function `Parser.expecting`.                                                                       |
| `<*`, `*>` operators                            | Replaced by `Parser.keepLeft` and `Parser.keepRight`.                                                                |
| `runParserT` and transformer APIs               | No monad transformers in v1 public API.                                                                              |
| `dbgTrace`                                      | Only `debug` (requires a label string) is exposed. Forces documentation at debug sites.                              |

### 10. Trait Instances

`Parser value` derives `Functor`, `Applicative`, `Monad`, and `Alternative` (required internally for repetition and choice). NeoHaskell-idiomatic helpers are exported as named functions:

| Instance method | nhcore function      |
| --------------- | -------------------- |
| `fmap f p`      | `Parser.map f p`     |
| `pure v`        | `Parser.yield v`     |
| `p <*> q`       | `Parser.apply p q`   |
| `p >>= f`       | `Parser.andThen f p` |

The `Alternative` instance is not documented as primary API — it is needed internally for `many`, `some`, and `<|>` within the implementation. Jess uses `Parser.choice` instead of `<|>`.

### 11. Cabal Changes

The following additions to `core/nhcore.cabal` are required:

**`hs-source-dirs`** (library stanza) — add `parser` alongside `neoql`:

```text
neoql
parser
```

**`exposed-modules`** (library stanza) — add `Parser` (alphabetically near `Path`):

```text
Parser
```

**`other-modules`** (library stanza):

```text
Parser.Internal
Parser.Internal.Error
Parser.Internal.Whitespace
```

**`other-modules`** (`nhcore-test-core` test stanza):

```text
ParserSpec
Parser.ErrorSpec
Parser.WhitespaceSpec
Parser.FileSpec
```

**No new `build-depends`** — `megaparsec >= 9.0 && < 10` and `scientific >= 0.3 && < 0.4` are already present in the library stanza.

### 12. Test Spec Structure

Tests live in `core/test-core/` and are auto-discovered by `hspec-discover`:

```text
core/test-core/
├── ParserSpec.hs          # Core run/formatError/yield/map/andThen/choice
├── Parser/ErrorSpec.hs    # ParseError construction and formatting
├── Parser/WhitespaceSpec.hs # spaces/token/symbol/tokenWith/symbolWith
└── Parser/FileSpec.hs     # runOnFile (mocked File.readText)
```

Minimum required test categories per suite:

- **Happy path** for every function in the public API
- **Edge cases**: empty input, single-char input, exactly-at-boundary repetitions
- **Error cases**: `Parser.run` on invalid input, `Parser.formatError` output shape, `Parser.formatErrorCompact` format
- **Whitespace modes**: explicit vs token vs custom whitespace (side-by-side comparisons showing whitespace is/is not consumed)
- **Integration**: round-trip tests using the CSV, config, and expression examples from this ADR

### 13. Implementation Notes

#### Security Advisories (Phase 2 Review — no blockers, advisory only)

The following Haddock comments MUST be present in the implementation.

**F1 (Medium) + F4 (Low): `runOnFile` — file size cap and path traversal**

```haskell
-- | Read a file and parse its contents.
--
-- __Security (F1)__: Reads the entire file into memory before parsing.
-- Do not call with untrusted or user-supplied paths without validating file
-- size and origin. For production use with unbounded files, check size first.
--
-- __Security (F4)__: nhcore's @Path@ type does not sanitize path traversal
-- sequences (@../@). Validate paths before passing user-controlled values.
runOnFile :: Parser value -> Path -> Task ParseFileError value
```

**F2 (Medium): `ParseError.contextLine` and `ParseError.rawMessage` — sensitive data echo**

Both fields echo user input verbatim. The implementation Haddock MUST warn:

```haskell
  , contextLine :: Text
    -- ^ Source line containing the error.
    -- __Security__: Contains verbatim user input. Redact before writing to server logs.
  , rawMessage  :: Text
    -- ^ Full megaparsec bundle output, for debugging.
    -- __Security__: May contain verbatim user input. Use 'Parser.formatError' for display.
    -- Redact before writing to server logs.
```

**F3 (Low): `zeroOrMore` — infinite loop on zero-consuming parsers**

```haskell
-- | Run a parser zero or more times, collecting results into an array.
--
-- __Warning__: The inner parser MUST consume input on each success.
-- Parsers that can succeed without consuming input (e.g. 'Parser.optional',
-- 'Parser.spaces') will cause an infinite loop here.
-- Use 'Parser.collectUntil' for termination-bounded repetition instead.
```

#### Performance Advisories (Phase 3 Review — no blockers, advisory only)

**`{-# INLINE #-}` on thin wrappers**: Every function that is a thin delegation to
`Parser.Internal` MUST be annotated `{-# INLINE #-}`, following the pattern in
`core/core/Task.hs`. At minimum: `yield`, `map`, `andThen`, `apply`, and all
character-level primitives (`text`, `char`, `letter`, `digit`, etc.).

**`{-# UNPACK #-}` on `ParsePosition` Int fields**: The `line`, `column`, and `offset`
fields are `Int` values on the hot-path error construction path and MUST use
`{-# UNPACK #-}` to eliminate boxing, consistent with the nhcore style guide for
primitive record fields in hot-path types. See updated `ParsePosition` definition in §3.

**Use `takeWhileP`/`takeWhile1P` for character-level repetition**: `zeroOrMore` and
`oneOrMore` applied to character parsers (`charWhere`, `digit`, `letter`, etc.) MUST
use megaparsec's `takeWhileP`/`takeWhile1P` internally. These return `Text` directly,
avoiding the `[Char] → Vector Char → Text` triple conversion that occurs with
`many`/`some` on `satisfy`.


## Consequences

### Positive

1. **Unblocks transpiler (#464)**: the transpiler syntax MVP can use `Parser` directly in NeoHaskell style, without any megaparsec coupling in transpiler code.

2. **Jess writes parsers without learning megaparsec**: one import, intention-revealing names, `Result`-based errors, and friendly formatting. The 15-minute rule is satisfied: Jess can write a working CSV parser in under 15 minutes following the example in this ADR.

3. **Consistent with nhcore conventions**: `Parser.yield`, `Parser.andThen`, `Parser.map`, `Result ParseError value`, pipe-first — everything works the same way as `Task`, `Result`, and `Array`.

4. **NeoQL.Parser can migrate**: the existing `NeoQL.Parser` module can be refactored to use the new `Parser` library in a follow-up, eliminating its inline megaparsec boilerplate (the current `GhcMegaparsec` prefix pattern in `NeoQL.Parser` is a workaround, not a design).

5. **No new dependencies**: `megaparsec` and `scientific` are already in `nhcore.cabal`. The library costs no dependency budget.

6. **Opaque type future-proofs**: because `Parser value` is opaque, the internal implementation (megaparsec version, error type, stream type) can change without breaking user code.

### Negative

1. **~55 functions to maintain**: any megaparsec API change that affects the underlying wrapping requires updating the corresponding nhcore function.

2. **Limited to Text input**: `Parser` is Text-only in v1. Binary or streaming parsers require a separate abstraction.

3. **No transformer support**: users who need `ReaderT Config (Parser value)` patterns must either restructure their parser or pass parameters manually via closures.

4. **`Alternative` instance technically available**: because `Parser` derives `Alternative`, `<|>` is technically accessible. Users may reach for it instead of `Parser.choice`. Documentation must note this explicitly.

### Risks

1. **Naming mismatch with megaparsec docs**: online megaparsec tutorials use `many`, `sepBy`, `lexeme`. Jess searching "how to parse comma-separated values in Haskell" will find megaparsec examples that do not match nhcore names. Mitigated by the naming translation table in this ADR (to be included in module Haddock).

2. **Error formatting divergence**: if megaparsec changes its error bundle structure across minor versions, `Parser.Internal.Error` may need updates. Mitigated by the version pin `megaparsec >= 9.0 && < 10` and by `rawMessage` preserving the original output.

3. **`recover` complexity**: `withRecovery` in megaparsec is one of the harder combinators to use correctly. Exposing `Parser.recover` may confuse beginners. Mitigated by documentation and by placing it in the "lookahead/recovery/control" section of Haddock with an explicit warning.

4. **`Alternative` leakage**: `<|>` is technically usable since `Parser` derives `Alternative`. Users may bypass `Parser.choice`. Mitigated by documentation and Haddock.

### Mitigations

1. **Naming translation table in Haddock**: the `Parser.hs` module Haddock includes a table mapping megaparsec names to nhcore `Parser` names, helping users who find megaparsec examples and need to translate them.

2. **Integration examples in Haddock**: the CSV, config, and expression examples from this ADR are included verbatim in the `Parser.hs` module documentation, giving Jess copy-pasteable starting points.

3. **Phase 1 / Phase 2 split**: by not auto-importing `Parser` from `Core.hs` in Phase 1, the API surface can be adjusted based on real usage before becoming a project-wide implicit dependency.

4. **`rawMessage` field**: preserving the full megaparsec error bundle in `ParseError.rawMessage` ensures that debugging is never blocked by the structured error formatting, even if the formatter misses edge cases.

5. **`recover` documentation**: the Haddock for `Parser.recover` explicitly states when recovery is needed vs. when `backtrack` + `choice` is sufficient, with examples of each.

## Future Considerations

### v1.x

- `binary`, `octal`, `scientific` number parsers
- `chainLeft` / `chainRight` helpers for expression grammars with associativity
- Additional delimiter helpers: `angled` (between `<` and `>`), `quoted`, `singleQuoted`
- Better error hints via parser labels and common typo dictionaries
- Optional "teaching mode" with extra explanatory hints enabled by a flag
- Migrate `NeoQL.Parser` to use `Parser` library (simplifies ~90 lines of boilerplate)

### v2

- `Parser.Expr` module for operator-precedence tables (Pratt parsing style)
- IDE-friendly diagnostics adapter: `ParseError -> LSP Diagnostic`
- Performance instrumentation for large grammars
- Full `Core.hs` prelude re-export once API is stable under real usage

## References

- [#481: nhcore: Parser library — beginner-friendly megaparsec wrapper](https://github.com/neohaskell/NeoHaskell/issues/481)
- [#464: Transpiler syntax MVP](https://github.com/neohaskell/NeoHaskell/issues/464) — blocked by this ADR
- [ADR-0040: NeoQL MVP](0040-neoql-mvp.md) — precedent for megaparsec use in nhcore
- [NeoQL/Parser.hs](../../core/neoql/NeoQL/Parser.hs) — existing bespoke megaparsec wrapper (pattern to be superseded)
- [core/nhcore.cabal](../../core/nhcore.cabal) — cabal config showing multi-source-dir pattern
- [megaparsec 9.x on Hackage](https://hackage.haskell.org/package/megaparsec)
