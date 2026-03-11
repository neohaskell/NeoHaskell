-- | Beginner-friendly parser combinator library.
--
-- == Quick Start
--
-- One import, pipe-first style, Result-based errors:
--
-- @
-- import Parser (Parser)
-- import Parser qualified
--
-- csvRow :: Parser (Array Text)
-- csvRow = cell |> Parser.separatedBy (Parser.char ',')
--
-- cell :: Parser Text
-- cell = do
--   chars <- Parser.noneOfChars [',', '\\n', '\\r'] |> Parser.zeroOrMore
--   Parser.yield (chars |> Text.fromArray |> Text.trim)
-- @
--
-- == megaparsec Name Translation
--
-- @
-- megaparsec          | nhcore Parser
-- ------------------  | ----------------------
-- many                | zeroOrMore
-- some                | oneOrMore
-- sepBy               | separatedBy
-- sepBy1              | oneOrMoreSeparatedBy
-- sepEndBy            | separatedOrTerminatedBy
-- manyTill            | collectUntil
-- someTill            | collectUntilOneOrMore
-- satisfy             | charWhere
-- label / <?>         | expecting
-- customFailure       | problem
-- try                 | backtrack
-- lookAhead           | peek
-- observing           | asResult
-- eof                 | end
-- option              | withDefault
-- lexeme              | token
-- dbg                 | debug
-- @
module Parser (
  -- * Types (opaque — constructors NOT exported)
  Parser,
  ParseError (..),
  ParsePosition (..),
  ParseFileError (..),
  ParseResult,
  -- * Running
  run,
  runNamed,
  runOnFile,
  runMaybe,
  -- * Error Rendering
  formatError,
  formatErrorCompact,
  -- * Transformations
  yield,
  map,
  apply,
  andThen,
  -- * Labels, Failure, Branching
  expecting,
  problem,
  backtrack,
  choice,
  optional,
  withDefault,
  -- * Repetition and Sequencing
  zeroOrMore,
  oneOrMore,
  exactly,
  between,
  keepLeft,
  keepRight,
  pair,
  separatedBy,
  oneOrMoreSeparatedBy,
  separatedOrTerminatedBy,
  collectUntil,
  collectUntilOneOrMore,
  -- * Lookahead, Recovery, Control
  peek,
  notFollowedBy,
  recover,
  asResult,
  end,
  debug,
  -- * Text and Char Primitives
  text,
  textIgnoringCase,
  char,
  charIgnoringCase,
  anyChar,
  anyCharExcept,
  charWhere,
  oneOfChars,
  noneOfChars,
  -- * Character Categories
  letter,
  digit,
  alphaNum,
  upper,
  lower,
  space,
  whitespace,
  newline,
  tab,
  hexadecimalDigit,
  -- * Whitespace, Comments, Lexical Helpers
  spaces,
  lineComment,
  blockComment,
  blockCommentNested,
  token,
  tokenWith,
  symbol,
  symbolIgnoringCase,
  symbolWith,
  -- * Numeric Parsers
  decimal,
  int,
  float,
  hexadecimal,
  -- * Convenience Wrappers
  parenthesized,
  bracketed,
  braced,
) where

import Basics
import Array (Array)
import Array qualified
import Applicable qualified
import Char (Char)
import Control.Applicative qualified as GhcAlt
import Data.Either qualified as GhcEither
import Data.Set qualified as GhcSet
import LinkedList qualified
import Mappable qualified
import Maybe (Maybe (..))
import Path (Path)
import Prelude qualified
import Result (Result (..))
import Task (Task)
import Text (Text)
import Text qualified

import Text.Megaparsec qualified as GhcMegaparsec
import Text.Megaparsec.Char qualified as GhcMegaparsecChar
import Text.Megaparsec.Char.Lexer qualified as GhcLexer
import Text.Megaparsec.Debug qualified as GhcMegaparsecDebug

import Parser.Internal (
  Parser,            -- type name only — constructor NOT re-exported
  ParseError (..),
  ParsePosition (..),
  ParseFileError (..),
  ParseResult,
  )
import Parser.Internal qualified as Internal
import Parser.Internal.Error qualified as Error
import Parser.Internal.Whitespace qualified as Whitespace


-- ---------------------------------------------------------------------------
-- Running
-- ---------------------------------------------------------------------------

run :: Parser value -> Text -> Result ParseError value
run = Internal.run
{-# INLINE run #-}

runNamed :: Text -> Parser value -> Text -> Result ParseError value
runNamed = Internal.runNamed
{-# INLINE runNamed #-}

-- | Read a file and parse its contents.
--
-- __Security (F1)__: Reads the entire file into memory before parsing.
-- Do not call with untrusted or user-supplied paths without validating file
-- size and origin. For production use with unbounded files, check size first.
--
-- __Security (F4)__: nhcore's @Path@ type does not sanitize path traversal
-- sequences (@../@). Validate paths before passing user-controlled values.
runOnFile :: Parser value -> Path -> Task ParseFileError value
runOnFile = Internal.runOnFile

runMaybe :: Parser value -> Text -> Maybe value
runMaybe = Internal.runMaybe
{-# INLINE runMaybe #-}


-- ---------------------------------------------------------------------------
-- Error Rendering
-- ---------------------------------------------------------------------------

formatError :: ParseError -> Text
formatError = Error.formatError

formatErrorCompact :: ParseError -> Text
formatErrorCompact = Error.formatErrorCompact


-- ---------------------------------------------------------------------------
-- Transformations
-- ---------------------------------------------------------------------------

yield :: forall value. value -> Parser value
yield value = Internal.wrap (Applicable.pure value)
{-# INLINE yield #-}

map :: forall input output. (input -> output) -> Parser input -> Parser output
map f parser = Internal.wrap (Mappable.fmap f (Internal.unwrap parser))
{-# INLINE map #-}

apply :: forall input output. Parser (input -> output) -> Parser input -> Parser output
apply pf px = Internal.wrap (Applicable.apply (Internal.unwrap pf) (Internal.unwrap px))
{-# INLINE apply #-}

andThen :: forall input output. (input -> Parser output) -> Parser input -> Parser output
andThen f parser = Internal.wrap do
  value <- Internal.unwrap parser
  Internal.unwrap (f value)
{-# INLINE andThen #-}


-- ---------------------------------------------------------------------------
-- Labels, Failure, Branching
-- ---------------------------------------------------------------------------

expecting :: forall value. Text -> Parser value -> Parser value
expecting label parser =
  Internal.wrap (GhcMegaparsec.label (Text.toLinkedList label) (Internal.unwrap parser))
{-# INLINE expecting #-}

problem :: forall value. Text -> Parser value
problem message =
  Internal.wrap
    ( GhcMegaparsec.fancyFailure
        ( GhcSet.singleton
            ( GhcMegaparsec.ErrorFail (Text.toLinkedList message) )
        )
    )
{-# INLINE problem #-}

backtrack :: forall value. Parser value -> Parser value
backtrack parser = Internal.wrap (GhcMegaparsec.try (Internal.unwrap parser))
{-# INLINE backtrack #-}

choice :: forall value. Array (Parser value) -> Parser value
choice parsers =
  Internal.wrap
    ( GhcMegaparsec.choice
        ( parsers
            |> Array.toLinkedList
            |> LinkedList.map (\p -> GhcMegaparsec.try (Internal.unwrap p))
        )
    )
{-# INLINE choice #-}

optional :: forall value. Parser value -> Parser (Maybe value)
optional parser = Internal.wrap (GhcMegaparsec.optional (Internal.unwrap parser))
{-# INLINE optional #-}

withDefault :: forall value. value -> Parser value -> Parser value
withDefault v p = Internal.wrap (GhcMegaparsec.option v (Internal.unwrap p))
{-# INLINE withDefault #-}


-- ---------------------------------------------------------------------------
-- Repetition and Sequencing
-- ---------------------------------------------------------------------------

-- | Run a parser zero or more times, collecting results into an array.
--
-- __Warning__: The inner parser MUST consume input on each success.
-- Parsers that can succeed without consuming input (e.g. 'Parser.optional',
-- 'Parser.spaces') will cause an infinite loop here.
-- Use 'Parser.collectUntil' for termination-bounded repetition instead.
zeroOrMore :: forall value. Parser value -> Parser (Array value)
zeroOrMore parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.many (Internal.unwrap parser)))
{-# INLINE zeroOrMore #-}

oneOrMore :: forall value. Parser value -> Parser (Array value)
oneOrMore parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.some (Internal.unwrap parser)))
{-# INLINE oneOrMore #-}

exactly :: forall value. Int -> Parser value -> Parser (Array value)
exactly n parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.count n (Internal.unwrap parser)))
{-# INLINE exactly #-}

between :: forall open close value. Parser open -> Parser close -> Parser value -> Parser value
between open close parser =
  Internal.wrap (GhcMegaparsec.between (Internal.unwrap open) (Internal.unwrap close) (Internal.unwrap parser))
{-# INLINE between #-}

-- Run parser first, then trailing; keep parser result (parser is LEFT in stream)
keepLeft :: forall trailing value. Parser trailing -> Parser value -> Parser value
keepLeft trailing parser =
  Internal.wrap (Internal.unwrap parser GhcAlt.<* Internal.unwrap trailing)
{-# INLINE keepLeft #-}

-- Run leading first (discarded), then parser (parser is RIGHT in stream)
keepRight :: forall leading value. Parser leading -> Parser value -> Parser value
keepRight leading parser =
  Internal.wrap (Internal.unwrap leading GhcAlt.*> Internal.unwrap parser)
{-# INLINE keepRight #-}

pair :: forall left right. Parser left -> Parser right -> Parser (left, right)
pair left right =
  Internal.wrap (Mappable.fmap (,) (Internal.unwrap left) Applicable.<*> Internal.unwrap right)
{-# INLINE pair #-}

separatedBy :: forall separator value. Parser separator -> Parser value -> Parser (Array value)
separatedBy sep parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.sepBy (Internal.unwrap parser) (Internal.unwrap sep)))
{-# INLINE separatedBy #-}

oneOrMoreSeparatedBy :: forall separator value. Parser separator -> Parser value -> Parser (Array value)
oneOrMoreSeparatedBy sep parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.sepBy1 (Internal.unwrap parser) (Internal.unwrap sep)))
{-# INLINE oneOrMoreSeparatedBy #-}

separatedOrTerminatedBy :: forall separator value. Parser separator -> Parser value -> Parser (Array value)
separatedOrTerminatedBy sep parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.sepEndBy (Internal.unwrap parser) (Internal.unwrap sep)))
{-# INLINE separatedOrTerminatedBy #-}

collectUntil :: forall ending value. Parser ending -> Parser value -> Parser (Array value)
collectUntil ending parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.manyTill (Internal.unwrap parser) (Internal.unwrap ending)))
{-# INLINE collectUntil #-}

collectUntilOneOrMore :: forall ending value. Parser ending -> Parser value -> Parser (Array value)
collectUntilOneOrMore ending parser =
  Internal.wrap (Mappable.fmap Array.fromLinkedList (GhcMegaparsec.someTill (Internal.unwrap parser) (Internal.unwrap ending)))
{-# INLINE collectUntilOneOrMore #-}


-- ---------------------------------------------------------------------------
-- Lookahead, Recovery, Control
-- ---------------------------------------------------------------------------

peek :: forall value. Parser value -> Parser value
peek parser = Internal.wrap (GhcMegaparsec.lookAhead (Internal.unwrap parser))
{-# INLINE peek #-}

notFollowedBy :: forall value. Parser value -> Parser Unit
notFollowedBy parser = Internal.wrap (GhcMegaparsec.notFollowedBy (Internal.unwrap parser))
{-# INLINE notFollowedBy #-}

-- | On failure, call the recovery handler with the structured error
-- and continue parsing from that point.
--
-- __Note__: The 'ParseError' passed to the handler will have empty
-- 'contextLine' and 'pointerLine' fields, since the full input context
-- is not available during recovery. For full context, inspect the
-- 'Result' returned by 'Parser.run'.
recover :: forall value. (ParseError -> Parser value) -> Parser value -> Parser value
recover handler parser =
  Internal.wrap
    ( GhcMegaparsec.withRecovery
        (\megaErr -> Internal.unwrap (handler (Error.fromSingleError megaErr)))
        (Internal.unwrap parser)
    )
{-# INLINE recover #-}

-- | Capture a parse failure as a 'Result' value rather than a fatal error.
--
-- __Note__: The 'ParseError' in 'Result.Err' will have empty 'contextLine'
-- and 'pointerLine' fields. For full error context, use 'Parser.run'.
asResult :: forall value. Parser value -> Parser (Result ParseError value)
asResult parser =
  Internal.wrap do
    result <- GhcMegaparsec.observing (Internal.unwrap parser)
    case result of
      GhcEither.Right value  -> Applicable.pure (Result.Ok value)
      GhcEither.Left megaErr -> Applicable.pure (Result.Err (Error.fromSingleError megaErr))
{-# INLINE asResult #-}

end :: Parser Unit
end = Internal.wrap GhcMegaparsec.eof
{-# INLINE end #-}

debug :: forall value. (Prelude.Show value) => Text -> Parser value -> Parser value
debug label parser =
  Internal.wrap (GhcMegaparsecDebug.dbg (Text.toLinkedList label) (Internal.unwrap parser))
{-# INLINE debug #-}


-- ---------------------------------------------------------------------------
-- Text and Char Primitives
-- ---------------------------------------------------------------------------

text :: Text -> Parser Text
text t = Internal.wrap (GhcMegaparsecChar.string t)
{-# INLINE text #-}

textIgnoringCase :: Text -> Parser Text
textIgnoringCase t = Internal.wrap (GhcMegaparsecChar.string' t)
{-# INLINE textIgnoringCase #-}

char :: Char -> Parser Char
char c = Internal.wrap (GhcMegaparsecChar.char c)
{-# INLINE char #-}

charIgnoringCase :: Char -> Parser Char
charIgnoringCase c = Internal.wrap (GhcMegaparsecChar.char' c)
{-# INLINE charIgnoringCase #-}

anyChar :: Parser Char
anyChar = Internal.wrap GhcMegaparsec.anySingle
{-# INLINE anyChar #-}

anyCharExcept :: Char -> Parser Char
anyCharExcept c = Internal.wrap (GhcMegaparsec.satisfy (\x -> x != c))
{-# INLINE anyCharExcept #-}

charWhere :: (Char -> Bool) -> Parser Char
charWhere predicate = Internal.wrap (GhcMegaparsec.satisfy predicate)
{-# INLINE charWhere #-}

oneOfChars :: Array Char -> Parser Char
oneOfChars chars = Internal.wrap (GhcMegaparsec.oneOf (Array.toLinkedList chars))
{-# INLINE oneOfChars #-}

noneOfChars :: Array Char -> Parser Char
noneOfChars chars = Internal.wrap (GhcMegaparsec.noneOf (Array.toLinkedList chars))
{-# INLINE noneOfChars #-}


-- ---------------------------------------------------------------------------
-- Character Categories
-- ---------------------------------------------------------------------------

letter :: Parser Char
letter = Internal.wrap GhcMegaparsecChar.letterChar
{-# INLINE letter #-}

digit :: Parser Char
digit = Internal.wrap GhcMegaparsecChar.digitChar
{-# INLINE digit #-}

alphaNum :: Parser Char
alphaNum = Internal.wrap GhcMegaparsecChar.alphaNumChar
{-# INLINE alphaNum #-}

upper :: Parser Char
upper = Internal.wrap GhcMegaparsecChar.upperChar
{-# INLINE upper #-}

lower :: Parser Char
lower = Internal.wrap GhcMegaparsecChar.lowerChar
{-# INLINE lower #-}

space :: Parser Char
space = Internal.wrap (GhcMegaparsecChar.char ' ')
{-# INLINE space #-}

whitespace :: Parser Unit
whitespace = Whitespace.whitespace
{-# INLINE whitespace #-}

newline :: Parser Char
newline = Internal.wrap GhcMegaparsecChar.newline
{-# INLINE newline #-}

tab :: Parser Char
tab = Internal.wrap GhcMegaparsecChar.tab
{-# INLINE tab #-}

hexadecimalDigit :: Parser Char
hexadecimalDigit = Internal.wrap GhcMegaparsecChar.hexDigitChar
{-# INLINE hexadecimalDigit #-}


-- ---------------------------------------------------------------------------
-- Whitespace, Comments, Lexical Helpers
-- ---------------------------------------------------------------------------

spaces :: Parser Unit
spaces = Whitespace.spaces
{-# INLINE spaces #-}

lineComment :: Text -> Parser Unit
lineComment = Whitespace.lineComment
{-# INLINE lineComment #-}

blockComment :: Text -> Text -> Parser Unit
blockComment = Whitespace.blockComment
{-# INLINE blockComment #-}

blockCommentNested :: Text -> Text -> Parser Unit
blockCommentNested = Whitespace.blockCommentNested
{-# INLINE blockCommentNested #-}

token :: forall value. Parser value -> Parser value
token = Whitespace.token
{-# INLINE token #-}

tokenWith :: forall value. Parser Unit -> Parser value -> Parser value
tokenWith = Whitespace.tokenWith
{-# INLINE tokenWith #-}

symbol :: Text -> Parser Text
symbol = Whitespace.symbol
{-# INLINE symbol #-}

symbolIgnoringCase :: Text -> Parser Text
symbolIgnoringCase = Whitespace.symbolIgnoringCase
{-# INLINE symbolIgnoringCase #-}

symbolWith :: Parser Unit -> Text -> Parser Text
symbolWith = Whitespace.symbolWith
{-# INLINE symbolWith #-}


-- ---------------------------------------------------------------------------
-- Numeric Parsers
-- ---------------------------------------------------------------------------

decimal :: Parser Int
decimal =
  Internal.wrap (Mappable.fmap (Prelude.fromIntegral :: Prelude.Integer -> Int)
    (GhcLexer.lexeme (Internal.unwrap spaces) GhcLexer.decimal))
{-# INLINE decimal #-}

int :: Parser Int
int =
  Internal.wrap (Mappable.fmap (Prelude.fromIntegral :: Prelude.Integer -> Int)
    (GhcLexer.lexeme (Internal.unwrap spaces)
       (GhcLexer.signed GhcMegaparsecChar.space GhcLexer.decimal)))
{-# INLINE int #-}

float :: Parser Float
float =
  Internal.wrap (Mappable.fmap (Prelude.realToFrac :: Prelude.Double -> Float)
    (GhcLexer.lexeme (Internal.unwrap spaces)
       (GhcLexer.signed GhcMegaparsecChar.space GhcLexer.float)))
{-# INLINE float #-}

hexadecimal :: Parser Int
hexadecimal =
  Internal.wrap (Mappable.fmap (Prelude.fromIntegral :: Prelude.Integer -> Int)
    (GhcLexer.lexeme (Internal.unwrap spaces) GhcLexer.hexadecimal))
{-# INLINE hexadecimal #-}


-- ---------------------------------------------------------------------------
-- Convenience Wrappers
-- ---------------------------------------------------------------------------

parenthesized :: forall value. Parser value -> Parser value
parenthesized parser = between (symbol "(") (symbol ")") parser
{-# INLINE parenthesized #-}

bracketed :: forall value. Parser value -> Parser value
bracketed parser = between (symbol "[") (symbol "]") parser
{-# INLINE bracketed #-}

braced :: forall value. Parser value -> Parser value
braced parser = between (symbol "{") (symbol "}") parser
{-# INLINE braced #-}
