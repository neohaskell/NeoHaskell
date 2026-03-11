module Parser.Internal.Types (
  InternalFailure,
  ParseError (..),
  ParsePosition (..),
  ParseFileError (..),
  ParseResult,
) where

import Basics
import Array (Array)
import Data.Void qualified as GhcVoid
import File
import Maybe (Maybe)
import Prelude qualified
import Result (Result)
import Text (Text)


-- | Internal alias for the megaparsec void error type.
-- Never exposed to users.
type InternalFailure = GhcVoid.Void


-- | Source position of a parse error.
--
-- Useful for editor integrations (line/column highlighting)
-- and for producing precise error messages.
data ParsePosition = ParsePosition
  { sourceName :: Text
    -- ^ File name or @\"<input>\"@ for inline parses
  , line       :: {-# UNPACK #-} Int
    -- ^ 1-based line number
  , column     :: {-# UNPACK #-} Int
    -- ^ 1-based column number
  , offset     :: {-# UNPACK #-} Int
    -- ^ 0-based character offset from start of input
  }
  deriving (Eq, Prelude.Show, Generic)


-- | Structured parse error returned by 'Parser.run' and friends.
--
-- Use 'Parser.formatError' to render a beginner-friendly multi-line message,
-- or 'Parser.formatErrorCompact' for a one-line log entry.
-- The 'rawMessage' field preserves the full megaparsec output for debugging.
data ParseError = ParseError
  { summary     :: Text
    -- ^ One-line human description of the failure
  , position    :: ParsePosition
    -- ^ Where in the input the error occurred
  , expected    :: Array Text
    -- ^ Deduplicated, sorted expected tokens/labels
  , unexpected  :: Maybe Text
    -- ^ What was actually found (Nothing if at EOF)
  , contextLine :: Text
    -- ^ Source line where the error occurred.
    -- __Security__: Contains verbatim user input. Redact before writing to server logs.
  , pointerLine :: Text
    -- ^ Caret indicator line (e.g. @\"           ^\"@)
  , hints       :: Array Text
    -- ^ Heuristic suggestions for common mistakes
  , rawMessage  :: Text
    -- ^ Full megaparsec bundle output, for debugging.
    -- __Security__: May contain verbatim user input. Use 'Parser.formatError' for display.
    -- Redact before writing to server logs.
  }
  deriving (Eq, Prelude.Show, Generic)


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
  deriving (Prelude.Show, Generic)


-- | Alias for the result of running a parser.
type ParseResult value = Result ParseError value
