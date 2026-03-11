module Parser.Internal.Whitespace (
  spaces,
  whitespace,
  token,
  tokenWith,
  symbol,
  symbolIgnoringCase,
  symbolWith,
  lineComment,
  blockComment,
  blockCommentNested,
) where

import Basics
import Applicable qualified
import Text (Text)

import Text.Megaparsec qualified as GhcMegaparsec
import Text.Megaparsec.Char qualified as GhcMegaparsecChar
import Text.Megaparsec.Char.Lexer qualified as GhcLexer

import Parser.Internal (Parser (..), unwrap)


-- | Skip zero or more whitespace characters (space, tab, newline, etc.).
-- Always succeeds, even on empty input.
spaces :: Parser Unit
spaces = Parser GhcMegaparsecChar.space
{-# INLINE spaces #-}


-- | Skip one or more whitespace characters.
-- Fails on empty input or if the first character is not whitespace.
whitespace :: Parser Unit
whitespace = Parser GhcMegaparsecChar.space1
{-# INLINE whitespace #-}


-- | Run a parser and then skip any trailing whitespace (using 'spaces').
token :: forall value. Parser value -> Parser value
token parser = tokenWith spaces parser
{-# INLINE token #-}


-- | Run a parser and then run a custom space consumer after it.
-- The result of the space consumer is discarded.
tokenWith :: forall value. Parser Unit -> Parser value -> Parser value
tokenWith spaceConsumer parser =
  Parser (GhcLexer.lexeme (unwrap spaceConsumer) (unwrap parser))
{-# INLINE tokenWith #-}


-- | Match an exact text string and skip trailing whitespace.
symbol :: Text -> Parser Text
symbol t = symbolWith spaces t
{-# INLINE symbol #-}


-- | Match a text string (case-insensitive) and skip trailing whitespace.
symbolIgnoringCase :: Text -> Parser Text
symbolIgnoringCase t =
  Parser (GhcLexer.symbol' (unwrap spaces) t)
{-# INLINE symbolIgnoringCase #-}


-- | Match an exact text string and skip trailing content using a custom consumer.
symbolWith :: Parser Unit -> Text -> Parser Text
symbolWith spaceConsumer t =
  Parser (GhcLexer.symbol (unwrap spaceConsumer) t)
{-# INLINE symbolWith #-}


-- | Skip from a prefix string to the end of the current line.
lineComment :: Text -> Parser Unit
lineComment prefix =
  Parser do
    GhcLexer.skipLineComment prefix
    GhcMegaparsec.optional GhcMegaparsecChar.eol
    Applicable.pure unit
{-# INLINE lineComment #-}


-- | Skip content between an open marker and a close marker (non-nested).
blockComment :: Text -> Text -> Parser Unit
blockComment open close = Parser (GhcLexer.skipBlockComment open close)
{-# INLINE blockComment #-}


-- | Skip content between an open marker and a close marker (nested).
-- Supports nested occurrences of the same open/close pair.
blockCommentNested :: Text -> Text -> Parser Unit
blockCommentNested open close = Parser (GhcLexer.skipBlockCommentNested open close)
{-# INLINE blockCommentNested #-}
