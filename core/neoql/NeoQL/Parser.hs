module NeoQL.Parser (
  parse,
) where

import Basics
import Data.Char qualified as GhcChar
import Data.Either qualified as GhcEither
import Data.Void qualified as GhcVoid
import NeoQL.Types (Expr (..), FieldName (..), Value (..))
import Result (Result (..))
import Maybe (Maybe (..))
import Text (Text)
import Text qualified
import Text.Megaparsec qualified as GhcMegaparsec
import Text.Megaparsec.Char qualified as GhcMegaparsecChar
import Text.Megaparsec.Char.Lexer qualified as GhcLexer


-- | Maximum allowed length for a ?q= query string (security guardrail).
maxQueryLength :: Int
maxQueryLength = 500


type Parser = GhcMegaparsec.Parsec GhcVoid.Void Text


-- | Parse a NeoQL expression from a Text string.
--
-- Returns Err with a human-readable message on parse failure.
-- Returns Err immediately if input exceeds 500 characters.
--
-- Examples:
--   parse ".status"              -> Ok (FieldAccess (FieldName "status"))
--   parse ".status == \"active\"" -> Ok (FieldEquals (FieldName "status") (StringValue "active"))
--   parse ".count == 42"         -> Ok (FieldEquals (FieldName "count") (NumberValue 42))
--   parse "status"               -> Err "NeoQL parse error: ..."
parse :: Text -> Result Text Expr
parse input = do
  case Text.length input > maxQueryLength of
    True ->
      Result.Err [fmt|NeoQL query too long (max #{maxQueryLength} chars)|]
    False ->
      case GhcMegaparsec.parse exprParser "" input of
        GhcEither.Left bundle ->
          Result.Err [fmt|NeoQL parse error: #{GhcMegaparsec.errorBundlePretty bundle}|]
        GhcEither.Right expr ->
          Result.Ok expr


-- | Top-level expression parser.
-- Parses: fieldAccess ( "==" value )?
-- Requires end-of-input after the expression.
exprParser :: Parser Expr
exprParser = do
  GhcMegaparsecChar.space
  fieldName <- fieldAccessParser
  GhcMegaparsecChar.space
  maybeOp <- GhcMegaparsec.optional (GhcMegaparsec.try equalityParser)
  GhcMegaparsecChar.space
  GhcMegaparsec.eof
  case maybeOp of
    Maybe.Nothing ->
      pure (FieldAccess fieldName)
    Maybe.Just value ->
      pure (FieldEquals fieldName value)


-- | Parse a field access: "." identifier
fieldAccessParser :: Parser FieldName
fieldAccessParser = do
  _ <- GhcMegaparsecChar.char '.'
  name <- identifierParser
  pure (FieldName name)


-- | Parse the equality operator and value: "==" value
equalityParser :: Parser Value
equalityParser = do
  GhcMegaparsecChar.space
  _ <- GhcMegaparsecChar.string "=="
  GhcMegaparsecChar.space
  valueParser


-- | Parse a literal value: string or number
valueParser :: Parser Value
valueParser =
  GhcMegaparsec.try stringValueParser
    GhcMegaparsec.<|> numberValueParser


-- | Parse a quoted string value: '"' chars '"'
stringValueParser :: Parser Value
stringValueParser = do
  _ <- GhcMegaparsecChar.char '"'
  chars <- GhcMegaparsec.many (GhcMegaparsec.satisfy (\c -> c != '"'))
  _ <- GhcMegaparsecChar.char '"'
  pure (StringValue (Text.fromLinkedList chars))


-- | Parse a numeric value: optional '-', digits, optional decimal
numberValueParser :: Parser Value
numberValueParser = do
  sci <- GhcLexer.signed GhcMegaparsecChar.space GhcLexer.scientific
  pure (NumberValue sci)


-- | Parse an identifier: letter (letter | digit | '_')*
identifierParser :: Parser Text
identifierParser = do
  first <- GhcMegaparsecChar.letterChar
  rest <- GhcMegaparsec.many (GhcMegaparsec.satisfy (\c -> GhcChar.isAlphaNum c || c == '_'))
  pure (Text.fromLinkedList (first : rest))
