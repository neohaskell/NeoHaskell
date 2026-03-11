module Parser.Internal (
  InternalFailure,
  Parser (..),
  ParseError (..),
  ParsePosition (..),
  ParseFileError (..),
  ParseResult,
  unwrap,
  wrap,
  run,
  runPartial,
  runNamed,
  runOnFile,
  runMaybe,
) where

import Basics
import Applicable (Applicative)
import Array qualified
import Control.Applicative (Alternative)
import Thenable (Monad)
import Data.Either qualified as GhcEither
import File qualified
import Mappable (Functor)
import Maybe (Maybe (..))
import Maybe qualified
import Path (Path)
import Path qualified
import Prelude qualified
import Result (Result (..))
import Task (Task)
import Task qualified
import Text (Text)
import Text qualified

import Text.Megaparsec qualified as GhcMegaparsec

import Parser.Internal.Types
import Parser.Internal.Error qualified as Error


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


-- | Extract the inner megaparsec parser. Used only within internal modules.
unwrap :: Parser value -> GhcMegaparsec.Parsec InternalFailure Text value
unwrap (Parser p) = p
{-# INLINE unwrap #-}


-- | Wrap a megaparsec parser. Used only within internal modules.
wrap :: GhcMegaparsec.Parsec InternalFailure Text value -> Parser value
wrap = Parser
{-# INLINE wrap #-}


run :: Parser value -> Text -> Result ParseError value
run parser input =
  runNamed "<input>" parser input
{-# INLINE run #-}


-- | Run a parser on input, requiring all input to be consumed (or zero tokens consumed).
--
-- __Zero-consumption rule__: If the parser succeeds but consumes zero tokens
-- (e.g. via 'withDefault' or 'recover'), the result is returned even when
-- input remains. Use 'runPartial' if you need explicit control over
-- unconsumed input.
runNamed :: Text -> Parser value -> Text -> Result ParseError value
runNamed sourceName parser input =
  let initState = GhcMegaparsec.State
        { GhcMegaparsec.stateInput = input
        , GhcMegaparsec.stateOffset = 0
        , GhcMegaparsec.statePosState = GhcMegaparsec.PosState
            { GhcMegaparsec.pstateInput = input
            , GhcMegaparsec.pstateOffset = 0
            , GhcMegaparsec.pstateSourcePos = GhcMegaparsec.initialPos (Text.toLinkedList sourceName)
            , GhcMegaparsec.pstateTabWidth = GhcMegaparsec.defaultTabWidth
            , GhcMegaparsec.pstateLinePrefix = ""
            }
        , GhcMegaparsec.stateParseErrors = []
        }
  in case GhcMegaparsec.runParser' (unwrap parser) initState of
    (finalState, GhcEither.Right value) ->
      case (finalState.stateOffset, Text.isEmpty finalState.stateInput) of
        -- Consumed all input: success
        (_, True) -> Result.Ok value
        -- Consumed zero tokens but remaining input: success (default/recovery path)
        (0, False) -> Result.Ok value
        -- Consumed some but not all: trailing input error
        (_, False) -> do
          let endOffset = finalState.stateOffset
          let posState = GhcMegaparsec.statePosState finalState
          let sourcePos = posState.pstateSourcePos
          let lineNum   = GhcMegaparsec.unPos sourcePos.sourceLine
          let colNum    = GhcMegaparsec.unPos sourcePos.sourceColumn
          let firstChar = case Text.uncons finalState.stateInput of
                Just (ch, _) -> Just ch
                Nothing -> Nothing
          let unexpectedText = case firstChar of
                Just ch -> Just [fmt|"#{ch}"|]
                Nothing -> Nothing
          let (maybeSourceLine, _) = GhcMegaparsec.reachOffset endOffset (GhcMegaparsec.statePosState initState)
          let contextLineText = Text.fromLinkedList (Maybe.withDefault "" maybeSourceLine)
          let pointerLineText = Text.fromLinkedList (Prelude.replicate (colNum - 1) ' ' Prelude.++ "^")
          Result.Err ParseError
            { summary     = [fmt|Parse error in #{sourceName} at line #{lineNum}, column #{colNum}|]
            , position    = ParsePosition
                { sourceName = sourceName
                , line       = lineNum
                , column     = colNum
                , offset     = endOffset
                }
            , expected    = Array.fromLinkedList ["end of input"]
            , unexpected  = unexpectedText
            , contextLine = contextLineText
            , pointerLine = pointerLineText
            , hints       = Array.fromLinkedList ["Remove the unexpected token, or check for a missing separator before it"]
            , rawMessage  = [fmt|Unexpected input remaining after position #{endOffset}|]
            }
    (_, GhcEither.Left bundle) ->
      Result.Err (Error.fromBundle sourceName input bundle)
{-# INLINABLE runNamed #-}


-- | Run a parser without requiring full input consumption.
--
-- Unlike 'run', this returns successfully whenever the inner parser succeeds,
-- regardless of remaining input. Useful for parsing prefixes or streaming input.
runPartial :: Parser value -> Text -> Result ParseError value
runPartial parser input =
  let initState = GhcMegaparsec.State
        { GhcMegaparsec.stateInput = input
        , GhcMegaparsec.stateOffset = 0
        , GhcMegaparsec.statePosState = GhcMegaparsec.PosState
            { GhcMegaparsec.pstateInput = input
            , GhcMegaparsec.pstateOffset = 0
            , GhcMegaparsec.pstateSourcePos = GhcMegaparsec.initialPos (Text.toLinkedList "<input>")
            , GhcMegaparsec.pstateTabWidth = GhcMegaparsec.defaultTabWidth
            , GhcMegaparsec.pstateLinePrefix = ""
            }
        , GhcMegaparsec.stateParseErrors = []
        }
  in case GhcMegaparsec.runParser' (unwrap parser) initState of
    (_, GhcEither.Right value) -> Result.Ok value
    (_, GhcEither.Left bundle) ->
      Result.Err (Error.fromBundle "<input>" input bundle)


-- | Read a file and parse its contents.
--
-- __Security (F1)__: Reads the entire file into memory before parsing.
-- Do not call with untrusted or user-supplied paths without validating file
-- size and origin. For production use with unbounded files, check size first.
--
-- __Security (F4)__: nhcore's @Path@ type does not sanitize path traversal
-- sequences (@../@). Validate paths before passing user-controlled values.
runOnFile :: Parser value -> Path -> Task ParseFileError value
runOnFile parser path = do
  text <- File.readText path |> Task.mapError FileReadError
  case runNamed (Text.fromLinkedList (Path.toLinkedList path)) parser text of
    Result.Ok value     -> Task.yield value
    Result.Err parseErr -> Task.throw (ParseFailure parseErr)


runMaybe :: Parser value -> Text -> Maybe value
runMaybe parser input =
  case run parser input of
    Result.Ok value -> Maybe.Just value
    Result.Err _    -> Maybe.Nothing
{-# INLINE runMaybe #-}
