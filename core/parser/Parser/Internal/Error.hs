module Parser.Internal.Error (
  fromBundle,
  fromSingleError,
  formatError,
  formatErrorCompact,
) where

import Basics
import Char (Char)
import Array (Array)
import Array qualified
import Data.List qualified as GhcList
import Data.List.NonEmpty qualified as GhcNonEmpty
import Data.Set qualified as GhcSet
import Data.Void qualified as GhcVoid
import LinkedList qualified
import Maybe (Maybe (..))
import Maybe qualified
import Prelude qualified
import Text (Text)
import Text qualified

import Text.Megaparsec qualified as GhcMegaparsec

import Parser.Internal.Types (ParseError (..), ParsePosition (..), InternalFailure)


-- | Convert a megaparsec ParseErrorBundle (from runNamed) to a structured ParseError.
-- The full input text is available, so contextLine and pointerLine are populated.
fromBundle
  :: Text
  -> Text
  -> GhcMegaparsec.ParseErrorBundle Text InternalFailure
  -> ParseError
fromBundle sourceName input bundle = do
  let firstError = GhcNonEmpty.head bundle.bundleErrors
  let errOffset  = displayOffsetFromError input firstError
  let (maybeSourceLine, posState) = GhcMegaparsec.reachOffset errOffset bundle.bundlePosState
  let sourcePos  = posState.pstateSourcePos
  let lineNum    = GhcMegaparsec.unPos sourcePos.sourceLine
  let colNum     = GhcMegaparsec.unPos sourcePos.sourceColumn
  let contextLineText = Text.fromLinkedList (Maybe.withDefault "" maybeSourceLine)
  let pointerLineText = Text.fromLinkedList (Prelude.replicate (colNum - 1) ' ' Prelude.++ "^")
  let (expectedArr, unexpectedMaybe) = extractItems firstError
  let hintsArr   = buildHints contextLineText colNum unexpectedMaybe expectedArr
  let rawMsg     = Text.fromLinkedList (GhcMegaparsec.errorBundlePretty bundle)
  let firstExpected = expectedArr |> Array.first |> Maybe.withDefault ""
  let baseSummary = [fmt|Parse error in #{sourceName} at line #{lineNum}, column #{colNum}|]
  let summaryTxt = case unexpectedMaybe of
        Just u ->
          if Array.isEmpty expectedArr
            then [fmt|#{baseSummary}: unexpected #{u}|]
            else [fmt|#{baseSummary}: unexpected #{u}, expected #{firstExpected}|]
        Nothing ->
          if Array.isEmpty expectedArr
            then baseSummary
            else [fmt|#{baseSummary}: expected #{firstExpected}|]
  ParseError
    { summary     = summaryTxt
    , position    = ParsePosition
        { sourceName = sourceName
        , line       = lineNum
        , column     = colNum
        , offset     = errOffset
        }
    , expected    = expectedArr
    , unexpected  = unexpectedMaybe
    , contextLine = contextLineText
    , pointerLine = pointerLineText
    , hints       = hintsArr
    , rawMessage  = rawMsg
    }
{-# INLINE fromBundle #-}


-- | Convert a single megaparsec ParseError (from recover/asResult) to a structured ParseError.
-- contextLine and pointerLine are empty since full input is not available.
fromSingleError
  :: GhcMegaparsec.ParseError Text InternalFailure
  -> ParseError
fromSingleError megaErr = do
  let (expectedArr, unexpectedMaybe) = extractItems megaErr
  let rawMsg = Text.fromLinkedList (GhcMegaparsec.parseErrorPretty megaErr)
  let firstExpectedSingle = expectedArr |> Array.first |> Maybe.withDefault ""
  let summaryTxt = case unexpectedMaybe of
        Just u ->
          if Array.isEmpty expectedArr
            then [fmt|Parse error: unexpected #{u}|]
            else [fmt|Parse error: unexpected #{u}, expected #{firstExpectedSingle}|]
        Nothing ->
          if Array.isEmpty expectedArr
            then "Parse error"
            else [fmt|Parse error: expected #{firstExpectedSingle}|]
  ParseError
    { summary     = summaryTxt
    , position    = ParsePosition
        { sourceName = "<unknown>"
        , line       = 1
        , column     = 1
        , offset     = GhcMegaparsec.errorOffset megaErr
        }
    , expected    = expectedArr
    , unexpected  = unexpectedMaybe
    , contextLine = ""
    , pointerLine = ""
    , hints       = Array.empty
    , rawMessage  = rawMsg
    }
{-# INLINE fromSingleError #-}


-- | Render a ParseError as a multi-line beginner-friendly message.
formatError :: ParseError -> Text
formatError parseError = do
  let headerLine   = parseError.summary
  let contextLn    = parseError.contextLine
  let pointerLn    = parseError.pointerLine
  let expectedLine =
        if Array.isEmpty parseError.expected
          then ""
          else do
            let items = parseError.expected |> Text.joinWith ", "
            [fmt|Expected: #{items}|]
  let foundLine =
        case parseError.unexpected of
          Nothing -> "Found: end of input"
          Just u  -> [fmt|Found: #{u}|]
  let hintLines =
        parseError.hints
          |> Array.map (\h -> [fmt|Hint: #{h}|])
          |> Text.joinWith "\n"
  [ headerLine, contextLn, pointerLn, expectedLine, foundLine, hintLines ]
    |> Array.fromLinkedList
    |> Array.takeIf (\p -> not (Text.isEmpty p))
    |> Text.joinWith "\n"


-- | Render a ParseError as a compact single-line message for logs.
formatErrorCompact :: ParseError -> Text
formatErrorCompact parseError = do
  let pos = parseError.position
  let sn  = pos.sourceName
  let ln  = pos.line
  let col = pos.column
  let sm  = parseError.summary
  [fmt|Parse error in #{sn} at line #{ln}, column #{col}: #{sm}|]


-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

extractItems
  :: GhcMegaparsec.ParseError Text GhcVoid.Void
  -> (Array Text, Maybe Text)
extractItems megaErr =
  case megaErr of
    GhcMegaparsec.TrivialError _offset maybeUnexpected expectedSet ->
      let unexpectedText =
            if shouldTreatUnexpectedAsEndOfInput maybeUnexpected expectedSet
              then Nothing
              else Maybe.map formatErrorItem maybeUnexpected
          expectedList   =
            GhcSet.toList expectedSet
              |> LinkedList.map formatErrorItem
              |> GhcList.sort
              |> GhcList.nub
      in (Array.fromLinkedList expectedList, unexpectedText)
    GhcMegaparsec.FancyError _offset fancySet ->
      let messages =
            GhcSet.toList fancySet
              |> LinkedList.map formatFancyItem
              |> GhcList.sort
          unexpectedText =
            case messages of
              []        -> Nothing
              (msg : _) -> Just (Text.fromLinkedList msg)
      in (Array.empty, unexpectedText)


displayOffsetFromError :: Text -> GhcMegaparsec.ParseError Text GhcVoid.Void -> Int
displayOffsetFromError input megaErr =
  case megaErr of
    GhcMegaparsec.TrivialError errorOffset maybeUnexpected expectedSet -> do
      let inputLength = Text.length input
      let offsetAdjustment = mismatchOffsetAdjustment maybeUnexpected expectedSet
      Prelude.min inputLength (errorOffset + offsetAdjustment)
    GhcMegaparsec.FancyError errorOffset _ ->
      errorOffset


mismatchOffsetAdjustment
  :: Maybe (GhcMegaparsec.ErrorItem Char)
  -> GhcSet.Set (GhcMegaparsec.ErrorItem Char)
  -> Int
mismatchOffsetAdjustment maybeUnexpected expectedSet =
  case maybeUnexpected of
    Just (GhcMegaparsec.Tokens unexpectedTokens) -> do
      let unexpectedText = Text.fromLinkedList (GhcNonEmpty.toList unexpectedTokens)
      tokenTextsFromExpectedSet expectedSet
        |> GhcList.map (\expectedText -> commonPrefixLength unexpectedText expectedText)
        |> GhcList.foldl' Prelude.max 0
    _ ->
      0


shouldTreatUnexpectedAsEndOfInput
  :: Maybe (GhcMegaparsec.ErrorItem Char)
  -> GhcSet.Set (GhcMegaparsec.ErrorItem Char)
  -> Bool
shouldTreatUnexpectedAsEndOfInput maybeUnexpected expectedSet =
  case maybeUnexpected of
    Nothing ->
      False
    Just unexpectedItem ->
      case unexpectedItem of
        GhcMegaparsec.EndOfInput ->
          True
        GhcMegaparsec.Tokens unexpectedTokens -> do
          let unexpectedText = Text.fromLinkedList (GhcNonEmpty.toList unexpectedTokens)
          tokenTextsFromExpectedSet expectedSet
            |> GhcList.any
                (\expectedText ->
                  Text.length unexpectedText < Text.length expectedText
                    && Text.startsWith unexpectedText expectedText
                )
        _ ->
          False


tokenTextsFromExpectedSet
  :: GhcSet.Set (GhcMegaparsec.ErrorItem Char)
  -> [Text]
tokenTextsFromExpectedSet expectedSet =
  GhcSet.toList expectedSet
    |> GhcList.foldr
        (\errorItem acc ->
          case errorItem of
            GhcMegaparsec.Tokens expectedTokens ->
              Text.fromLinkedList (GhcNonEmpty.toList expectedTokens) : acc
            _ ->
              acc
        )
        []


commonPrefixLength :: Text -> Text -> Int
commonPrefixLength left right = do
  let pairedChars = GhcList.zip (Text.toLinkedList left) (Text.toLinkedList right)
  let matchingPrefix = pairedChars |> GhcList.takeWhile (Prelude.uncurry (==))
  Prelude.length matchingPrefix


formatErrorItem :: GhcMegaparsec.ErrorItem Char -> Text
formatErrorItem item =
  case item of
    GhcMegaparsec.Tokens nonEmptyTokens -> do
      let inner = Text.fromLinkedList (GhcNonEmpty.toList nonEmptyTokens)
      [fmt|"#{inner}"|]
    GhcMegaparsec.Label nonEmptyLabel ->
      Text.fromLinkedList (GhcNonEmpty.toList nonEmptyLabel)
    GhcMegaparsec.EndOfInput ->
      "end of input"


formatFancyItem :: GhcMegaparsec.ErrorFancy GhcVoid.Void -> Prelude.String
formatFancyItem item =
  case item of
    GhcMegaparsec.ErrorFail message        -> message
    GhcMegaparsec.ErrorIndentation {}      -> "incorrect indentation"
    GhcMegaparsec.ErrorCustom v            -> GhcVoid.absurd v


buildHints :: Text -> Int -> Maybe Text -> Array Text -> Array Text
buildHints _contextLine _col maybeUnexpected _expected =
  let hintList =
        case maybeUnexpected of
          Nothing ->
            ["Check for a missing closing delimiter (e.g. ), ], or })"]
          Just unexpected ->
            if unexpected == "\")\"" || unexpected == "')'"
              then ["Check for a missing opening '(' earlier in the input"]
              else if unexpected == "\"]\"" || unexpected == "']'"
                then ["Check for a missing opening '[' earlier in the input"]
                else if unexpected == "\"}\"" || unexpected == "'}'"
                  then ["Check for a missing opening '{' earlier in the input"]
                  else
                    if Text.contains "\"" unexpected
                      then ["Remove the unexpected token, or check for a missing separator before it"]
                      else ["Check the input format or parser configuration"]
  in Array.fromLinkedList hintList
