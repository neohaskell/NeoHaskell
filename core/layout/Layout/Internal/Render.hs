module Layout.Internal.Render (
  renderWith,
  toLayoutOptions,
) where

import Basics
import Array qualified
import Data.Char qualified as GhcChar
import Layout.Internal (Blueprint, RenderOptions (..), WrapMode (..), unwrap)
import Prelude qualified
import Prettyprinter qualified as GhcPretty
import Prettyprinter.Render.Text qualified as GhcPrettyRender
import Text (Text)
import Text qualified


renderWith :: RenderOptions -> Blueprint annotation -> Text
renderWith options blueprint =
  case options.wrapMode of
    Balanced ->
      blueprint
        |> unwrap
        |> GhcPretty.layoutSmart (toLayoutOptions options)
        |> GhcPrettyRender.renderStrict
        |> stripTrailingWhitespaceBeforeNewline
        |> sanitizeRenderOutput
    Fast ->
      blueprint
        |> unwrap
        |> GhcPretty.layoutPretty (toFastLayoutOptions options)
        |> GhcPrettyRender.renderStrict
        |> sanitizeRenderOutput
    Compact ->
      blueprint
        |> unwrap
        |> GhcPretty.group
        |> GhcPretty.layoutPretty (GhcPretty.LayoutOptions {GhcPretty.layoutPageWidth = GhcPretty.AvailablePerLine 10_000 1.0})
        |> GhcPrettyRender.renderStrict
        |> sanitizeRenderOutput


toLayoutOptions :: RenderOptions -> GhcPretty.LayoutOptions
toLayoutOptions options =
  GhcPretty.LayoutOptions
    { GhcPretty.layoutPageWidth =
        GhcPretty.AvailablePerLine
          options.maxWidth
          (Prelude.realToFrac options.ribbonFraction)
    }


-- Fast mode intentionally uses a minimal width (fastWidth = 1) to trigger
-- aggressive line breaks with the greedy layoutPretty algorithm. This bypasses
-- the user's maxWidth setting to optimize layout performance by avoiding
-- look-ahead. Use layoutSmart (Balanced mode) if you need width-aware layout.
toFastLayoutOptions :: RenderOptions -> GhcPretty.LayoutOptions
toFastLayoutOptions options =
  let fastRibbon = options.ribbonFraction * 0.4
      fastWidth = 1
  in
    GhcPretty.LayoutOptions
      { GhcPretty.layoutPageWidth =
          GhcPretty.AvailablePerLine
            fastWidth
            (Prelude.realToFrac fastRibbon)
      }


sanitizeRenderOutput :: Text -> Text
sanitizeRenderOutput rendered =
  let clearLineEscape = Text.fromLinkedList ['\ESC', '[', 'K']
  in
    rendered
      |> stripAnsiSequences
      |> Text.replace "\r" ""
      |> Text.replace clearLineEscape ""
      |> Text.replace "\ESC[0K" ""
      |> Text.replace "\x1b[0K" ""
      |> Text.replace "\r\ESC[K" ""
      |> Text.replace "\r\x1b[K" ""
      |> Text.replace "\n\ESC[K\n" "\n\n"
      |> Text.replace "\ESC[K" ""
      |> Text.replace "\x1b[K" ""
      |> Text.replace "\x9bK" ""
      |> Text.replace "\x9b0K" ""
      |> Text.replace "\x9b" ""
      |> Text.replace "[0K" ""
      |> Text.replace "[1K" ""
      |> Text.replace "[2K" ""
      |> Text.replace "[K" ""
      |> stripWhitespaceOnlyLines
      |> collapseExcessiveNewlines


stripWhitespaceOnlyLines :: Text -> Text
stripWhitespaceOnlyLines textValue =
  let sourceLines = textValue |> Text.lines
      endsWithNewline = Text.endsWith "\n" textValue
      isWhitespaceOnly lineText =
        (not (Text.isEmpty lineText)) && Text.all isSpace lineText
      filteredLines =
        sourceLines
          |> Array.filter (\lineText -> not (isWhitespaceOnly lineText))
      resultText = filteredLines |> Text.joinWith "\n"
  in
    if endsWithNewline
      then Text.append resultText "\n"
      else resultText
  where
    isSpace char = char == ' ' || char == '\t'


stripTrailingWhitespaceBeforeNewline :: Text -> Text
stripTrailingWhitespaceBeforeNewline rendered =
  let sourceLines = rendered |> Text.lines
      endsWithNewline = Text.endsWith "\n" rendered
      lineCount = sourceLines |> Array.length
      normalizedLines =
        sourceLines
          |> Array.indexedMap
            (\lineIndex lineText ->
              let isFinalLine = lineIndex == lineCount - 1
              in
                if (not endsWithNewline) && isFinalLine
                  then lineText
                  else Text.trimRight lineText
            )
      normalizedText = normalizedLines |> Text.joinWith "\n"
  in
    if endsWithNewline
      then Text.append normalizedText "\n"
      else normalizedText


collapseExcessiveNewlines :: Text -> Text
collapseExcessiveNewlines textValue =
  if Text.contains "\n\n\n" textValue
    then
      textValue
        |> Text.replace "\n\n\n" "\n\n"
        |> collapseExcessiveNewlines
    else textValue


stripAnsiSequences :: Text -> Text
stripAnsiSequences input =
  input
    |> Text.toLinkedList
    |> strip
    |> Text.fromLinkedList
  where
    strip chars =
      case chars of
        [] -> []
        '\ESC' : '[' : tailChars -> strip (dropControl tailChars)
        '\ESC' : tailChars -> strip tailChars
        headChar : tailChars -> headChar : strip tailChars

    dropControl chars =
      case chars of
        [] -> []
        headChar : tailChars ->
          if isAsciiLetter headChar
            then tailChars
            else dropControl tailChars

    isAsciiLetter character =
      GhcChar.isAsciiUpper character || GhcChar.isAsciiLower character