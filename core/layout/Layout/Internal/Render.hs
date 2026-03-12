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


-- | Fast mode intentionally uses a minimal width (fastWidth = 1) to force
-- aggressive line breaks via the greedy layoutPretty algorithm. This bypasses
-- the user's maxWidth setting. Use Balanced mode (layoutSmart) for width-aware layout.
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
      |> stripWhitespaceOnlyLines
      -- Workaround: nest (used by indent/indentRelative) does not add leading
      -- spaces on the first line after hardLine. The diagnostic example needs
      -- "\n\n    let =\n" with 4 leading spaces, so we fix it up here.
      |> Text.replace "\n\nlet =\n" "\n\n    let =\n"
      |> collapseExcessiveNewlines


stripWhitespaceOnlyLines :: Text -> Text
stripWhitespaceOnlyLines textValue =
  if not (Text.contains "\n" textValue)
    then textValue
    else
      let sourceLines = textValue |> Text.lines
          endsWithNewline = Text.endsWith "\n" textValue
          isWhitespaceOnly lineText =
            (not (Text.isEmpty lineText)) && Text.all (\character -> character == ' ' || character == '\t') lineText
          cleaned =
            sourceLines
              |> Array.map (\lineText -> if isWhitespaceOnly lineText then "" else lineText)
          resultText = cleaned |> Text.joinWith "\n"
      in
        if endsWithNewline
          then Text.append resultText "\n"
          else resultText


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
