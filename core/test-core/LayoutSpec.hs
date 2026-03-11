module LayoutSpec where

import Combinable qualified
import Core
import Layout (Blueprint, ToBlueprint (..))
import Layout qualified
import Mappable qualified
import Test
import Text qualified


shortLineDoc :: Blueprint ann
shortLineDoc = Layout.joinTight [Layout.text "a", Layout.line, Layout.text "b"]


shortLineTightDoc :: Blueprint ann
shortLineTightDoc = Layout.joinTight [Layout.text "a", Layout.lineTight, Layout.text "b"]


shortSoftLineDoc :: Blueprint ann
shortSoftLineDoc = Layout.joinTight [Layout.text "a", Layout.softLine, Layout.text "b"]


shortSoftLineTightDoc :: Blueprint ann
shortSoftLineTightDoc = Layout.joinTight [Layout.text "a", Layout.softLineTight, Layout.text "b"]


breakDoc :: Blueprint ann
breakDoc =
  Layout.joinTight [Layout.text "a", Layout.line, Layout.line, Layout.text "b"]


expectRender :: Text -> Blueprint ann -> Task Text Unit
expectRender expected blueprint =
  Layout.render blueprint |> shouldBe expected


expectRenderWithWidth :: Int -> Text -> Blueprint ann -> Task Text Unit
expectRenderWithWidth width expected blueprint =
  Layout.renderWithWidth width blueprint |> shouldBe expected


expectSameRender :: Blueprint annA -> Blueprint annB -> Task Text Unit
expectSameRender left right = do
  let expected = Layout.render left
  let actual = Layout.render right
  actual |> shouldBe expected


spec :: Spec Unit
spec = do
  describe "Layout.Construction" do
    it "empty renders as empty string" \_ -> do
      Layout.empty |> expectRender ""

    it "empty is left identity for append" \_ -> do
      let doc = Layout.text "hello"
      Layout.append Layout.empty doc |> expectRender "hello"

    it "empty is right identity for append" \_ -> do
      let doc = Layout.text "hello"
      Layout.append doc Layout.empty |> expectRender "hello"

    it "text renders ascii verbatim" \_ -> do
      Layout.text "hello" |> expectRender "hello"

    it "text renders empty input" \_ -> do
      Layout.text "" |> expectRender ""

    it "text renders unicode verbatim" \_ -> do
      Layout.text "hola-ñ" |> expectRender "hola-ñ"

    it "value converts Text through ToBlueprint" \_ -> do
      Layout.value ("neo" :: Text) |> expectRender "neo"

    it "value converts Int through ToBlueprint" \_ -> do
      Layout.value (42 :: Int) |> expectRender "42"

    it "value converts Bool through ToBlueprint" \_ -> do
      Layout.value False |> expectRender "False"

    it "space renders one space" \_ -> do
      Layout.space |> expectRender " "

    it "space composes between words" \_ -> do
      Layout.joinTight [Layout.text "a", Layout.space, Layout.text "b"]
        |> expectRender "a b"

    it "space is preserved in compact mode" \_ -> do
      Layout.renderCompact (Layout.joinTight [Layout.text "a", Layout.space, Layout.text "b"])
        |> shouldBe "a b"

    it "line renders as space in flat group" \_ -> do
      Layout.trySingleLine shortLineDoc |> expectRender "a b"

    it "line renders as newline when broken" \_ -> do
      Layout.trySingleLine shortLineDoc |> expectRenderWithWidth 1 "a\nb"

    it "line standalone renders newline" \_ -> do
      Layout.line |> expectRender "\n"

    it "lineTight renders empty in flat group" \_ -> do
      Layout.trySingleLine shortLineTightDoc |> expectRender "ab"

    it "lineTight renders newline when broken" \_ -> do
      Layout.trySingleLine shortLineTightDoc |> expectRenderWithWidth 1 "a\nb"

    it "lineTight standalone renders newline" \_ -> do
      Layout.lineTight |> expectRender "\n"

    it "softLine renders space when fit" \_ -> do
      Layout.trySingleLine shortSoftLineDoc |> expectRender "a b"

    it "softLine renders newline when too wide" \_ -> do
      Layout.trySingleLine shortSoftLineDoc |> expectRenderWithWidth 1 "a\nb"

    it "softLine standalone renders space" \_ -> do
      Layout.softLine |> expectRender " "

    it "softLineTight renders empty when fit" \_ -> do
      Layout.trySingleLine shortSoftLineTightDoc |> expectRender "ab"

    it "softLineTight renders newline when too wide" \_ -> do
      Layout.trySingleLine shortSoftLineTightDoc |> expectRenderWithWidth 1 "a\nb"

    it "softLineTight standalone renders empty" \_ -> do
      Layout.softLineTight |> expectRender ""

    it "hardLine always renders newline" \_ -> do
      Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"]
        |> expectRender "a\nb"

    it "hardLine is not flattened by trySingleLine" \_ -> do
      Layout.trySingleLine (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "hardLine preserved in renderCompact" \_ -> do
      Layout.renderCompact (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> shouldBe "a\nb"

    it "blankLine renders two newlines" \_ -> do
      Layout.blankLine |> expectRender "\n\n"

    it "blankLine inserts visual blank line" \_ -> do
      Layout.joinTight [Layout.text "a", Layout.blankLine, Layout.text "b"]
        |> expectRender "a\n\nb"

    it "blankLine preserved in renderCompact" \_ -> do
      Layout.renderCompact (Layout.joinTight [Layout.text "a", Layout.blankLine, Layout.text "b"])
        |> shouldBe "a\n\nb"

  describe "Layout.Composition" do
    it "append preserves pipe-first argument order" \_ -> do
      Layout.append (Layout.text "b") (Layout.text "a") |> expectRender "ab"

    it "append handles empty right argument" \_ -> do
      Layout.append Layout.empty (Layout.text "a") |> expectRender "a"

    it "append handles empty left argument" \_ -> do
      Layout.append (Layout.text "a") Layout.empty |> expectRender "a"

    it "appendWithSpace inserts one space" \_ -> do
      Layout.appendWithSpace (Layout.text "b") (Layout.text "a") |> expectRender "a b"

    it "appendWithSpace with empty right keeps trailing separator space" \_ -> do
      Layout.appendWithSpace Layout.empty (Layout.text "a") |> expectRender "a "

    it "appendWithSpace with empty left keeps leading separator space" \_ -> do
      Layout.appendWithSpace (Layout.text "a") Layout.empty |> expectRender " a"

    it "joinWith empty array returns empty output" \_ -> do
      Layout.joinWith (Layout.text ", ") [] |> expectRender ""

    it "joinWith single-element array returns element" \_ -> do
      Layout.joinWith (Layout.text ", ") [Layout.text "a"] |> expectRender "a"

    it "joinWith multi-element array inserts separators" \_ -> do
      Layout.joinWith (Layout.text ", ") [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a, b, c"

    it "joinWords empty array returns empty output" \_ -> do
      Layout.joinWords [] |> expectRender ""

    it "joinWords single-element array returns element" \_ -> do
      Layout.joinWords [Layout.text "alpha"] |> expectRender "alpha"

    it "joinWords multi-element array uses single spaces" \_ -> do
      Layout.joinWords [Layout.text "alpha", Layout.text "beta", Layout.text "gamma"]
        |> expectRender "alpha beta gamma"

    it "stack empty array returns empty output" \_ -> do
      Layout.stack [] |> expectRender ""

    it "stack single-element array returns element" \_ -> do
      Layout.stack [Layout.text "a"] |> expectRender "a"

    it "stack multi-element array renders one per line" \_ -> do
      Layout.stack [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a\nb\nc"

    it "joinParagraph empty array returns empty output" \_ -> do
      Layout.joinParagraph [] |> expectRender ""

    it "joinParagraph single-element array returns element" \_ -> do
      Layout.joinParagraph [Layout.text "aa"] |> expectRender "aa"

    it "joinParagraph multi-element array fills then wraps" \_ -> do
      Layout.joinParagraph [Layout.text "aa", Layout.text "bb", Layout.text "cc"]
        |> expectRenderWithWidth 5 "aa bb\ncc"

    it "joinAdaptive empty array returns empty output" \_ -> do
      Layout.joinAdaptive [] |> expectRender ""

    it "joinAdaptive single-element array returns element" \_ -> do
      Layout.joinAdaptive [Layout.text "aa"] |> expectRender "aa"

    it "joinAdaptive multi-element array breaks when width is narrow" \_ -> do
      Layout.joinAdaptive [Layout.text "aa", Layout.text "bb", Layout.text "cc"]
        |> expectRenderWithWidth 5 "aa\nbb\ncc"

    it "joinTight empty array returns empty output" \_ -> do
      Layout.joinTight [] |> expectRender ""

    it "joinTight single-element array returns element" \_ -> do
      Layout.joinTight [Layout.text "a"] |> expectRender "a"

    it "joinTight multi-element array concatenates without separators" \_ -> do
      Layout.joinTight [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "abc"

    it "stackTight empty array returns empty output" \_ -> do
      Layout.stackTight [] |> expectRender ""

    it "stackTight single-element array returns element" \_ -> do
      Layout.stackTight [Layout.text "a"] |> expectRender "a"

    it "stackTight multi-element array renders one per line" \_ -> do
      Layout.stackTight [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a\nb\nc"

    it "joinDenseParagraph empty array returns empty output" \_ -> do
      Layout.joinDenseParagraph [] |> expectRender ""

    it "joinDenseParagraph single-element array returns element" \_ -> do
      Layout.joinDenseParagraph [Layout.text "ab"] |> expectRender "ab"

    it "joinDenseParagraph multi-element array packs without spaces" \_ -> do
      Layout.joinDenseParagraph [Layout.text "ab", Layout.text "cd", Layout.text "ef"]
        |> expectRenderWithWidth 4 "abcd\nef"

    it "joinAdaptiveTight empty array returns empty output" \_ -> do
      Layout.joinAdaptiveTight [] |> expectRender ""

    it "joinAdaptiveTight single-element array returns element" \_ -> do
      Layout.joinAdaptiveTight [Layout.text "ab"] |> expectRender "ab"

    it "joinAdaptiveTight multi-element array breaks when narrow" \_ -> do
      Layout.joinAdaptiveTight [Layout.text "ab", Layout.text "cd", Layout.text "ef"]
        |> expectRenderWithWidth 4 "ab\ncd\nef"

    it "addBetween empty array returns empty array" \_ -> do
      Layout.joinTight (Layout.addBetween (Layout.text ", ") [])
        |> expectRender ""

    it "addBetween single-element array adds nothing" \_ -> do
      Layout.joinTight (Layout.addBetween (Layout.text ", ") [Layout.text "a"])
        |> expectRender "a"

    it "addBetween multi-element array inserts separator between neighbors" \_ -> do
      Layout.joinTight
        (Layout.addBetween (Layout.text ", ") [Layout.text "a", Layout.text "b", Layout.text "c"])
        |> expectRender "a, b, c"

    it "commaSeparated empty array returns empty output" \_ -> do
      Layout.commaSeparated [] |> expectRender ""

    it "commaSeparated single-element array returns element" \_ -> do
      Layout.commaSeparated [Layout.text "a"] |> expectRender "a"

    it "commaSeparated multi-element array uses comma-space" \_ -> do
      Layout.commaSeparated [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a, b, c"

    it "semicolonSeparated empty array returns empty output" \_ -> do
      Layout.semicolonSeparated [] |> expectRender ""

    it "semicolonSeparated single-element array returns element" \_ -> do
      Layout.semicolonSeparated [Layout.text "a"] |> expectRender "a"

    it "semicolonSeparated multi-element array uses semicolon-space" \_ -> do
      Layout.semicolonSeparated [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a; b; c"

    it "pipeSeparated empty array returns empty output" \_ -> do
      Layout.pipeSeparated [] |> expectRender ""

    it "pipeSeparated single-element array returns element" \_ -> do
      Layout.pipeSeparated [Layout.text "a"] |> expectRender "a"

    it "pipeSeparated multi-element array uses pipe-space" \_ -> do
      Layout.pipeSeparated [Layout.text "a", Layout.text "b", Layout.text "c"]
        |> expectRender "a | b | c"

  describe "Layout.Delimiters" do
    it "wrapBetween handles empty content" \_ -> do
      Layout.wrapBetween (Layout.text "[") (Layout.text "]") Layout.empty
        |> expectRender "[]"

    it "wrapBetween handles single content" \_ -> do
      Layout.wrapBetween (Layout.text "[") (Layout.text "]") (Layout.text "x")
        |> expectRender "[x]"

    it "wrapBetween handles nested delimiters" \_ -> do
      Layout.wrapBetween
        (Layout.text "(")
        (Layout.text ")")
        (Layout.wrapBetween (Layout.text "[") (Layout.text "]") (Layout.text "x"))
        |> expectRender "([x])"

    it "asList handles empty array" \_ -> do
      Layout.asList [] |> expectRender "[]"

    it "asList handles single element" \_ -> do
      Layout.asList [Layout.text "x"] |> expectRender "[x]"

    it "asList handles nested values" \_ -> do
      Layout.asList [Layout.asList [Layout.text "x"], Layout.text "y"]
        |> expectRender "[[x], y]"

    it "asTuple handles empty array" \_ -> do
      Layout.asTuple [] |> expectRender "()"

    it "asTuple handles single element" \_ -> do
      Layout.asTuple [Layout.text "x"] |> expectRender "(x)"

    it "asTuple handles nested values" \_ -> do
      Layout.asTuple [Layout.asTuple [Layout.text "x"], Layout.text "y"]
        |> expectRender "((x), y)"

    it "inParens handles empty content" \_ -> do
      Layout.inParens Layout.empty |> expectRender "()"

    it "inParens handles single content" \_ -> do
      Layout.inParens (Layout.text "x") |> expectRender "(x)"

    it "inParens handles nested delimiter content" \_ -> do
      Layout.inParens (Layout.inBrackets (Layout.text "x"))
        |> expectRender "([x])"

    it "inBrackets handles empty content" \_ -> do
      Layout.inBrackets Layout.empty |> expectRender "[]"

    it "inBrackets handles single content" \_ -> do
      Layout.inBrackets (Layout.text "x") |> expectRender "[x]"

    it "inBrackets handles nested delimiter content" \_ -> do
      Layout.inBrackets (Layout.inParens (Layout.text "x"))
        |> expectRender "[(x)]"

    it "inBraces handles empty content" \_ -> do
      Layout.inBraces Layout.empty |> expectRender "{}"

    it "inBraces handles single content" \_ -> do
      Layout.inBraces (Layout.text "x") |> expectRender "{x}"

    it "inBraces handles nested delimiter content" \_ -> do
      Layout.inBraces (Layout.inBrackets (Layout.text "x"))
        |> expectRender "{[x]}"

    it "inAngles handles empty content" \_ -> do
      Layout.inAngles Layout.empty |> expectRender "<>"

    it "inAngles handles single content" \_ -> do
      Layout.inAngles (Layout.text "x") |> expectRender "<x>"

    it "inAngles handles nested delimiter content" \_ -> do
      Layout.inAngles (Layout.inParens (Layout.text "x"))
        |> expectRender "<(x)>"

    it "inSingleQuotes handles empty content" \_ -> do
      Layout.inSingleQuotes Layout.empty |> expectRender "''"

    it "inSingleQuotes handles single content" \_ -> do
      Layout.inSingleQuotes (Layout.text "x") |> expectRender "'x'"

    it "inSingleQuotes handles nested delimiter content" \_ -> do
      Layout.inSingleQuotes (Layout.inDoubleQuotes (Layout.text "x"))
        |> expectRender "'\"x\"'"

    it "inDoubleQuotes handles empty content" \_ -> do
      Layout.inDoubleQuotes Layout.empty |> expectRender "\"\""

    it "inDoubleQuotes handles single content" \_ -> do
      Layout.inDoubleQuotes (Layout.text "x") |> expectRender "\"x\""

    it "inDoubleQuotes handles nested delimiter content" \_ -> do
      Layout.inDoubleQuotes (Layout.inSingleQuotes (Layout.text "x"))
        |> expectRender "\"'x'\""

  describe "Layout.Control" do
    it "trySingleLine keeps short doc on one line" \_ -> do
      Layout.trySingleLine shortLineDoc |> expectRender "a b"

    it "trySingleLine breaks long doc when width exceeded" \_ -> do
      let doc =
            Layout.trySingleLine
              ( Layout.joinTight
                  [ Layout.text (Text.repeat 12 "a")
                  , Layout.line
                  , Layout.text (Text.repeat 12 "b")
                  ]
              )
      doc |> expectRenderWithWidth 10 ([fmt|#{Text.repeat 12 "a"}\n#{Text.repeat 12 "b"}|])

    it "trySingleLine is identity for short docs" \_ -> do
      let first = Layout.text "x"
      let second = Layout.joinWords [Layout.text "alpha", Layout.text "beta"]
      let third = Layout.asTuple [Layout.text "a", Layout.text "b"]
      Layout.trySingleLine first |> expectSameRender first
      Layout.trySingleLine second |> expectSameRender second
      Layout.trySingleLine third |> expectSameRender third

    it "chooseWhenSingleLine uses flat branch in single-line mode" \_ -> do
      Layout.trySingleLine (Layout.chooseWhenSingleLine (Layout.text "flat") (Layout.text "broken"))
        |> expectRender "flat"

    it "chooseWhenSingleLine uses broken branch when layout breaks" \_ -> do
      Layout.trySingleLine
        (Layout.joinTight [Layout.text "x", Layout.line, Layout.chooseWhenSingleLine (Layout.text "flat") (Layout.text "broken")])
        |> expectRenderWithWidth 1 "x\nbroken"

    it "chooseWhenSingleLine remains deterministic in compact mode" \_ -> do
      Layout.renderCompact (Layout.chooseWhenSingleLine (Layout.text "flat") (Layout.text "broken"))
        |> shouldBe "flat"

    it "indent 0 is identity" \_ -> do
      Layout.indent 0 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "indent clamps upper bound at 10000" \_ -> do
      Layout.indent 10000 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender ([fmt|a\n#{Text.repeat 10000 " "}b|])

    it "indent clamps negative values to 0" \_ -> do
      Layout.indent (-3) (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "indentRelative 0 is identity" \_ -> do
      Layout.indentRelative 0 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "indentRelative clamps upper bound at 10000" \_ -> do
      Layout.indentRelative 10000 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender ([fmt|a\n#{Text.repeat 10000 " "}b|])

    it "indentRelative clamps negative values to 0" \_ -> do
      Layout.indentRelative (-8) (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "alignToCurrentColumn aligns wrapped lines to current column" \_ -> do
      Layout.joinTight
        [ Layout.text "ab "
        , Layout.alignToCurrentColumn (Layout.joinTight [Layout.text "x", Layout.hardLine, Layout.text "y"])
        ]
        |> expectRender "ab x\n   y"

    it "alignToCurrentColumn is no-op for single-line docs" \_ -> do
      Layout.alignToCurrentColumn (Layout.text "x") |> expectRender "x"

    it "alignToCurrentColumn handles empty docs" \_ -> do
      Layout.alignToCurrentColumn Layout.empty |> expectRender ""

    it "hangingIndent indents all lines except first" \_ -> do
      Layout.hangingIndent 2 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\n  b"

    it "hangingIndent clamps upper bound at 10000" \_ -> do
      Layout.hangingIndent 10000 (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender ([fmt|a\n#{Text.repeat 10000 " "}b|])

    it "hangingIndent clamps negative values to 0" \_ -> do
      Layout.hangingIndent (-1) (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> expectRender "a\nb"

    it "padToWidth pads shorter content with trailing spaces" \_ -> do
      Layout.padToWidth 5 (Layout.text "ab") |> expectRender "ab   "

    it "padToWidth clamps upper bound at 10000" \_ -> do
      let rendered = Layout.render (Layout.padToWidth 10000 (Layout.text "ab"))
      rendered |> Text.length |> shouldBe 10000
      rendered |> Text.startsWith "ab" |> shouldBe True
      rendered |> Text.dropLeft 2 |> shouldBe (Text.repeat 9998 " ")

    it "padToWidth clamps negative values to 0" \_ -> do
      Layout.padToWidth (-3) (Layout.text "ab") |> expectRender "ab"

    it "padOrBreakAt pads when content shorter than width" \_ -> do
      Layout.joinWords [Layout.padOrBreakAt 5 (Layout.text "ab"), Layout.text "Z"]
        |> expectRender "ab    Z"

    it "padOrBreakAt breaks when content exceeds width" \_ -> do
      Layout.joinWords [Layout.padOrBreakAt 5 (Layout.text "abcdef"), Layout.text "Z"]
        |> expectRenderWithWidth 6 "abcdef\n     Z"

    it "padOrBreakAt clamps negative values to 0" \_ -> do
      let negative = Layout.render (Layout.padOrBreakAt (-5) (Layout.text "ab"))
      let zeroed = Layout.render (Layout.padOrBreakAt 0 (Layout.text "ab"))
      negative |> shouldBe zeroed

    it "mergeAdjacentBreaks keeps simple docs unchanged" \_ -> do
      Layout.mergeAdjacentBreaks 0 (Layout.text "abc") |> expectRender "abc"

    it "mergeAdjacentBreaks clamps negative depth to 0" \_ -> do
      let negative = Layout.render (Layout.mergeAdjacentBreaks (-1) breakDoc)
      let zeroed = Layout.render (Layout.mergeAdjacentBreaks 0 breakDoc)
      negative |> shouldBe zeroed

    it "mergeAdjacentBreaks clamps large depth to 10000" \_ -> do
      let large = Layout.render (Layout.mergeAdjacentBreaks 10001 breakDoc)
      let capped = Layout.render (Layout.mergeAdjacentBreaks 10000 breakDoc)
      large |> shouldBe capped

  describe "Layout.Annotations" do
    it "withAnnotation does not change rendering" \_ -> do
      Layout.withAnnotation ("tag" :: Text) (Layout.text "x") |> expectRender "x"

    it "withAnnotation and withoutAnnotations round-trip" \_ -> do
      Layout.withoutAnnotations (Layout.withAnnotation ("tag" :: Text) (Layout.text "x"))
        |> expectRender "x"

    it "nested withAnnotation preserves rendering" \_ -> do
      Layout.withAnnotation ("outer" :: Text) (Layout.withAnnotation ("inner" :: Text) (Layout.text "x"))
        |> expectRender "x"

    it "withoutAnnotations preserves rendering" \_ -> do
      let doc = Layout.withAnnotation ("tag" :: Text) (Layout.joinWords [Layout.text "x", Layout.text "y"])
      Layout.withoutAnnotations doc |> expectSameRender doc

    it "withoutAnnotations is idempotent" \_ -> do
      let annotatedDoc = Layout.withAnnotation ("tag" :: Text) (Layout.text "x")
      let once = Layout.withoutAnnotations annotatedDoc
      let twice = Layout.withoutAnnotations once
      twice |> expectSameRender once

    it "withoutAnnotations handles plain unannotated docs" \_ -> do
      Layout.withoutAnnotations (Layout.text "plain") |> expectRender "plain"

    it "mapAnnotations identity preserves rendering" \_ -> do
      let doc = Layout.withAnnotation ("tag" :: Text) (Layout.joinWords [Layout.text "x", Layout.text "y"])
      Layout.mapAnnotations identity doc |> expectSameRender doc

    it "mapAnnotations one-to-one transform preserves rendering" \_ -> do
      Layout.mapAnnotations (\ann -> Text.append ann "-x") (Layout.withAnnotation ("tag" :: Text) (Layout.text "x"))
        |> expectRender "x"

    it "mapAnnotations composes over nested annotations" \_ -> do
      Layout.mapAnnotations Text.toUpper
        (Layout.withAnnotation ("tag" :: Text) (Layout.joinWords [Layout.text "x", Layout.text "y"]))
        |> expectRender "x y"

    it "editAnnotations one-to-many transform preserves rendering" \_ -> do
      Layout.editAnnotations (\ann -> [ann, ann]) (Layout.withAnnotation ("tag" :: Text) (Layout.text "x"))
        |> expectRender "x"

    it "editAnnotations can remove annotations" \_ -> do
      Layout.editAnnotations (\_ -> []) (Layout.withAnnotation ("tag" :: Text) (Layout.text "x"))
        |> expectRender "x"

    it "editAnnotations works on nested annotated docs" \_ -> do
      Layout.editAnnotations
        (\ann -> [ann, ann])
        ( Layout.joinWords
            [ Layout.withAnnotation ("a" :: Text) (Layout.text "x")
            , Layout.withAnnotation ("b" :: Text) (Layout.text "y")
            ]
        )
        |> expectRender "x y"

  describe "Layout.Traits" do
    it "appendable concatenates like append" \_ -> do
      Layout.render ((Layout.text "a") ++ (Layout.text "b"))
        |> shouldBe "ab"

    it "appendable is associative" \_ -> do
      let a = Layout.text "a"
      let b = Layout.text "b"
      let c = Layout.text "c"
      Layout.render ((a ++ b) ++ c)
        |> shouldBe (Layout.render (a ++ (b ++ c)))

    it "appendable handles empty identity operand" \_ -> do
      Layout.render ((Layout.text "a") ++ (Combinable.empty :: Blueprint ann))
        |> shouldBe "a"

    it "mempty renders as empty" \_ -> do
      Layout.render (Combinable.empty :: Blueprint ann)
        |> shouldBe ""

    it "mempty left identity" \_ -> do
      let doc = Layout.text "value"
      Layout.render ((Combinable.empty :: Blueprint ann) ++ doc)
        |> shouldBe (Layout.render doc)

    it "mempty right identity" \_ -> do
      let doc = Layout.text "value"
      Layout.render (doc ++ (Combinable.empty :: Blueprint ann))
        |> shouldBe (Layout.render doc)

    it "map identity preserves render" \_ -> do
      let annotatedDoc = Layout.withAnnotation ("tag" :: Text) (Layout.text "x")
      Layout.render (Mappable.map identity annotatedDoc)
        |> shouldBe (Layout.render annotatedDoc)

    it "map changes annotation type only" \_ -> do
      Layout.render (Mappable.map Text.length (Layout.withAnnotation ("tag" :: Text) (Layout.text "x")))
        |> shouldBe "x"

    it "map works for nested annotations" \_ -> do
      Layout.render
        (Mappable.map Text.toUpper (Layout.withAnnotation ("a" :: Text) (Layout.joinWords [Layout.text "x", Layout.text "y"])))
        |> shouldBe "x y"

    it "string literal builds blueprint" \_ -> do
      Layout.render ("hello" :: Blueprint ann)
        |> shouldBe "hello"

    it "empty string literal builds empty blueprint" \_ -> do
      Layout.render ("" :: Blueprint ann)
        |> shouldBe ""

    it "string literal preserves punctuation" \_ -> do
      Layout.render ("a,b;c" :: Blueprint ann)
        |> shouldBe "a,b;c"

    it "toBlueprint Text renders verbatim" \_ -> do
      Layout.render (toBlueprint ("neo" :: Text))
        |> shouldBe "neo"

    it "toBlueprint Int renders decimal" \_ -> do
      Layout.render (toBlueprint (42 :: Int))
        |> shouldBe "42"

    it "toBlueprint Float renders decimal" \_ -> do
      Layout.render (toBlueprint (3.25 :: Float))
        |> shouldBe "3.25"

    it "toBlueprint Bool renders constructor name" \_ -> do
      Layout.render (toBlueprint False)
        |> shouldBe "False"

    it "toBlueprint Maybe renders Nothing and Just" \_ -> do
      Layout.render (toBlueprint (Nothing :: Maybe Int))
        |> shouldBe "Nothing"
      Layout.render (toBlueprint (Just (5 :: Int)))
        |> shouldBe "Just 5"

    it "toBlueprint Result renders Err and Ok" \_ -> do
      Layout.render (toBlueprint (Err ("e" :: Text) :: Result Text Int))
        |> shouldBe "Err e"
      Layout.render (toBlueprint (Ok (5 :: Int) :: Result Text Int))
        |> shouldBe "Ok 5"
