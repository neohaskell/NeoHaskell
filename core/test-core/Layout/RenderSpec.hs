module Layout.RenderSpec where

import Array qualified
import Core
import Layout (Blueprint, WrapMode (..))
import Layout qualified
import Test
import Text qualified


shortLineDoc :: Blueprint ann
shortLineDoc = Layout.joinTight [Layout.text "a", Layout.line, Layout.text "b"]


wideBreakDoc :: Blueprint ann
wideBreakDoc =
  Layout.trySingleLine
    ( Layout.joinTight
        [ Layout.text (Text.repeat 60 "x")
        , Layout.line
        , Layout.text (Text.repeat 40 "y")
        ]
    )


modeProbeDoc :: Blueprint ann
modeProbeDoc =
  Layout.trySingleLine
    ( Layout.joinWords
        [ Layout.text "start"
        , Layout.text (Text.repeat 24 "a")
        , Layout.line
        , Layout.text (Text.repeat 24 "b")
        , Layout.line
        , Layout.text (Text.repeat 24 "c")
        , Layout.text "end"
        ]
    )


spec :: Spec Unit
spec = do
  describe "Layout.Rendering" do
    it "defaultRenderOptions matches render behavior" \_ -> do
      Layout.renderWith Layout.defaultRenderOptions wideBreakDoc
        |> shouldBe (Layout.render wideBreakDoc)

    it "defaultRenderOptions uses Balanced mode" \_ -> do
      Layout.renderWith Layout.defaultRenderOptions modeProbeDoc
        |> shouldBe (Layout.renderBalanced modeProbeDoc)

    it "defaultRenderOptions uses width 80" \_ -> do
      Layout.renderWith Layout.defaultRenderOptions wideBreakDoc
        |> shouldBe ([fmt|#{Text.repeat 60 "x"}\n#{Text.repeat 40 "y"}|])

    it "withMaxWidth changes wrapping width" \_ -> do
      Layout.renderWith
        (Layout.withMaxWidth 10 Layout.defaultRenderOptions)
        (Layout.joinAdaptive [Layout.text "aaaa", Layout.text "bbbb", Layout.text "cccc"])
        |> shouldBe "aaaa\nbbbb\ncccc"

    it "withMaxWidth clamps negative values to 0" \_ -> do
      Layout.renderWith
        (Layout.withMaxWidth (-1) Layout.defaultRenderOptions)
        wideBreakDoc
        |> shouldBe
          ( Layout.renderWith
              (Layout.withMaxWidth 0 Layout.defaultRenderOptions)
              wideBreakDoc
          )

    it "withMaxWidth clamps values above 10000" \_ -> do
      Layout.renderWith
        (Layout.withMaxWidth 99999 Layout.defaultRenderOptions)
        wideBreakDoc
        |> shouldBe
          ( Layout.renderWith
              (Layout.withMaxWidth 10000 Layout.defaultRenderOptions)
              wideBreakDoc
          )

    it "withRibbonFraction clamps 0.0 to 1.0" \_ -> do
      Layout.renderWith
        (Layout.withRibbonFraction 0.0 Layout.defaultRenderOptions)
        modeProbeDoc
        |> shouldBe
          ( Layout.renderWith
              (Layout.withRibbonFraction 1.0 Layout.defaultRenderOptions)
              modeProbeDoc
          )

    it "withRibbonFraction clamps values greater than 1.0" \_ -> do
      Layout.renderWith
        (Layout.withRibbonFraction 1.5 Layout.defaultRenderOptions)
        modeProbeDoc
        |> shouldBe
          ( Layout.renderWith
              (Layout.withRibbonFraction 1.0 Layout.defaultRenderOptions)
              modeProbeDoc
          )

    it "withRibbonFraction clamps NaN and Infinity to 1.0" \_ -> do
      let nanOutput =
            Layout.renderWith
              (Layout.withRibbonFraction (0.0 / 0.0) Layout.defaultRenderOptions)
              modeProbeDoc
      let infOutput =
            Layout.renderWith
              (Layout.withRibbonFraction (1.0 / 0.0) Layout.defaultRenderOptions)
              modeProbeDoc
      let oneOutput =
            Layout.renderWith
              (Layout.withRibbonFraction 1.0 Layout.defaultRenderOptions)
              modeProbeDoc
      nanOutput |> shouldBe oneOutput
      infOutput |> shouldBe oneOutput

    it "withWrapMode sets Balanced mode" \_ -> do
      Layout.renderWith
        (Layout.withWrapMode Balanced Layout.defaultRenderOptions)
        modeProbeDoc
        |> shouldBe (Layout.renderBalanced modeProbeDoc)

    it "withWrapMode sets Fast mode" \_ -> do
      Layout.renderWith
        (Layout.withWrapMode Fast Layout.defaultRenderOptions)
        modeProbeDoc
        |> shouldBe (Layout.renderFast modeProbeDoc)

    it "withWrapMode sets Compact mode and modes differ on same probe" \_ -> do
      let balanced =
            Layout.renderWith (Layout.withWrapMode Balanced Layout.defaultRenderOptions) modeProbeDoc
      let fast =
            Layout.renderWith (Layout.withWrapMode Fast Layout.defaultRenderOptions) modeProbeDoc
      let compact =
            Layout.renderWith (Layout.withWrapMode Compact Layout.defaultRenderOptions) modeProbeDoc
      (balanced != fast) |> shouldBe True
      (fast != compact) |> shouldBe True
      (balanced != compact) |> shouldBe True

    it "render uses default options" \_ -> do
      Layout.render wideBreakDoc
        |> shouldBe (Layout.renderWith Layout.defaultRenderOptions wideBreakDoc)

    it "render treats line as space in flat fit" \_ -> do
      Layout.render (Layout.trySingleLine shortLineDoc)
        |> shouldBe "a b"

    it "render breaks when grouped content exceeds width" \_ -> do
      Layout.render wideBreakDoc
        |> shouldBe ([fmt|#{Text.repeat 60 "x"}\n#{Text.repeat 40 "y"}|])

    it "renderWithWidth applies custom width" \_ -> do
      Layout.renderWithWidth
        10
        ( Layout.trySingleLine
            ( Layout.joinTight
                [ Layout.text (Text.repeat 12 "a")
                , Layout.line
                , Layout.text (Text.repeat 12 "b")
                ]
            )
        )
        |> shouldBe ([fmt|#{Text.repeat 12 "a"}\n#{Text.repeat 12 "b"}|])

    it "renderWithWidth clamps negative width to 0" \_ -> do
      Layout.renderWithWidth (-1) wideBreakDoc
        |> shouldBe (Layout.renderWithWidth 0 wideBreakDoc)

    it "renderWithWidth clamps above 10000" \_ -> do
      Layout.renderWithWidth 10001 wideBreakDoc
        |> shouldBe (Layout.renderWithWidth 10000 wideBreakDoc)

    it "renderWith respects all options fields" \_ -> do
      Layout.renderWith
        (Layout.withWrapMode Compact (Layout.withMaxWidth 5 (Layout.withRibbonFraction 0.7 Layout.defaultRenderOptions)))
        modeProbeDoc
        |> shouldBe (Layout.renderCompact modeProbeDoc)

    it "renderWith supports integration diagnostic example" \_ -> do
      Layout.renderWithWidth
        80
        ( Layout.stackTight
            [ Layout.text "Parse error:"
            , Layout.hardLine
            , Layout.indent 4 (Layout.stackTight [Layout.text "let =", Layout.text "    ^"])
            ]
        )
        |> shouldBe "Parse error:\n\n    let =\n        ^"

    it "renderWith mode comparison on same trySingleLine+line document" \_ -> do
      let balanced = Layout.renderWith (Layout.withWrapMode Balanced Layout.defaultRenderOptions) modeProbeDoc
      let fast = Layout.renderWith (Layout.withWrapMode Fast Layout.defaultRenderOptions) modeProbeDoc
      let compact = Layout.renderWith (Layout.withWrapMode Compact Layout.defaultRenderOptions) modeProbeDoc
      (balanced != fast) |> shouldBe True
      (fast != compact) |> shouldBe True
      (balanced != compact) |> shouldBe True

    it "renderBalanced equals renderWith Balanced" \_ -> do
      Layout.renderBalanced modeProbeDoc
        |> shouldBe (Layout.renderWith (Layout.withWrapMode Balanced Layout.defaultRenderOptions) modeProbeDoc)

    it "renderBalanced differs from renderCompact on break-sensitive doc" \_ -> do
      (Layout.renderBalanced wideBreakDoc != Layout.renderCompact wideBreakDoc)
        |> shouldBe True

    it "renderBalanced integration type-signature output" \_ -> do
      Layout.renderBalanced
        ( Layout.joinWords
            [ Layout.text "f"
            , Layout.text "::"
            , Layout.joinTight
                ( Layout.addBetween
                    (Layout.text " -> ")
                    [Layout.text "Int", Layout.text "Text", Layout.text "Bool"]
                )
            ]
        )
        |> shouldBe "f :: Int -> Text -> Bool"

    it "renderFast equals renderWith Fast" \_ -> do
      Layout.renderFast modeProbeDoc
        |> shouldBe (Layout.renderWith (Layout.withWrapMode Fast Layout.defaultRenderOptions) modeProbeDoc)

    it "renderFast differs from renderBalanced on probe" \_ -> do
      (Layout.renderFast modeProbeDoc != Layout.renderBalanced modeProbeDoc)
        |> shouldBe True

    it "renderFast preserves token order" \_ -> do
      let rendered = Layout.renderFast modeProbeDoc
      let startIndex = Text.indices "start" rendered |> Array.get 0
      let aIndex = Text.indices (Text.repeat 24 "a") rendered |> Array.get 0
      let bIndex = Text.indices (Text.repeat 24 "b") rendered |> Array.get 0
      let cIndex = Text.indices (Text.repeat 24 "c") rendered |> Array.get 0
      let endIndex = Text.indices "end" rendered |> Array.get 0
      case (startIndex, aIndex, bIndex, cIndex, endIndex) of
        (Just si, Just ai, Just bi, Just ci, Just ei) -> do
          (si < ai) |> shouldBe True
          (ai < bi) |> shouldBe True
          (bi < ci) |> shouldBe True
          (ci < ei) |> shouldBe True
        _ -> do
          fail "Expected all probe tokens to be present in renderFast output"

    it "renderCompact equals renderWith Compact" \_ -> do
      Layout.renderCompact modeProbeDoc
        |> shouldBe (Layout.renderWith (Layout.withWrapMode Compact Layout.defaultRenderOptions) modeProbeDoc)

    it "renderCompact suppresses soft and normal line breaks" \_ -> do
      Layout.renderCompact
        (Layout.trySingleLine (Layout.joinTight [Layout.text "a", Layout.line, Layout.text "b", Layout.softLine, Layout.text "c"]))
        |> shouldBe "a b c"

    it "renderCompact only includes newlines from hardLine or blankLine" \_ -> do
      Layout.renderCompact (Layout.joinWords [Layout.text "alpha", Layout.text "beta", Layout.text "gamma"])
        |> Text.contains "\n"
        |> shouldBe False
      Layout.renderCompact (Layout.joinTight [Layout.text "a", Layout.hardLine, Layout.text "b"])
        |> shouldBe "a\nb"
      Layout.renderCompact (Layout.joinTight [Layout.text "a", Layout.blankLine, Layout.text "b"])
        |> shouldBe "a\n\nb"
