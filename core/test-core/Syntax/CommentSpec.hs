module Syntax.CommentSpec (spec) where

import Core
import Parser (ParsePosition (..))
import Parser qualified
import Result qualified
import Syntax.Comment (Comment (..))
import Syntax.Comment qualified
import Test


spec :: Spec Unit
spec = do
  -- ===========================================================
  -- lineComment
  -- ===========================================================
  describe "lineComment" do
    it "captures content after //" \_ -> do
      let result = "// hello world" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { content = c }) ->
          c |> shouldBe "hello world"
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

    it "strips one leading space after //" \_ -> do
      let result = "// spaced" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { content = c }) ->
          c |> shouldBe "spaced"
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

    it "handles no space after //" \_ -> do
      let result = "//nospace" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { content = c }) ->
          c |> shouldBe "nospace"
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

    it "captures empty comment" \_ -> do
      let result = "//" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { content = c }) ->
          c |> shouldBe ""
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

    it "stops before newline" \_ -> do
      let result = "// hello\nmore" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { content = c }) ->
          c |> shouldBe "hello"
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

    it "captures source position" \_ -> do
      let result = "// pos" |> Parser.runPartial Syntax.Comment.lineComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (LineComment { position = pos }) -> do
          pos.line |> shouldBe 1
          pos.column |> shouldBe 1
        Ok other ->
          fail [fmt|Expected LineComment, got: #{toText other}|]

  -- ===========================================================
  -- blockComment
  -- ===========================================================
  describe "blockComment" do
    it "captures flat block comment" \_ -> do
      let result = "/* hello */" |> Parser.run Syntax.Comment.blockComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (BlockComment { content = c, depth = d }) -> do
          c |> shouldBe " hello "
          d |> shouldBe 1
        Ok other ->
          fail [fmt|Expected BlockComment, got: #{toText other}|]

    it "captures nested block comment" \_ -> do
      let result = "/* outer /* inner */ outer */" |> Parser.run Syntax.Comment.blockComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (BlockComment { content = c, depth = d }) -> do
          c |> shouldBe " outer /* inner */ outer "
          d |> shouldBe 2
        Ok other ->
          fail [fmt|Expected BlockComment, got: #{toText other}|]

    it "captures deeply nested block comment" \_ -> do
      let result = "/* a /* b /* c */ b */ a */" |> Parser.run Syntax.Comment.blockComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (BlockComment { content = c, depth = d }) -> do
          c |> shouldBe " a /* b /* c */ b */ a "
          d |> shouldBe 3
        Ok other ->
          fail [fmt|Expected BlockComment, got: #{toText other}|]

    it "captures empty block comment" \_ -> do
      let result = "/**/" |> Parser.run Syntax.Comment.blockComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (BlockComment { content = c, depth = d }) -> do
          c |> shouldBe ""
          d |> shouldBe 1
        Ok other ->
          fail [fmt|Expected BlockComment, got: #{toText other}|]

    it "captures source position" \_ -> do
      let result = "/* pos */" |> Parser.run Syntax.Comment.blockComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (BlockComment { position = pos }) -> do
          pos.line |> shouldBe 1
          pos.column |> shouldBe 1
        Ok other ->
          fail [fmt|Expected BlockComment, got: #{toText other}|]

  -- ===========================================================
  -- docComment
  -- ===========================================================
  describe "docComment" do
    it "captures single-line doc comment" \_ -> do
      let result = "/** Calculates distance. */" |> Parser.run Syntax.Comment.docComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (DocComment { content = c }) ->
          c |> shouldBe "Calculates distance."
        Ok other ->
          fail [fmt|Expected DocComment, got: #{toText other}|]

    it "strips leading * decoration from multi-line doc comment" \_ -> do
      let input = "/**\n * Line one.\n * Line two.\n */"
      let result = input |> Parser.run Syntax.Comment.docComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (DocComment { content = c }) ->
          c |> shouldBe "Line one.\nLine two."
        Ok other ->
          fail [fmt|Expected DocComment, got: #{toText other}|]

    it "handles doc comment without star decoration" \_ -> do
      let result = "/** plain text */" |> Parser.run Syntax.Comment.docComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (DocComment { content = c }) ->
          c |> shouldBe "plain text"
        Ok other ->
          fail [fmt|Expected DocComment, got: #{toText other}|]

    it "captures source position" \_ -> do
      let result = "/** doc */" |> Parser.run Syntax.Comment.docComment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (DocComment { position = pos }) -> do
          pos.line |> shouldBe 1
          pos.column |> shouldBe 1
        Ok other ->
          fail [fmt|Expected DocComment, got: #{toText other}|]

  -- ===========================================================
  -- comment (combined parser)
  -- ===========================================================
  describe "comment" do
    it "parses line comment" \_ -> do
      let result = "// hello" |> Parser.runPartial Syntax.Comment.comment
      result |> shouldSatisfy Result.isOk

    it "parses block comment" \_ -> do
      let result = "/* hello */" |> Parser.run Syntax.Comment.comment
      result |> shouldSatisfy Result.isOk

    it "parses doc comment before block comment (/** vs /*)" \_ -> do
      let result = "/** doc */" |> Parser.run Syntax.Comment.comment
      case result of
        Err err ->
          fail [fmt|Expected Ok, got error: #{Parser.formatError err}|]
        Ok (DocComment { content = c }) ->
          c |> shouldBe "doc"
        Ok other ->
          fail [fmt|Expected DocComment, got: #{toText other}|]

  -- ===========================================================
  -- toHaskell
  -- ===========================================================
  describe "toHaskell" do
    it "transpiles line comment to --" \_ -> do
      let commentNode =
            LineComment
              { position = ParsePosition { sourceName = "<test>", line = 1, column = 1, offset = 0 }
              , content  = "hello"
              }
      Syntax.Comment.toHaskell commentNode
        |> shouldBe "-- hello"

    it "transpiles flat block comment to {- -}" \_ -> do
      let commentNode =
            BlockComment
              { position = ParsePosition { sourceName = "<test>", line = 1, column = 1, offset = 0 }
              , content  = " world "
              , depth    = 1
              }
      Syntax.Comment.toHaskell commentNode
        |> shouldBe "{- world -}"

    it "transpiles nested block comment with marker substitution" \_ -> do
      let commentNode =
            BlockComment
              { position = ParsePosition { sourceName = "<test>", line = 1, column = 1, offset = 0 }
              , content  = " outer /* inner */ outer "
              , depth    = 2
              }
      Syntax.Comment.toHaskell commentNode
        |> shouldBe "{- outer {- inner -} outer -}"

    it "transpiles single-line doc comment to -- |" \_ -> do
      let commentNode =
            DocComment
              { position = ParsePosition { sourceName = "<test>", line = 1, column = 1, offset = 0 }
              , content  = "Calculates distance."
              }
      Syntax.Comment.toHaskell commentNode
        |> shouldBe "-- | Calculates distance."

    it "transpiles multi-line doc comment to multiple -- | lines" \_ -> do
      let commentNode =
            DocComment
              { position = ParsePosition { sourceName = "<test>", line = 1, column = 1, offset = 0 }
              , content  = "Line one.\nLine two."
              }
      Syntax.Comment.toHaskell commentNode
        |> shouldBe "-- | Line one.\n-- | Line two."
