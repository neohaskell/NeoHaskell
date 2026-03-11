module Parser.WhitespaceSpec where

import Core
import Parser qualified
import Result (Result (..))
import Result qualified
import Test


spec :: Spec Unit
spec = do
  -- ===========================================================
  -- 3.1 Whitespace, Comments, and Lexical Helpers
  -- ===========================================================
  describe "Parser.spaces" do
    it "succeeds on empty input (zero or more — does not fail)" \_ -> do
      Parser.run Parser.spaces "" |> shouldBe (Result.Ok unit)

    it "consumes space characters" \_ -> do
      let p = Parser.keepRight Parser.spaces (Parser.char 'a')
      Parser.run p " a" |> shouldBe (Result.Ok 'a')

    it "consumes tab characters" \_ -> do
      let p = Parser.keepRight Parser.spaces (Parser.char 'a')
      Parser.run p "\ta" |> shouldBe (Result.Ok 'a')

    it "consumes newline characters" \_ -> do
      let p = Parser.keepRight Parser.spaces (Parser.char 'a')
      Parser.run p "\na" |> shouldBe (Result.Ok 'a')

    it "consumes mixed whitespace" \_ -> do
      let p = Parser.keepRight Parser.spaces (Parser.char 'a')
      Parser.run p " \t\na" |> shouldBe (Result.Ok 'a')

    it "does not consume non-whitespace" \_ -> do
      let p = Parser.pair Parser.spaces (Parser.char 'a')
      Parser.run p "a" |> shouldBe (Result.Ok (unit, 'a'))

  describe "Parser.whitespace" do
    it "consumes one or more whitespace characters" \_ -> do
      Parser.run Parser.whitespace " \t\n" |> shouldBe (Result.Ok unit)

    it "fails on empty input" \_ -> do
      Parser.run Parser.whitespace "" |> shouldSatisfy Result.isErr

    it "fails if first character is not whitespace" \_ -> do
      Parser.run Parser.whitespace "a" |> shouldSatisfy Result.isErr

  describe "Parser.token" do
    it "runs parser and consumes trailing spaces" \_ -> do
      Parser.run (Parser.token (Parser.text "let")) "let   "
        |> shouldBe (Result.Ok "let")

    it "does not consume leading spaces" \_ -> do
      Parser.run (Parser.token (Parser.char 'a')) " a"
        |> shouldSatisfy Result.isErr

    it "returns the parser's result" \_ -> do
      Parser.run (Parser.token (Parser.char 'ñ')) "ñ  "
        |> shouldBe (Result.Ok 'ñ')

  describe "Parser.tokenWith" do
    it "uses custom space consumer after the parser" \_ -> do
      let p = Parser.tokenWith (Parser.lineComment "#") (Parser.text "x")
      Parser.run p "x#note\n" |> shouldBe (Result.Ok "x")

    it "example: skip # comments as whitespace" \_ -> do
      let p = Parser.tokenWith (Parser.lineComment "#") (Parser.text "x")
      Parser.run p "x#note\n" |> shouldBe (Result.Ok "x")

    it "custom consumer can enforce at least one whitespace" \_ -> do
      let p = Parser.tokenWith Parser.whitespace (Parser.text "x")
      Parser.run p "x" |> shouldSatisfy Result.isErr

    it "propagates parser failure" \_ -> do
      let p = Parser.tokenWith Parser.spaces (Parser.char 'x')
      Parser.run p "y" |> shouldSatisfy Result.isErr

  describe "Parser.symbol" do
    it "matches text and consumes trailing spaces" \_ -> do
      Parser.run (Parser.symbol "let") "let   " |> shouldBe (Result.Ok "let")

    it "fails on mismatch" \_ -> do
      Parser.run (Parser.symbol "let") "lex" |> shouldSatisfy Result.isErr

    it "is case-sensitive" \_ -> do
      Parser.run (Parser.symbol "let") "LET" |> shouldSatisfy Result.isErr

    it "does not consume leading spaces" \_ -> do
      Parser.run (Parser.symbol "let") " let" |> shouldSatisfy Result.isErr

  describe "Parser.symbolIgnoringCase" do
    it "matches text ignoring case" \_ -> do
      Parser.run (Parser.symbolIgnoringCase "let") "LeT" |> shouldBe (Result.Ok "LeT")

    it "consumes trailing spaces" \_ -> do
      Parser.run (Parser.symbolIgnoringCase "let") "LET   " |> shouldBe (Result.Ok "LET")

    it "fails on non-matching token" \_ -> do
      Parser.run (Parser.symbolIgnoringCase "let") "lex" |> shouldSatisfy Result.isErr

  describe "Parser.symbolWith" do
    it "uses custom space consumer after the matched text" \_ -> do
      let p = Parser.symbolWith (Parser.lineComment "#") "let"
      Parser.run p "let#c\n" |> shouldBe (Result.Ok "let")

    it "uses custom consumer (whitespace)" \_ -> do
      let p = Parser.symbolWith Parser.whitespace "let"
      Parser.run p "let \t" |> shouldBe (Result.Ok "let")

    it "fails on text mismatch" \_ -> do
      let p = Parser.symbolWith Parser.spaces "let"
      Parser.run p "lex" |> shouldSatisfy Result.isErr

  describe "Parser.lineComment" do
    it "skips content from prefix to end of line" \_ -> do
      let p = Parser.keepRight (Parser.lineComment "#") (Parser.char 'x')
      Parser.run p "# hello\nx" |> shouldBe (Result.Ok 'x')

    it "skips comment up to EOF" \_ -> do
      Parser.run (Parser.lineComment "#") "# only comment"
        |> shouldBe (Result.Ok unit)

    it "fails when prefix not present" \_ -> do
      Parser.run (Parser.lineComment "#") "not-comment"
        |> shouldSatisfy Result.isErr

  describe "Parser.blockComment" do
    it "skips content between open and close markers" \_ -> do
      let p = Parser.keepRight (Parser.blockComment "/*" "*/") (Parser.char 'x')
      Parser.run p "/* hello */x" |> shouldBe (Result.Ok 'x')

    it "fails if close marker is missing" \_ -> do
      Parser.run (Parser.blockComment "/*" "*/") "/* unclosed"
        |> shouldSatisfy Result.isErr

    it "does NOT support nesting (inner markers are ignored)" \_ -> do
      let p = Parser.keepRight (Parser.blockComment "/*" "*/") (Parser.text "c */x")
      Parser.run p "/*a /*b*/c */x" |> shouldBe (Result.Ok "c */x")

  describe "Parser.blockCommentNested" do
    it "supports nested block comments" \_ -> do
      let p = Parser.keepRight (Parser.blockCommentNested "/*" "*/") (Parser.char 'x')
      Parser.run p "/* a /* b */ c */x" |> shouldBe (Result.Ok 'x')

    it "supports deeper nesting" \_ -> do
      let p = Parser.keepRight (Parser.blockCommentNested "/*" "*/") (Parser.char 'x')
      Parser.run p "/*1 /*2 /*3*/ 2*/ 1*/x" |> shouldBe (Result.Ok 'x')

    it "fails if matching close marker is missing" \_ -> do
      Parser.run (Parser.blockCommentNested "/*" "*/") "/* a /* b */"
        |> shouldSatisfy Result.isErr

  describe "whitespace mode side-by-side comparison" do
    it "Parser.char 'a' does NOT consume trailing spaces" \_ -> do
      -- Parse 'a' followed by space — second char must be space
      let p = Parser.pair (Parser.char 'a') Parser.space
      Parser.run p "a " |> shouldBe (Result.Ok ('a', ' '))

    it "Parser.token (Parser.char 'a') DOES consume trailing spaces" \_ -> do
      -- token consumes trailing spaces; next char after is non-space
      let p = Parser.keepRight (Parser.token (Parser.char 'a')) (Parser.char 'b')
      Parser.run p "a   b" |> shouldBe (Result.Ok 'b')

    it "Parser.text \"let\" does NOT consume trailing spaces" \_ -> do
      let p = Parser.pair (Parser.text "let") Parser.space
      Parser.run p "let " |> shouldBe (Result.Ok ("let", ' '))

    it "Parser.symbol \"let\" DOES consume trailing spaces" \_ -> do
      let p = Parser.keepRight (Parser.symbol "let") (Parser.char 'x')
      Parser.run p "let   x" |> shouldBe (Result.Ok 'x')
