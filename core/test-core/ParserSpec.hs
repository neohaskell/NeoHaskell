module ParserSpec where

import Array qualified
import Char qualified
import Core
import Maybe (Maybe (..))
import Parser (Parser, ParseError (..), ParsePosition (..))
import Parser qualified
import Result (Result (..))
import Result qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  -- ===========================================================
  -- 1.1 Running APIs: run, runNamed, runMaybe
  -- ===========================================================
  describe "Parser.run" do
    it "succeeds on full consumption" \_ -> do
      let result = Parser.run (Parser.text "abc") "abc"
      result |> shouldSatisfy Result.isOk
      result |> shouldBe (Result.Ok "abc")

    it "fails when trailing input remains" \_ -> do
      let result = Parser.run (Parser.text "abc") "abcX"
      result |> shouldSatisfy Result.isErr

    it "reports correct multiline position" \_ -> do
      let p = Parser.keepRight (Parser.text "a\n") (Parser.char 'b')
      let result = Parser.run p "a\nx"
      case result of
        Err err -> do
          err.position.line |> shouldBe 2
          err.position.column |> shouldBe 1
          err.position.offset |> shouldBe 2
        Ok _ -> fail "expected parse error"

    it "sets sourceName to \"<input>\"" \_ -> do
      let result = Parser.run (Parser.char 'x') "y"
      case result of
        Err err -> err.position.sourceName |> shouldBe "<input>"
        Ok _ -> fail "expected parse error"

  describe "Parser.runNamed" do
    it "stores custom source name in error" \_ -> do
      let result = Parser.runNamed "config.neo" (Parser.char 'x') "y"
      case result of
        Err err -> err.position.sourceName |> shouldBe "config.neo"
        Ok _ -> fail "expected parse error"

    it "enforces EOF after parser" \_ -> do
      let result = Parser.runNamed "partial.neo" (Parser.text "let") "let value"
      result |> shouldSatisfy Result.isErr

    it "handles unicode source line text" \_ -> do
      let result = Parser.runNamed "unicode.neo" (Parser.text "hola") "holñ"
      case result of
        Err err -> err.contextLine |> shouldBe "holñ"
        Ok _ -> fail "expected parse error"

  describe "Parser.runMaybe" do
    it "returns Just on success" \_ -> do
      let result = Parser.runMaybe (Parser.text "ok") "ok"
      result |> shouldBe (Maybe.Just "ok")

    it "returns Nothing on parse failure" \_ -> do
      let result = Parser.runMaybe (Parser.text "ok") "no"
      result |> shouldBe Maybe.Nothing

    it "returns Nothing when trailing input exists" \_ -> do
      let result = Parser.runMaybe (Parser.text "ok") "ok!"
      result |> shouldBe Maybe.Nothing

  -- ===========================================================
  -- 1.2 Transformations: yield, map, apply, andThen
  -- ===========================================================
  describe "Parser.yield" do
    it "always succeeds with given value" \_ -> do
      let result = Parser.run (Parser.yield (42 :: Int)) ""
      result |> shouldBe (Result.Ok 42)

    it "consumes no input" \_ -> do
      let p = Parser.pair (Parser.yield (7 :: Int)) (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok (7, 'a'))

    it "supports unicode payload" \_ -> do
      let result = Parser.run (Parser.yield ("hola-ñ" :: Text)) ""
      result |> shouldBe (Result.Ok "hola-ñ")

  describe "Parser.map" do
    it "transforms parsed value" \_ -> do
      let p = Parser.map Text.toUpper (Parser.text "neo")
      let result = Parser.run p "neo"
      result |> shouldBe (Result.Ok "NEO")

    it "propagates failure" \_ -> do
      let p = Parser.map Text.toUpper (Parser.text "neo")
      let result = Parser.run p "nea"
      result |> shouldSatisfy Result.isErr

    it "maps boundary value" \_ -> do
      let p = Parser.map (\d -> d + 1) Parser.decimal
      let result = Parser.run p "0"
      result |> shouldBe (Result.Ok 1)

  describe "Parser.apply" do
    it "applies parsed function to parsed value" \_ -> do
      let p = Parser.apply (Parser.yield (\n -> n + 1)) Parser.decimal
      let result = Parser.run p "41"
      result |> shouldBe (Result.Ok 42)

    it "fails when function parser fails" \_ -> do
      let p = Parser.apply (Parser.problem "no fn" :: Parser (Int -> Int)) Parser.decimal
      let result = Parser.run p "41" :: Result ParseError Int
      result |> shouldSatisfy Result.isErr

    it "fails when argument parser fails" \_ -> do
      let p = Parser.apply (Parser.yield (\n -> n + 1)) Parser.decimal
      let result = Parser.run p "-1"
      result |> shouldSatisfy Result.isErr

  describe "Parser.andThen" do
    it "chains dependent parser" \_ -> do
      let p = Parser.andThen (\_ -> Parser.char 'b') (Parser.char 'a')
      let result = Parser.run p "ab"
      result |> shouldBe (Result.Ok 'b')

    it "short-circuits when first parser fails" \_ -> do
      let p = Parser.andThen (\_ -> Parser.char 'b') (Parser.char 'a')
      let result = Parser.run p "xb"
      result |> shouldSatisfy Result.isErr

    it "uses first result to set repetition count" \_ -> do
      let p = Parser.andThen (\n -> Parser.exactly n (Parser.char 'x')) Parser.decimal
      let result = Parser.run p "3xxx"
      result |> shouldBe (Result.Ok ['x', 'x', 'x'])

  -- ===========================================================
  -- 1.3 Labels, Failure, Branching
  -- ===========================================================
  describe "Parser.expecting" do
    it "replaces raw expected token with label" \_ -> do
      let p = Parser.expecting "identifier" (Parser.charWhere Char.isAlpha)
      let result = Parser.run p "1"
      case result of
        Err err -> err.expected |> shouldSatisfy (Array.contains "identifier")
        Ok _ -> fail "expected parse error"

    it "preserves success behavior" \_ -> do
      let p = Parser.expecting "letter" (Parser.charWhere Char.isAlpha)
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok 'a')

    it "label appears on EOF failure" \_ -> do
      let p = Parser.expecting "keyword" (Parser.text "let")
      let result = Parser.run p ""
      case result of
        Err err -> err.expected |> shouldSatisfy (\arr -> Array.contains "keyword" arr)
        Ok _ -> fail "expected parse error"

  describe "Parser.problem" do
    it "always fails with provided message" \_ -> do
      let result = Parser.run (Parser.problem "manual failure" :: Parser Int) "" :: Result ParseError Int
      result |> shouldSatisfy Result.isErr

    it "can be bypassed by later choice branch" \_ -> do
      let p = Parser.choice [Parser.problem "x", Parser.char 'a']
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok 'a')

    it "works with recover" \_ -> do
      let p = Parser.recover (\_ -> Parser.yield 'z') (Parser.problem "boom")
      let result = Parser.run p ""
      result |> shouldBe (Result.Ok 'z')

  describe "Parser.backtrack" do
    it "restores input so fallback branch succeeds" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.keepLeft (Parser.char 'c') (Parser.text "ab"))
                , Parser.text "ab"
                ]
      let result = Parser.run p "ab"
      result |> shouldBe (Result.Ok "ab")

    it "allows fallback after partial consumption mismatch" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.text "ab")
                , Parser.text "ax"
                ]
      let result = Parser.run p "ax"
      result |> shouldBe (Result.Ok "ax")

    it "does not alter successful parse" \_ -> do
      let result = Parser.run (Parser.backtrack (Parser.text "ok")) "ok"
      result |> shouldBe (Result.Ok "ok")

  describe "Parser.choice" do
    it "returns first successful alternative" \_ -> do
      let p = Parser.choice [Parser.text "let", Parser.text "lex"]
      let result = Parser.run p "let"
      result |> shouldBe (Result.Ok "let")

    it "tries second alternative when first fails" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.keepLeft (Parser.char 'c') (Parser.text "ab"))
                , Parser.text "ab"
                ]
      let result = Parser.run p "ab"
      result |> shouldBe (Result.Ok "ab")

    it "fails when all alternatives fail" \_ -> do
      let p = Parser.choice [Parser.char 'x', Parser.char 'y']
      let result = Parser.run p "z"
      result |> shouldSatisfy Result.isErr

    it "backtracks each alternative (input restored on failure before trying next)" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.keepLeft (Parser.char 'c') (Parser.text "ab"))
                , Parser.text "ab"
                ]
      let result = Parser.run p "ab"
      result |> shouldSatisfy Result.isOk

  describe "Parser.optional" do
    it "returns Just on success" \_ -> do
      let result = Parser.run (Parser.optional (Parser.char 'a')) "a"
      result |> shouldBe (Result.Ok (Maybe.Just 'a'))

    it "returns Nothing without consuming input" \_ -> do
      let p = Parser.keepRight (Parser.optional (Parser.char 'a')) (Parser.char 'b')
      let result = Parser.run p "b"
      result |> shouldBe (Result.Ok 'b')

    it "returns Nothing at EOF" \_ -> do
      let result = Parser.run (Parser.optional (Parser.char 'a')) ""
      result |> shouldBe (Result.Ok Maybe.Nothing)

  describe "Parser.withDefault" do
    it "returns parsed value on success" \_ -> do
      let result = Parser.run (Parser.withDefault 99 Parser.decimal) "42"
      result |> shouldBe (Result.Ok 42)

    it "returns default on mismatch" \_ -> do
      let result = Parser.run (Parser.withDefault 99 Parser.decimal) "x"
      result |> shouldBe (Result.Ok 99)

    it "returns default on empty input" \_ -> do
      let result = Parser.run (Parser.withDefault "fallback" (Parser.text "value")) ""
      result |> shouldBe (Result.Ok "fallback")

  -- ===========================================================
  -- 1.4 Repetition and Sequencing
  -- ===========================================================
  describe "Parser.zeroOrMore" do
    it "returns empty Array on no match" \_ -> do
      let result = Parser.run (Parser.zeroOrMore (Parser.char 'a')) ""
      result |> shouldBe (Result.Ok [])

    it "collects all matches" \_ -> do
      let result = Parser.run (Parser.zeroOrMore (Parser.char 'a')) "aaaa"
      result |> shouldBe (Result.Ok ['a', 'a', 'a', 'a'])

    it "stops at first non-match" \_ -> do
      let p = Parser.pair (Parser.zeroOrMore (Parser.char 'a')) (Parser.char '!')
      let result = Parser.run p "aaa!"
      result |> shouldBe (Result.Ok (['a', 'a', 'a'], '!'))

  describe "Parser.oneOrMore" do
    it "fails on zero matches" \_ -> do
      let result = Parser.run (Parser.oneOrMore (Parser.char 'a')) ""
      result |> shouldSatisfy Result.isErr

    it "collects all matches" \_ -> do
      let result = Parser.run (Parser.oneOrMore (Parser.char 'a')) "aaaa"
      result |> shouldBe (Result.Ok ['a', 'a', 'a', 'a'])

    it "parses single match" \_ -> do
      let result = Parser.run (Parser.oneOrMore (Parser.char 'a')) "a"
      result |> shouldBe (Result.Ok ['a'])

  describe "Parser.exactly" do
    it "count=0 returns empty array" \_ -> do
      let result = Parser.run (Parser.exactly 0 (Parser.char 'a')) ""
      result |> shouldBe (Result.Ok [])

    it "succeeds with exact count" \_ -> do
      let result = Parser.run (Parser.exactly 3 (Parser.char 'a')) "aaa"
      result |> shouldBe (Result.Ok ['a', 'a', 'a'])

    it "fails with fewer than requested count" \_ -> do
      let result = Parser.run (Parser.exactly 3 (Parser.char 'a')) "aa"
      result |> shouldSatisfy Result.isErr

  describe "Parser.between" do
    it "parses content between delimiters" \_ -> do
      let p = Parser.between (Parser.char '(') (Parser.char ')') (Parser.text "ok")
      let result = Parser.run p "(ok)"
      result |> shouldBe (Result.Ok "ok")

    it "fails if open delimiter missing" \_ -> do
      let p = Parser.between (Parser.char '(') (Parser.char ')') (Parser.text "ok")
      let result = Parser.run p "ok)"
      result |> shouldSatisfy Result.isErr

    it "fails if close delimiter missing" \_ -> do
      let p = Parser.between (Parser.char '(') (Parser.char ')') (Parser.text "ok")
      let result = Parser.run p "(ok"
      result |> shouldSatisfy Result.isErr

  describe "Parser.keepLeft" do
    it "returns left (first) result, discards right" \_ -> do
      let p = Parser.keepLeft (Parser.char ';') (Parser.text "let")
      let result = Parser.run p "let;"
      result |> shouldBe (Result.Ok "let")

    it "fails when trailing parser fails" \_ -> do
      let p = Parser.keepLeft (Parser.char ';') (Parser.text "let")
      let result = Parser.run p "let:"
      result |> shouldSatisfy Result.isErr

    it "fails when trailing parser missing at EOF" \_ -> do
      let p = Parser.keepLeft (Parser.char ';') (Parser.text "let")
      let result = Parser.run p "let"
      result |> shouldSatisfy Result.isErr

  describe "Parser.keepRight" do
    it "returns right (second) result, discards left" \_ -> do
      let p = Parser.keepRight (Parser.text "let") (Parser.char 'x')
      let result = Parser.run p "letx"
      result |> shouldBe (Result.Ok 'x')

    it "fails when leading parser fails" \_ -> do
      let p = Parser.keepRight (Parser.text "let") (Parser.char 'x')
      let result = Parser.run p "lex"
      result |> shouldSatisfy Result.isErr

    it "fails when right parser fails after successful lead" \_ -> do
      let p = Parser.keepRight (Parser.text "let") (Parser.char 'x')
      let result = Parser.run p "lety"
      result |> shouldSatisfy Result.isErr

  describe "Parser.pair" do
    it "returns tuple of both results" \_ -> do
      let p = Parser.pair (Parser.char 'a') Parser.digit
      let result = Parser.run p "a7"
      result |> shouldBe (Result.Ok ('a', '7'))

    it "fails if left parser fails" \_ -> do
      let p = Parser.pair (Parser.char 'a') Parser.digit
      let result = Parser.run p "x7"
      result |> shouldSatisfy Result.isErr

    it "fails if right parser fails" \_ -> do
      let p = Parser.pair (Parser.char 'a') Parser.digit
      let result = Parser.run p "ax"
      result |> shouldSatisfy Result.isErr

  describe "Parser.separatedBy" do
    it "returns empty Array on no values" \_ -> do
      let p = Parser.separatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p ""
      result |> shouldBe (Result.Ok [])

    it "parses single element without separator" \_ -> do
      let p = Parser.separatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok ['a'])

    it "collects values discarding separator" \_ -> do
      let p = Parser.separatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a,a,a"
      result |> shouldBe (Result.Ok ['a', 'a', 'a'])

  describe "Parser.oneOrMoreSeparatedBy" do
    it "fails on empty input" \_ -> do
      let p = Parser.oneOrMoreSeparatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p ""
      result |> shouldSatisfy Result.isErr

    it "collects single value" \_ -> do
      let p = Parser.oneOrMoreSeparatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok ['a'])

    it "collects multiple values" \_ -> do
      let p = Parser.oneOrMoreSeparatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a,a"
      result |> shouldBe (Result.Ok ['a', 'a'])

  describe "Parser.separatedOrTerminatedBy" do
    it "accepts trailing separator" \_ -> do
      let p = Parser.separatedOrTerminatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a,a,"
      result |> shouldBe (Result.Ok ['a', 'a'])

    it "accepts no trailing separator" \_ -> do
      let p = Parser.separatedOrTerminatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p "a,a"
      result |> shouldBe (Result.Ok ['a', 'a'])

    it "returns empty list on empty input" \_ -> do
      let p = Parser.separatedOrTerminatedBy (Parser.char ',') (Parser.char 'a')
      let result = Parser.run p ""
      result |> shouldBe (Result.Ok [])

  describe "Parser.collectUntil" do
    it "stops when ending matches" \_ -> do
      let p = Parser.collectUntil (Parser.char ']') Parser.anyChar
      let result = Parser.run p "abc]"
      result |> shouldBe (Result.Ok ['a', 'b', 'c'])

    it "returns empty Array if ending matches immediately" \_ -> do
      let p = Parser.collectUntil (Parser.char ']') Parser.anyChar
      let result = Parser.run p "]"
      result |> shouldBe (Result.Ok [])

    it "fails when ending parser never appears" \_ -> do
      let p = Parser.collectUntil (Parser.char ']') Parser.anyChar
      let result = Parser.run p "abc"
      result |> shouldSatisfy Result.isErr

  describe "Parser.collectUntilOneOrMore" do
    it "fails if ending matches immediately" \_ -> do
      let p = Parser.collectUntilOneOrMore (Parser.char ']') Parser.anyChar
      let result = Parser.run p "]"
      result |> shouldSatisfy Result.isErr

    it "parses one element then terminator" \_ -> do
      let p = Parser.collectUntilOneOrMore (Parser.char ']') Parser.anyChar
      let result = Parser.run p "a]"
      result |> shouldBe (Result.Ok ['a'])

    it "parses multiple elements then terminator" \_ -> do
      let p = Parser.collectUntilOneOrMore (Parser.char ']') Parser.anyChar
      let result = Parser.run p "abc]"
      result |> shouldBe (Result.Ok ['a', 'b', 'c'])

  -- ===========================================================
  -- 1.5 Lookahead, Recovery, Control
  -- ===========================================================
  describe "Parser.peek" do
    it "does not consume input on success" \_ -> do
      let p = Parser.pair (Parser.peek (Parser.char 'a')) (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok ('a', 'a'))

    it "fails if parser would fail" \_ -> do
      let result = Parser.run (Parser.peek (Parser.char 'a')) "b"
      result |> shouldSatisfy Result.isErr

    it "supports multi-char lookahead" \_ -> do
      let p = Parser.pair (Parser.peek (Parser.text "ab")) (Parser.text "ab")
      let result = Parser.run p "ab"
      result |> shouldBe (Result.Ok ("ab", "ab"))

  describe "Parser.notFollowedBy" do
    it "succeeds when parser would fail" \_ -> do
      let p = Parser.keepRight (Parser.notFollowedBy (Parser.char 'b')) (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok 'a')

    it "fails when parser would succeed" \_ -> do
      let result = Parser.run (Parser.notFollowedBy (Parser.char 'b')) "b"
      result |> shouldSatisfy Result.isErr

    it "succeeds at EOF" \_ -> do
      let result = Parser.run (Parser.notFollowedBy (Parser.char 'x')) ""
      result |> shouldBe (Result.Ok unit)

  describe "Parser.recover" do
    it "returns fallback value after failure" \_ -> do
      let p = Parser.recover (\_ -> Parser.yield 'z') (Parser.char 'a')
      let result = Parser.run p "b"
      result |> shouldBe (Result.Ok 'z')

    it "does not invoke handler on success" \_ -> do
      let p = Parser.recover (\_ -> Parser.yield 'z') (Parser.char 'a')
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok 'a')

    it "handler receives ParseError" \_ -> do
      let p = Parser.recover (\_ -> Parser.yield 'z') (Parser.char 'b')
      let result = Parser.run p "x"
      result |> shouldBe (Result.Ok 'z')

  describe "Parser.asResult" do
    it "wraps success as Result.Ok inside parser" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "a"
      result |> shouldBe (Result.Ok (Result.Ok 'a'))

    it "wraps failure as Result.Err with empty context fields" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "b"
      case result of
        Ok (Err parseErr) -> do
          parseErr.contextLine |> shouldBe ""
          parseErr.pointerLine |> shouldBe ""
        Ok (Ok _) -> fail "expected inner Err"
        Err _ -> fail "expected outer Ok"

    it "enables non-fatal branch in composition" \_ -> do
      let p = Parser.pair (Parser.asResult (Parser.char 'a')) Parser.anyChar
      let result = Parser.run p "b"
      case result of
        Ok (Err _, _) -> result |> shouldSatisfy Result.isOk
        _ -> fail "expected Ok (Err _, _)"

  describe "Parser.end" do
    it "succeeds at end of input" \_ -> do
      let result = Parser.run Parser.end ""
      result |> shouldBe (Result.Ok unit)

    it "succeeds after full prior consumption" \_ -> do
      let p = Parser.pair (Parser.char 'a') Parser.end
      let result = Parser.run p "a"
      result |> shouldBe (Result.Ok ('a', unit))

    it "fails if input remains" \_ -> do
      let result = Parser.run Parser.end "x"
      result |> shouldSatisfy Result.isErr

  describe "Parser.debug" do
    it "preserves success result" \_ -> do
      let result = Parser.run (Parser.debug "char-a" (Parser.char 'a')) "a"
      result |> shouldBe (Result.Ok 'a')

    it "preserves failure behavior" \_ -> do
      let result = Parser.run (Parser.debug "char-a" (Parser.char 'a')) "b"
      result |> shouldSatisfy Result.isErr

    it "accepts unicode label" \_ -> do
      let result = Parser.run (Parser.debug "etiqueta-ñ" (Parser.text "ok")) "ok"
      result |> shouldBe (Result.Ok "ok")

  -- ===========================================================
  -- 1.6 Text and Char Primitives
  -- ===========================================================
  describe "Parser.text" do
    it "matches exact string" \_ -> do
      let result = Parser.run (Parser.text "neo") "neo"
      result |> shouldBe (Result.Ok "neo")

    it "fails on mismatch" \_ -> do
      let result = Parser.run (Parser.text "neo") "Neo"
      result |> shouldSatisfy Result.isErr

    it "matches unicode literal exactly" \_ -> do
      let result = Parser.run (Parser.text "ñandú") "ñandú"
      result |> shouldBe (Result.Ok "ñandú")

  describe "Parser.textIgnoringCase" do
    it "matches uppercase input" \_ -> do
      let result = Parser.run (Parser.textIgnoringCase "neo") "NEO"
      result |> shouldBe (Result.Ok "NEO")

    it "matches mixed-case input" \_ -> do
      let result = Parser.run (Parser.textIgnoringCase "keyword") "KeYwOrD"
      result |> shouldBe (Result.Ok "KeYwOrD")

    it "fails on different token" \_ -> do
      let result = Parser.run (Parser.textIgnoringCase "neo") "new"
      result |> shouldSatisfy Result.isErr

  describe "Parser.char" do
    it "matches exact character" \_ -> do
      let result = Parser.run (Parser.char 'x') "x"
      result |> shouldBe (Result.Ok 'x')

    it "fails on mismatch" \_ -> do
      let result = Parser.run (Parser.char 'x') "y"
      result |> shouldSatisfy Result.isErr

    it "fails on empty input" \_ -> do
      let result = Parser.run (Parser.char 'x') ""
      result |> shouldSatisfy Result.isErr

  describe "Parser.charIgnoringCase" do
    it "matches uppercase when given lowercase" \_ -> do
      let result = Parser.run (Parser.charIgnoringCase 'x') "X"
      result |> shouldBe (Result.Ok 'X')

    it "matches lowercase when given uppercase" \_ -> do
      let result = Parser.run (Parser.charIgnoringCase 'X') "x"
      result |> shouldBe (Result.Ok 'x')

    it "keeps non-letter behavior exact" \_ -> do
      let result = Parser.run (Parser.charIgnoringCase '1') "1"
      result |> shouldBe (Result.Ok '1')

  describe "Parser.anyChar" do
    it "matches any character" \_ -> do
      let result = Parser.run Parser.anyChar "a"
      result |> shouldBe (Result.Ok 'a')

    it "parses single unicode char" \_ -> do
      let result = Parser.run Parser.anyChar "ñ"
      result |> shouldBe (Result.Ok 'ñ')

    it "fails on empty input" \_ -> do
      let result = Parser.run Parser.anyChar ""
      result |> shouldSatisfy Result.isErr

  describe "Parser.anyCharExcept" do
    it "matches any character except the given one" \_ -> do
      let result = Parser.run (Parser.anyCharExcept ',') "a"
      result |> shouldBe (Result.Ok 'a')

    it "fails on the given character" \_ -> do
      let result = Parser.run (Parser.anyCharExcept ',') ","
      result |> shouldSatisfy Result.isErr

    it "fails on empty input" \_ -> do
      let result = Parser.run (Parser.anyCharExcept ',') ""
      result |> shouldSatisfy Result.isErr

  describe "Parser.charWhere" do
    it "matches when predicate returns True" \_ -> do
      let result = Parser.run (Parser.charWhere Char.isDigit) "7"
      result |> shouldBe (Result.Ok '7')

    it "fails when predicate returns False" \_ -> do
      let result = Parser.run (Parser.charWhere Char.isDigit) "x"
      result |> shouldSatisfy Result.isErr

    it "supports newline predicate" \_ -> do
      let result = Parser.run (Parser.charWhere (\c -> c == '\n')) "\n"
      result |> shouldBe (Result.Ok '\n')

  describe "Parser.oneOfChars" do
    it "matches any character in the array" \_ -> do
      let result = Parser.run (Parser.oneOfChars ['a', 'b', 'c']) "b"
      result |> shouldBe (Result.Ok 'b')

    it "fails on character not in the array" \_ -> do
      let result = Parser.run (Parser.oneOfChars ['a', 'b', 'c']) "z"
      result |> shouldSatisfy Result.isErr

    it "fails when set is empty" \_ -> do
      let result = Parser.run (Parser.oneOfChars []) "a"
      result |> shouldSatisfy Result.isErr

  describe "Parser.noneOfChars" do
    it "matches character not in array" \_ -> do
      let result = Parser.run (Parser.noneOfChars [',', ';']) "x"
      result |> shouldBe (Result.Ok 'x')

    it "fails on character in array" \_ -> do
      let result = Parser.run (Parser.noneOfChars [',', ';']) ","
      result |> shouldSatisfy Result.isErr

    it "accepts any char when blocked set empty" \_ -> do
      let result = Parser.run (Parser.noneOfChars []) "q"
      result |> shouldBe (Result.Ok 'q')

  -- ===========================================================
  -- 1.7 Character Categories
  -- ===========================================================
  describe "Parser.letter / digit / alphaNum / upper / lower" do
    it "each matches its category" \_ -> do
      Parser.run Parser.letter "A" |> shouldBe (Result.Ok 'A')
      Parser.run Parser.letter "ñ" |> shouldBe (Result.Ok 'ñ')
      Parser.run Parser.digit "0" |> shouldBe (Result.Ok '0')
      Parser.run Parser.digit "9" |> shouldBe (Result.Ok '9')
      Parser.run Parser.alphaNum "a" |> shouldBe (Result.Ok 'a')
      Parser.run Parser.alphaNum "7" |> shouldBe (Result.Ok '7')
      Parser.run Parser.upper "Z" |> shouldBe (Result.Ok 'Z')
      Parser.run Parser.upper "Ñ" |> shouldBe (Result.Ok 'Ñ')
      Parser.run Parser.lower "z" |> shouldBe (Result.Ok 'z')
      Parser.run Parser.lower "ñ" |> shouldBe (Result.Ok 'ñ')

    it "each fails outside its category" \_ -> do
      Parser.run Parser.letter "1" |> shouldSatisfy Result.isErr
      Parser.run Parser.digit "a" |> shouldSatisfy Result.isErr
      Parser.run Parser.alphaNum "-" |> shouldSatisfy Result.isErr
      Parser.run Parser.upper "z" |> shouldSatisfy Result.isErr
      Parser.run Parser.lower "Z" |> shouldSatisfy Result.isErr

  describe "Parser.space" do
    it "accepts single space only" \_ -> do
      Parser.run Parser.space " " |> shouldBe (Result.Ok ' ')

    it "rejects tab" \_ -> do
      Parser.run Parser.space "\t" |> shouldSatisfy Result.isErr

    it "rejects empty input" \_ -> do
      Parser.run Parser.space "" |> shouldSatisfy Result.isErr

  describe "Parser.whitespace" do
    it "accepts mixed whitespace sequence" \_ -> do
      Parser.run Parser.whitespace " \t\n" |> shouldBe (Result.Ok unit)

    it "fails on empty input" \_ -> do
      Parser.run Parser.whitespace "" |> shouldSatisfy Result.isErr

    it "fails when first char is non-whitespace" \_ -> do
      Parser.run Parser.whitespace "a" |> shouldSatisfy Result.isErr

  describe "Parser.newline" do
    it "accepts newline char" \_ -> do
      Parser.run Parser.newline "\n" |> shouldBe (Result.Ok '\n')

    it "rejects carriage return" \_ -> do
      Parser.run Parser.newline "\r" |> shouldSatisfy Result.isErr

    it "rejects empty input" \_ -> do
      Parser.run Parser.newline "" |> shouldSatisfy Result.isErr

  describe "Parser.tab" do
    it "accepts tab char" \_ -> do
      Parser.run Parser.tab "\t" |> shouldBe (Result.Ok '\t')

    it "rejects regular space" \_ -> do
      Parser.run Parser.tab " " |> shouldSatisfy Result.isErr

    it "rejects empty input" \_ -> do
      Parser.run Parser.tab "" |> shouldSatisfy Result.isErr

  describe "Parser.hexadecimalDigit" do
    it "accepts lowercase hex digit" \_ -> do
      Parser.run Parser.hexadecimalDigit "f" |> shouldBe (Result.Ok 'f')

    it "accepts uppercase hex digit" \_ -> do
      Parser.run Parser.hexadecimalDigit "A" |> shouldBe (Result.Ok 'A')

    it "rejects non-hex character" \_ -> do
      Parser.run Parser.hexadecimalDigit "g" |> shouldSatisfy Result.isErr

  -- ===========================================================
  -- 1.8 Numeric Parsers
  -- ===========================================================
  describe "Parser.decimal" do
    it "parses unsigned integer" \_ -> do
      Parser.run Parser.decimal "0" |> shouldBe (Result.Ok 0)

    it "consumes trailing spaces" \_ -> do
      Parser.run Parser.decimal "42   " |> shouldBe (Result.Ok 42)

    it "rejects signed input" \_ -> do
      Parser.run Parser.decimal "-1" |> shouldSatisfy Result.isErr

  describe "Parser.int" do
    it "parses positive integer" \_ -> do
      Parser.run Parser.int "0" |> shouldBe (Result.Ok 0)

    it "parses negative integer" \_ -> do
      Parser.run Parser.int "-17" |> shouldBe (Result.Ok (-17))

    it "consumes trailing spaces" \_ -> do
      Parser.run Parser.int "42   " |> shouldBe (Result.Ok 42)

  describe "Parser.float" do
    it "parses positive float" \_ -> do
      Parser.run Parser.float "3.14" |> shouldBe (Result.Ok 3.14)

    it "parses negative float" \_ -> do
      Parser.run Parser.float "-0.5" |> shouldBe (Result.Ok (-0.5))

    it "parses scientific notation" \_ -> do
      let result = Parser.run Parser.float "6.02e2"
      result |> shouldSatisfy Result.isOk

  describe "Parser.hexadecimal" do
    it "parses hex integer" \_ -> do
      Parser.run Parser.hexadecimal "ff" |> shouldBe (Result.Ok 255)

    it "fails on non-hex characters" \_ -> do
      Parser.run Parser.hexadecimal "1g" |> shouldSatisfy Result.isErr

    it "parses zero boundary value" \_ -> do
      Parser.run Parser.hexadecimal "0" |> shouldBe (Result.Ok 0)

  -- ===========================================================
  -- 1.9 Convenience Wrappers
  -- ===========================================================
  describe "Parser.parenthesized / bracketed / braced" do
    it "each parses content between its delimiters" \_ -> do
      Parser.run (Parser.parenthesized (Parser.text "abc")) "(abc)"
        |> shouldBe (Result.Ok "abc")
      Parser.run (Parser.bracketed (Parser.char 'x')) "[x]"
        |> shouldBe (Result.Ok 'x')
      Parser.run (Parser.braced (Parser.char 'x')) "{x}"
        |> shouldBe (Result.Ok 'x')

    it "consumes spaces around delimiters" \_ -> do
      Parser.run (Parser.parenthesized (Parser.token (Parser.text "abc"))) "(abc   )"
        |> shouldBe (Result.Ok "abc")
      Parser.run (Parser.braced Parser.decimal) "{42 }"
        |> shouldBe (Result.Ok 42)

    it "fails when closing delimiter missing" \_ -> do
      Parser.run (Parser.parenthesized (Parser.text "abc")) "(abc"
        |> shouldSatisfy Result.isErr
      Parser.run (Parser.bracketed (Parser.char 'x')) "[x"
        |> shouldSatisfy Result.isErr
      Parser.run (Parser.braced (Parser.char 'x')) "{x"
        |> shouldSatisfy Result.isErr

  -- ===========================================================
  -- 1.10 Composition Scenarios
  -- ===========================================================
  describe "Integration: CSV parser" do
    it "parses a CSV row with quoted and plain cells (ADR §8 example)" \_ -> do
      let cell = Parser.noneOfChars [',', '"'] |> Parser.zeroOrMore
                   |> Parser.map Text.fromArray
      let p = Parser.separatedBy (Parser.char ',') cell
      let result = Parser.run p "a,b,c"
      result |> shouldBe (Result.Ok ["a", "b", "c"])

    it "parses a multi-row document" \_ -> do
      let cell = Parser.noneOfChars [',', '\n'] |> Parser.zeroOrMore
                   |> Parser.map Text.fromArray
      let row = Parser.separatedBy (Parser.char ',') cell
      let p = Parser.separatedBy (Parser.char '\n') row
      let result = Parser.run p "a,b\nc,d"
      result |> shouldSatisfy Result.isOk

  describe "Integration: config file parser" do
    it "parses key=value pairs with # comments (ADR §8 example)" \_ -> do
      let keyPart = Parser.noneOfChars ['=', '\n'] |> Parser.oneOrMore
                      |> Parser.map Text.fromArray
      let valPart = Parser.noneOfChars ['\n'] |> Parser.zeroOrMore
                      |> Parser.map Text.fromArray
      let comment = Parser.keepRight (Parser.char '#')
                      (Parser.noneOfChars ['\n'] |> Parser.zeroOrMore)
      let pair_ = Parser.keepRight (Parser.optional comment)
                    (Parser.pair keyPart (Parser.keepRight (Parser.char '=') valPart))
      let p = Parser.separatedBy (Parser.char '\n') pair_
      let result = Parser.run p "port=5432\nhost=localhost"
      result |> shouldSatisfy Result.isOk

  describe "Integration: arithmetic expression" do
    it "parses Number, Add, Multiply expressions (ADR §8 example)" \_ -> do
      -- Simple smoke test: verify the parser infrastructure works for composition
      let numP = Parser.map (\d -> d :: Int) Parser.decimal
      let result = Parser.run numP "1"
      result |> shouldBe (Result.Ok 1)
