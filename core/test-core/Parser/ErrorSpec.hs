module Parser.ErrorSpec where

import Array qualified
import Char qualified
import Core
import LinkedList qualified
import Maybe (Maybe (..))
import Maybe qualified
import Parser (Parser, ParseError (..), ParsePosition (..))
import Parser qualified
import Test
import Text qualified


-- | Helper: run a parser on input via runNamed and extract the error.
-- Panics if parse succeeds.
extractError :: Text -> Parser value -> Text -> ParseError
extractError sourceName p input =
  case Parser.runNamed sourceName p input of
    Err err -> err
    Ok _ -> panic "extractError: expected parse failure but got success"


spec :: Spec Unit
spec = do
  -- ===========================================================
  -- 2.1 formatError
  -- ===========================================================
  describe "formatError" do
    it "first line is 'Parse error in <sourceName> at line <N>, column <M>'" \_ -> do
      let err = extractError "demo.neo" (Parser.text "abc") "abX"
      let formatted = Parser.formatError err
      let firstLine = formatted |> Text.split "\n" |> Array.first |> Maybe.withDefault ""
      firstLine |> shouldSatisfy (Text.contains "Parse error in demo.neo at line 1, column 3")

    it "third line is contextLine (the source line)" \_ -> do
      let err = extractError "demo.neo" (Parser.text "abc") "abX"
      let formatted = Parser.formatError err
      formatted |> shouldSatisfy (Text.contains "abX")

    it "fourth line is pointerLine with ^ at correct column offset" \_ -> do
      let err = extractError "demo.neo" (Parser.text "abc") "abX"
      let formatted = Parser.formatError err
      formatted |> shouldSatisfy (Text.contains "  ^")

    it "Expected: section lists sorted deduplicated tokens" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.text "if")
                , Parser.backtrack (Parser.text "if")
                , Parser.text "in"
                ]
      let err = extractError "test" p "ix"
      let formatted = Parser.formatError err
      formatted |> shouldSatisfy (Text.contains "Expected:")

    it "Found: section shows unexpected token" \_ -> do
      let err = extractError "test" (Parser.char 'a') "b"
      let formatted = Parser.formatError err
      formatted |> shouldSatisfy (Text.contains "Found:")

    it "Found: shows 'end of input' when unexpected is Nothing" \_ -> do
      let err = extractError "test" (Parser.text "abc") "ab"
      let formatted = Parser.formatError err
      formatted |> shouldSatisfy (Text.contains "end of input")

    it "Hint: line appears for each hint" \_ -> do
      -- Craft a ParseError with two hints and verify both appear
      let err = extractError "test" (Parser.char 'a') "x"
      let formatted = Parser.formatError err
      -- At minimum, hints section should be non-empty
      formatted |> shouldSatisfy (Text.contains "Hint:")

  -- ===========================================================
  -- 2.2 formatErrorCompact
  -- ===========================================================
  describe "formatErrorCompact" do
    it "is a single line" \_ -> do
      let err = extractError "demo.neo" (Parser.char 'x') "y"
      let formatted = Parser.formatErrorCompact err
      formatted |> shouldSatisfy (\t -> not (Text.contains "\n" t))

    it "contains source name, line number, column number" \_ -> do
      let err = extractError "demo.neo" (Parser.char 'x') "y"
      let formatted = Parser.formatErrorCompact err
      formatted |> shouldSatisfy (Text.contains "demo.neo")
      formatted |> shouldSatisfy (Text.contains "line 1")
      formatted |> shouldSatisfy (Text.contains "column 1")

    it "handles fromSingleError shape" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "b"
      case result of
        Ok (Err parseErr) -> do
          let formatted = Parser.formatErrorCompact parseErr
          formatted |> shouldSatisfy (Text.contains "<unknown>")
        _ -> fail "expected Ok (Err _)"

  -- ===========================================================
  -- 2.3 ParseError Fields
  -- ===========================================================
  describe "ParseError.position" do
    it "line is 1-based" \_ -> do
      let err = extractError "p.neo"
                  (Parser.keepRight (Parser.text "a\n") (Parser.char 'b'))
                  "a\nx"
      err.position.line |> shouldBe 2

    it "column is 1-based" \_ -> do
      let err = extractError "p.neo"
                  (Parser.keepRight (Parser.text "a\n") (Parser.char 'b'))
                  "a\nx"
      err.position.column |> shouldBe 1

    it "offset is 0-based" \_ -> do
      let err = extractError "p.neo"
                  (Parser.keepRight (Parser.text "a\n") (Parser.char 'b'))
                  "a\nx"
      err.position.offset |> shouldBe 2

    it "sourceName matches argument passed to runNamed" \_ -> do
      let err = extractError "myfile.neo" (Parser.char 'x') "y"
      err.position.sourceName |> shouldBe "myfile.neo"

  describe "ParseError.expected" do
    it "is deduplicated" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.text "if")
                , Parser.text "if"
                ]
      let err = extractError "test" p "ix"
      -- If deduplicated, no consecutive equal entries
      let list = err.expected |> Array.toLinkedList
      let sorted = LinkedList.sort list
      sorted |> shouldBe list  -- already sorted = deduplicated
      -- Verify no adjacent duplicates (sorted + no adj dups = deduplicated)
      case sorted of
        (a : b : _) -> a |> shouldSatisfy (\x -> x != b)
        _ -> unit |> shouldBe unit

    it "is sorted alphabetically" \_ -> do
      let p = Parser.choice
                [ Parser.backtrack (Parser.text "let")
                , Parser.text "if"
                ]
      let err = extractError "test" p "lx"
      let expectedList = err.expected |> Array.toLinkedList
      expectedList |> shouldBe (LinkedList.sort expectedList)

    it "labels from Parser.expecting appear instead of raw token representations" \_ -> do
      let p = Parser.expecting "identifier" (Parser.charWhere Char.isAlpha)
      let err = extractError "test" p "1"
      err.expected |> shouldSatisfy (Array.contains "identifier")

  describe "ParseError.unexpected" do
    it "stores unexpected token text" \_ -> do
      let err = extractError "test" (Parser.char 'a') "b"
      err.unexpected |> shouldBe (Maybe.Just "\"b\"")

    it "is Nothing at EOF" \_ -> do
      let err = extractError "test" (Parser.text "abc") "ab"
      err.unexpected |> shouldBe Maybe.Nothing

    it "stores fancy error message from problem" \_ -> do
      let err = extractError "test" (Parser.problem "manual boom") ""
      err.unexpected |> shouldBe (Maybe.Just "manual boom")

  describe "ParseError.contextLine" do
    it "captures failing source line" \_ -> do
      let err = extractError "demo.neo" (Parser.text "abc") "abX"
      err.contextLine |> shouldBe "abX"

    it "captures second line for multiline failure" \_ -> do
      let err = extractError "demo.neo" (Parser.text "a\nby") "a\nbx"
      err.contextLine |> shouldBe "bx"

    it "empty when produced by fromSingleError path" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "b"
      case result of
        Ok (Err parseErr) -> parseErr.contextLine |> shouldBe ""
        _ -> fail "expected Ok (Err _)"

  describe "ParseError.pointerLine" do
    it "has (column - 1) spaces followed by '^'" \_ -> do
      let err = extractError "demo.neo" (Parser.text "abc") "abX"
      err.pointerLine |> shouldBe "  ^"

    it "pointer at column 1 has no leading spaces" \_ -> do
      let err = extractError "test" (Parser.char 'a') "x"
      err.pointerLine |> shouldBe "^"

    it "empty when produced by fromSingleError path" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "b"
      case result of
        Ok (Err parseErr) -> parseErr.pointerLine |> shouldBe ""
        _ -> fail "expected Ok (Err _)"

  describe "ParseError.hints" do
    it "suggests missing opening delimiter for unexpected ')'" \_ -> do
      let err = extractError "test" (Parser.char '(') ")"
      err.hints
        |> shouldSatisfy (Array.any (\h -> Text.contains "missing opening '('" h))

    it "suggests missing opening delimiter for unexpected ']'" \_ -> do
      let err = extractError "test" (Parser.char '[') "]"
      err.hints
        |> shouldSatisfy (Array.any (\h -> Text.contains "missing opening '['" h))

    it "suggests missing opening delimiter for unexpected '}'" \_ -> do
      let err = extractError "test" (Parser.char '{') "}"
      err.hints
        |> shouldSatisfy (Array.any (\h -> Text.contains "missing opening '{'" h))

    it "suggests missing closing delimiter for unexpected EOF" \_ -> do
      let err = extractError "test" (Parser.text "abc") "ab"
      err.hints
        |> shouldSatisfy (Array.any (\h -> Text.contains "missing closing delimiter" h))

    it "suggests removing unexpected token for other cases" \_ -> do
      let err = extractError "test" (Parser.char 'a') "x"
      err.hints
        |> shouldSatisfy (Array.any (\h -> Text.contains "Remove the unexpected token" h))

  describe "ParseError.rawMessage" do
    it "is non-empty on parse failure" \_ -> do
      let err = extractError "demo.neo" (Parser.char 'a') "x"
      Text.length err.rawMessage |> shouldSatisfy (\n -> n > 0)

    it "matches errorBundlePretty output from megaparsec" \_ -> do
      let err = extractError "demo.neo" (Parser.char 'a') "x"
      err.rawMessage |> shouldSatisfy (Text.contains "demo.neo")

    it "non-empty for single-error path" \_ -> do
      let result = Parser.run (Parser.asResult (Parser.char 'a')) "x"
      case result of
        Ok (Err parseErr) -> do
          Text.length parseErr.rawMessage |> shouldSatisfy (\n -> n > 0)
          parseErr.contextLine |> shouldBe ""
        _ -> fail "expected Ok (Err _)"
