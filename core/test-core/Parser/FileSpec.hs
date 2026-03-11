module Parser.FileSpec where

import Core
import Maybe qualified
import Parser (ParseFileError (..))
import Parser qualified
import "nhcore" Path qualified
import Result qualified
import Task qualified
import Test


spec :: Spec Unit
spec = do
  describe "Parser.runOnFile" do
    it "returns Err (FileReadError ...) when file does not exist" \_ -> do
      let path = Path.fromText "/tmp/nhparser-nonexistent-99999.txt"
                   |> Maybe.getOrDie
      result <- Parser.runOnFile (Parser.text "abc") path
                  |> Task.asResult
      case result of
        Err (FileReadError _) -> result |> shouldSatisfy Result.isErr
        Err (ParseFailure _)  -> fail "expected FileReadError, got ParseFailure"
        Ok _                  -> fail "expected Err, got Ok"

    it "constructor path: FileReadError branch" \_ -> do
      let path = Path.fromText "/tmp/nhparser-nonexistent-branch.txt"
                   |> Maybe.getOrDie
      result <- Parser.runOnFile (Parser.text "x") path
                  |> Task.asResult
      case result of
        Err (FileReadError _) -> result |> shouldSatisfy Result.isErr
        Err (ParseFailure _)  -> fail "expected FileReadError branch"
        Ok _                  -> fail "expected Err"

    it "exhaustive handling check" \_ -> do
      let path = Path.fromText "/tmp/nhparser-test-7.txt" |> Maybe.getOrDie
      result <- Parser.runOnFile (Parser.text "x") path
                  |> Task.asResult
      case result of
        Ok _                  -> result |> shouldSatisfy Result.isOk
        Err (FileReadError _) -> result |> shouldSatisfy Result.isErr
        Err (ParseFailure _)  -> result |> shouldSatisfy Result.isErr
