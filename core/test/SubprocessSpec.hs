module SubprocessSpec (spec) where

import Core
import Subprocess qualified
import Task qualified
import Test
import Text qualified


spec :: Spec Unit
spec = do
  describe "Subprocess" do
    it "drains real stderr before waiting for process exit" \_ -> do
      result <- Subprocess.runWithTimeout 3 "python3"
        [ "-c"
        , "import os; payload = b'x' * 262144; offset = 0\nwhile offset < len(payload): offset += os.write(2, payload[offset:])\nraise SystemExit(7)"
        ] "."
        |> Task.asResult
      case result of
        Err err -> Test.fail [fmt|Expected exit 7 after draining stderr, got #{err}|]
        Ok completion -> do
          completion.exitCode |> shouldBe 7
          completion.stderr |> Text.length |> shouldBe 262144
          completion.stdout |> shouldBe ""
