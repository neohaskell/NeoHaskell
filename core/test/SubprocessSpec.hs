module SubprocessSpec (spec) where

import Core
import Array qualified
import AsyncTask qualified
import Console qualified
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

    it "captures complete normal and nonzero child output" \_ -> do
      assertCaptured 1024 0
      assertCaptured 1024 7

    it "drains simultaneous real pipes beyond pipe capacity" \_ -> do
      assertCaptured 262144 7

    it "captures complete one MiB output per stream" \_ -> do
      assertCaptured 1048576 0

    it "preserves complete output above one MiB without a new capture cap" \_ -> do
      assertCaptured 1048577 0

    it "reaps an observed direct child after timeout" \_ -> do
      withRunningChild 1 "normal" \workspace worker -> do
        outcome <- AsyncTask.waitFor worker |> Task.asResult
        case outcome of
          Err message -> message |> shouldSatisfy (Text.contains "TimeoutError")
          other -> Test.fail [fmt|Expected child timeout, got #{other}|]
        requireAbsent workspace

    it "reaps an observed TERM-resistant child after timeout" \_ -> do
      withRunningChild 1 "ignore-term" \workspace worker -> do
        let deadline = do
              AsyncTask.sleep 4000
              Task.throw "Owned TERM-resistant child cleanup exceeded four seconds"
        outcome <- AsyncTask.race (AsyncTask.waitFor worker) deadline |> Task.asResult
        case outcome of
          Err message -> message |> shouldSatisfy (Text.contains "TimeoutError")
          Ok _ -> Test.fail "Expected a timeout result for the TERM-resistant child"
        requireAbsent workspace

    it "propagates cancellation and reaps the observed direct child" \_ -> do
      withRunningChild 10 "normal" \workspace worker -> do
        AsyncTask.cancel worker
        outcome <- AsyncTask.waitCatch worker
        case outcome of
          Err message -> message |> shouldSatisfy (Text.contains "AsyncCancelled")
          Ok completion -> Test.fail [fmt|Expected cancellation, got #{completion}|]
        requireAbsent workspace


runFixture :: Int -> Array Text -> Task Subprocess.Error Subprocess.Completion
runFixture seconds arguments =
  Subprocess.runWithTimeout seconds "python3"
    (["-c", fixtureScript] |> Array.append arguments) "."


assertCaptured :: Int -> Int -> Task Text Unit
assertCaptured size code = do
  completion <- runFixture 3 ["emit", Text.fromInt size, Text.fromInt code]
    |> Task.mapError (\err -> [fmt|#{err}|])
  completion.exitCode |> shouldBe code
  completion.stdout |> shouldBe (Text.repeat size "x")
  completion.stderr |> shouldBe (Text.repeat size "x")


fixtureControl :: Array Text -> Task Text Text
fixtureControl arguments = do
  completion <- runFixture 5 arguments |> Task.mapError (\err -> [fmt|#{err}|])
  Task.unless (completion.exitCode == 0) do
    let stderrText = completion.stderr
    Test.fail [fmt|Fixture control failed: #{stderrText}|]
  let diagnostic = completion.stderr
  Task.unless (Text.isEmpty diagnostic) (Console.print diagnostic)
  Task.yield completion.stdout


withRunningChild ::
  Int -> Text ->
  (Text -> AsyncTask.AsyncTask Text Subprocess.Completion -> Task Text Unit) ->
  Task Text Unit
withRunningChild seconds mode test = do
  workspace <- fixtureControl ["create"]
  let cleanup = fixtureControl ["cleanup", workspace] |> Task.map (\_ -> ())
  let child = runFixture seconds ["start", workspace, mode] |> Task.mapError (\err -> [fmt|#{err}|])
  AsyncTask.process child (\worker -> do
    let check = do
          requireReady workspace
          test workspace worker
    check |> Task.finally cleanup)


requireReady :: Text -> Task Text Unit
requireReady workspace = do
  status <- fixtureControl ["ready", workspace]
  status |> shouldBe "alive"


requireAbsent :: Text -> Task Text Unit
requireAbsent workspace = do
  status <- fixtureControl ["observe", workspace]
  status |> shouldBe "absent"


-- Cleanup stops execution but cannot reap another parent's child. The test's
-- requireAbsent rejects zombies; only cleanup permits one until the REPL exits.
-- The fixture owns only its mkdtemp directory and the PID whose command names it.
fixtureScript :: Text
fixtureScript = [fmt|
"""Synthetic direct-child fixtures; no network or user file access."""
import os
from pathlib import Path
import shutil
import signal
import subprocess
import sys
import tempfile
import threading
import time


def process_state(pid):
    result = subprocess.run(["ps", "-p", str(pid), "-o", "stat=", "-o", "command="], capture_output=True, text=True)
    state = result.stdout.strip()
    if result.stderr or result.returncode not in (0, 1):
        raise RuntimeError("process observer failed: " + result.stderr)
    if result.returncode == 0 and state:
        return state
    if result.returncode == 1 and not state:
        return ""
    raise RuntimeError("process observer returned inconsistent status")


def owned_pid(workspace):
    pid = int((workspace / "pid").read_text())
    state = process_state(pid)
    if state and "Z" not in state.split()[0] and str(workspace) not in state:
        raise RuntimeError("PID no longer belongs to this fixture")
    return pid, state


def emit(stream, size):
    remaining = size
    chunk = b"x" * 4096
    while remaining:
        remaining -= os.write(stream, chunk[:remaining])


def main():
    mode = sys.argv[1]
    if mode == "create":
        print(tempfile.mkdtemp(prefix="nh-subprocess-test-"), end="")
    elif mode == "emit":
        size, exit_code = int(sys.argv[2]), int(sys.argv[3])
        threads = [threading.Thread(target=emit, args=(fd, size)) for fd in (1, 2)]
        for thread in threads:
            thread.start()
        for thread in threads:
            thread.join()
        raise SystemExit(exit_code)
    elif mode == "start":
        workspace = Path(sys.argv[2])
        if sys.argv[3] == "ignore-term":
            signal.signal(signal.SIGTERM, signal.SIG_IGN)
        (workspace / "pid").write_text(str(os.getpid()))
        time.sleep(20)
    elif mode == "ready":
        workspace = Path(sys.argv[2])
        deadline = time.monotonic() + 3
        while not (workspace / "pid").exists():
            if time.monotonic() >= deadline:
                raise RuntimeError("fixture child never published readiness")
            time.sleep(0.01)
        _, state = owned_pid(workspace)
        if not state or "Z" in state.split()[0]:
            raise RuntimeError("fixture child was not alive at readiness")
        print("alive", end="")
    elif mode == "observe":
        _, state = owned_pid(Path(sys.argv[2]))
        print("present" if state else "absent", end="")
    elif mode == "cleanup":
        workspace = Path(sys.argv[2])
        if (workspace / "pid").exists():
            pid, state = owned_pid(workspace)
            if state and "Z" not in state.split()[0]:
                os.kill(pid, signal.SIGKILL)
            deadline = time.monotonic() + 3
            while True:
                state = process_state(pid)
                if not state or "Z" in state.split()[0]:
                    print("cleanup-state:" + ("zombie" if state else "absent"), file=sys.stderr)
                    break
                if time.monotonic() >= deadline:
                    raise RuntimeError("fixture child still executing after cleanup")
                time.sleep(0.01)
        shutil.rmtree(workspace)
    else:
        raise RuntimeError("unknown fixture operation")


if __name__ == "__main__":
    main()
|]
