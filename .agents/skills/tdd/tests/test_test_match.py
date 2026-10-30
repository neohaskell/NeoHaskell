from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
SOURCE = Path(__file__).resolve().parents[1] / "scripts/test-match"
BOUNDED_PROCESS_SOURCE = Path(__file__).resolve().parents[2] / "neohaskell-best-practices/scripts/bounded_process.py"


class TestMatchTests(unittest.TestCase):
    def setUp(self) -> None:
        self.temporary = tempfile.TemporaryDirectory()
        self.base = Path(self.temporary.name)
        self.skills = self.base / ".agents/skills"
        self.script = self.skills / "tdd/scripts/test-match"
        self.wrapper = self.skills / "neohaskell-best-practices/scripts/with-toolchain"
        self.root = self.base / "project"
        self.work = self.base / "caller"
        self.work.mkdir()
        self.root.mkdir()
        (self.root / "nix").mkdir()
        (self.root / "cabal.project").write_text("packages: core/*.cabal\n")
        (self.root / "nix/hix.nix").write_text("compiler-nix-name = ghc98;\n")
        self.script.parent.mkdir(parents=True)
        self.wrapper.parent.mkdir(parents=True)
        shutil.copy2(SOURCE, self.script)
        self.script.chmod(0o755)
        self.log = self.base / "argv.json"
        self.stdin = self.base / "stdin.txt"
        self.scenario = self.base / "scenario.json"
        self.scenario.write_text(json.dumps({"output": "1 example, 0 failures\n", "exit": 0}))
        bounded_process = self.skills / "neohaskell-best-practices/scripts/bounded_process.py"
        bounded_process.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(BOUNDED_PROCESS_SOURCE, bounded_process)
        root_helper = self.skills / "neohaskell-best-practices/scripts/repo-root"
        root_helper.write_text(f"#!/bin/sh\nprintf '%s\\n' {shlex_quote(str(self.root))}\n")
        root_helper.chmod(0o755)
        self.wrapper.write_text(
            "#!/usr/bin/env python3\n"
            "import json, os, sys, time\n"
            "from pathlib import Path\n"
            "Path(os.environ['TESTMATCH_ARGV']).write_text(json.dumps(sys.argv[1:]))\n"
            "Path(os.environ['TESTMATCH_STDIN']).write_text(sys.stdin.read())\n"
            "scenario = json.loads(Path(os.environ['TESTMATCH_SCENARIO']).read_text())\n"
            "sys.stdout.write(scenario.get('output', ''))\nsys.stdout.flush()\n"
            "if scenario.get('sleep'):\n    time.sleep(scenario['sleep'])\n"
            "raise SystemExit(scenario.get('exit', 0))\n"
        )
        self.wrapper.chmod(0o755)
        self.env = os.environ.copy()
        self.env.update(
            {
                "TESTMATCH_ARGV": str(self.log),
                "TESTMATCH_STDIN": str(self.stdin),
                "TESTMATCH_SCENARIO": str(self.scenario),
                "NEOHASKELL_ROOT": str(self.root),
            }
        )

    def tearDown(self) -> None:
        self.temporary.cleanup()

    def set_scenario(self, **values: object) -> None:
        self.scenario.write_text(json.dumps(values))

    def run_match(self, *args: str, timeout: float = 10) -> subprocess.CompletedProcess[str]:
        return subprocess.run(
            [sys.executable, str(self.script), *args],
            cwd=self.work,
            env=self.env,
            text=True,
            capture_output=True,
            check=False,
            timeout=timeout,
        )

    def test_pass_uses_owned_wrapper_argv_and_writes_requested_transcript(self) -> None:
        result = self.run_match("--transcript", "runs/pass.log", 'Cart "quoted" \\ path', "nhcore-test-core")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("PASS: 1 example, 0 failures", result.stdout)
        self.assertEqual(
            json.loads(self.log.read_text()),
            ["cabal", "repl", "nhcore-test-core", "--project-file=cabal.project.dev", "-v0"],
        )
        self.assertEqual(self.stdin.read_text(), ':main --match "Cart \\"quoted\\" \\\\ path"\n')
        transcript = self.work / "runs/pass.log"
        self.assertTrue(transcript.is_file())
        self.assertIn('"cabal", "repl", "nhcore-test-core"', transcript.read_text())
        self.assertIn("outcome: passed", transcript.read_text())

    def test_default_transcript_is_owned_by_tdd_under_repository_state(self) -> None:
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 0, result.stderr)
        lines = result.stdout.splitlines()
        path_line = next(line for line in lines if line.startswith("test-match: transcript "))
        transcript = Path(path_line.removeprefix("test-match: transcript "))
        self.assertTrue(transcript.is_file())
        self.assertEqual(transcript.parent, self.root.resolve() / ".agents-state/tdd/test-match")

    def test_builddir_keeps_focused_repl_output_separate(self) -> None:
        builddir = self.base / "focused-build"
        result = self.run_match("--builddir", str(builddir), "Cart")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(
            json.loads(self.log.read_text()),
            [
                "cabal",
                "repl",
                "nhcore-test-core",
                "--project-file=cabal.project.dev",
                "-v0",
                f"--builddir={builddir}",
            ],
        )

    def test_zero_selection_is_rejected(self) -> None:
        self.set_scenario(output="0 examples, 0 failures\n", exit=0)
        result = self.run_match("NoSuchExample")
        self.assertEqual(result.returncode, 1)
        self.assertIn("no Hspec examples matched", result.stdout)

    def test_hspec_failure_is_rejected(self) -> None:
        self.set_scenario(output="2 examples, 1 failure\n", exit=0)
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 1)
        self.assertIn("Hspec reported failures", result.stdout)

    def test_hspec_pending_summary_is_accepted(self) -> None:
        self.set_scenario(output="31 examples, 0 failures, 4 pending\n", exit=0)
        result = self.run_match("Integration.Dispatcher", "nhcore-test-integration")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("PASS: 31 examples, 0 failures, 4 pending", result.stdout)

    def test_hspec_failure_with_pending_is_rejected(self) -> None:
        self.set_scenario(
            output=(
                "Failures:\n  1) Cart rejects a stale revision\n"
                "       expected revision 3, got 2\n\n"
                "5 examples, 1 failure, 4 pending\n"
            ),
            exit=0,
        )
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 1)
        self.assertIn("Hspec reported failures", result.stdout)

    def test_failure_and_pending_counts_cannot_exceed_example_count(self) -> None:
        self.set_scenario(output="5 examples, 2 failures, 4 pending\n", exit=0)
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 1)
        self.assertIn("failure and pending counts exceed the example count", result.stdout)

    def test_oversized_summary_count_is_rejected_and_transcribed(self) -> None:
        self.set_scenario(output=f"1 example, 0 failures, {'9' * 5000} pending\n", exit=0)
        transcript = self.base / "oversized-summary.log"
        result = self.run_match("--transcript", str(transcript), "Cart")
        self.assertEqual(result.returncode, 1, result.stderr)
        self.assertTrue(transcript.is_file(), result.stderr)
        self.assertIn("outcome: invalid-summary", transcript.read_text())

    def test_compile_setup_error_is_rejected_even_with_a_success_summary(self) -> None:
        self.set_scenario(output="Main.hs:9:1: error: Could not find module X\n1 example, 0 failures\n", exit=0)
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 1)
        self.assertIn("setup error", result.stdout)

    def test_nonzero_cabal_exit_is_rejected_even_with_a_success_summary(self) -> None:
        self.set_scenario(output="1 example, 0 failures, 1 pending\n", exit=23)
        result = self.run_match("Cart")
        self.assertEqual(result.returncode, 1)
        self.assertIn("exited with status 23", result.stdout)

    def test_missing_ambiguous_and_malformed_summaries_are_rejected(self) -> None:
        for output in (
            "no summary\n",
            "1 example, 0 failures\n2 examples, 0 failures, 1 pending\n",
            "1 examples, 0 failure\n",
            "1 example, 0 failures, x pending\n",
            "1 example, 0 failures, -1 pending\n",
        ):
            with self.subTest(output=output):
                self.set_scenario(output=output, exit=0)
                result = self.run_match("Cart")
                self.assertEqual(result.returncode, 1)
                self.assertTrue("summary" in result.stdout, result.stdout)

    def test_timeout_kills_child_and_records_failure(self) -> None:
        self.set_scenario(sleep=5, output="1 example, 0 failures\n", exit=0)
        result = self.run_match("--timeout", "0.5", "Cart", timeout=8)
        self.assertEqual(result.returncode, 1)
        self.assertIn("timed out", result.stdout)
        transcript = self.root / ".agents-state/tdd/test-match"
        receipts = list(transcript.glob("run-*.log"))
        self.assertEqual(len(receipts), 1)
        self.assertIn("outcome: timed-out", receipts[0].read_text())

    def test_assertion_output_followed_by_timeout_has_a_timeout_outcome(self) -> None:
        self.set_scenario(
            sleep=5,
            output=(
                "Failures:\\n  1) component, rejects stale revision\\n"
                "       EXPECTED_REVISION\\n  To rerun use: --match revision\\n\\n"
                "1 example, 1 failure\\n"
            ),
            exit=1,
        )
        result = self.run_match("--timeout", "0.5", "rejects stale revision", timeout=8)
        self.assertEqual(result.returncode, 1)
        self.assertIn("timed out", result.stdout)
        transcript = self.root / ".agents-state/tdd/test-match"
        receipt = next(transcript.glob("run-*.log"))
        self.assertIn("outcome: timed-out", receipt.read_text())
        self.assertIn("1 example, 1 failure", receipt.read_text())

    def test_suite_cannot_be_used_as_an_option_or_shell_fragment(self) -> None:
        result = self.run_match("Cart", "not a cabal target")
        self.assertEqual(result.returncode, 2)
        self.assertIn("one Cabal target token", result.stderr)
        self.assertFalse(self.log.exists())

    def test_nonfinite_timeout_is_rejected_before_starting_the_child(self) -> None:
        for value in ("nan", "NaN", "inf"):
            with self.subTest(value=value):
                result = self.run_match("--timeout", value, "Cart")
                self.assertEqual(result.returncode, 2)
                self.assertFalse(self.log.exists())


def shlex_quote(value: str) -> str:
    return "'" + value.replace("'", "'\\''") + "'"


if __name__ == "__main__":
    unittest.main(verbosity=2)
