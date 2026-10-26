from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path

sys.dont_write_bytecode = True
SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))

from bounded_process import ProcessOutcome, run_bounded


ROOT = Path(__file__).resolve()
while not (ROOT / ".git").exists():
    if ROOT.parent == ROOT:
        raise RuntimeError("Tests need a repository")
    ROOT = ROOT.parent
FIXTURES = ROOT / ".agents-state/bounded-process-tests"
FIXTURES.mkdir(parents=True, exist_ok=True)


class BoundedProcessTests(unittest.TestCase):
    def test_output_limit_is_explicit_and_capped(self) -> None:
        with tempfile.TemporaryDirectory(dir=FIXTURES) as directory:
            result = run_bounded(
                [sys.executable, "-c", "import sys; sys.stdout.write('x' * 4096)"],
                cwd=Path(directory),
                timeout=3,
                max_output_bytes=128,
            )
        self.assertIs(result.outcome, ProcessOutcome.OUTPUT_LIMITED)
        self.assertLessEqual(len(result.output), 128)
        self.assertIsNotNone(result.returncode)

    def test_timeout_is_explicit_and_reaps_the_direct_child(self) -> None:
        with tempfile.TemporaryDirectory(dir=FIXTURES) as directory:
            result = run_bounded(
                [sys.executable, "-c", "import time; time.sleep(10)"],
                cwd=Path(directory),
                timeout=0.1,
            )
        self.assertIs(result.outcome, ProcessOutcome.TIMED_OUT)
        self.assertIsNotNone(result.returncode)

    def test_invalid_bounds_fail_before_child_execution(self) -> None:
        with tempfile.TemporaryDirectory(dir=FIXTURES) as directory:
            for timeout, cap in ((0, 128), (float("inf"), 128), (1, 0)):
                with self.subTest(timeout=timeout, cap=cap), self.assertRaises(ValueError):
                    run_bounded(
                        [sys.executable, "-c", "raise SystemExit('must not run')"],
                        cwd=Path(directory),
                        timeout=timeout,
                        max_output_bytes=cap,
                    )


if __name__ == "__main__":
    unittest.main(verbosity=2)
