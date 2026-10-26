from __future__ import annotations

import contextlib
import importlib.machinery
import importlib.util
import io
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import time
import unittest
from unittest.mock import patch

sys.dont_write_bytecode = True
SKILLS = Path(__file__).resolve().parents[2]
BEST_PRACTICES = SKILLS / "neohaskell-best-practices/scripts"
NEOHASKELL_TEST = SKILLS / "neohaskell-test/scripts"
TDD = SKILLS / "tdd/scripts"
REGRESSION_EVIDENCE = SKILLS / "regression-evidence/scripts"
VALIDATE = SKILLS / "validate-skills/scripts"
ROOT = Path(__file__).resolve()
while not (ROOT / ".git").exists():
    if ROOT.parent == ROOT:
        raise RuntimeError("Tests need a repository")
    ROOT = ROOT.parent
FIXTURES = ROOT / ".agents-state/execution-contract-tests"
FIXTURES.mkdir(parents=True, exist_ok=True)
sys.path[:0] = [str(BEST_PRACTICES), str(NEOHASKELL_TEST), str(TDD), str(REGRESSION_EVIDENCE), str(VALIDATE)]


def load(name: str, path: Path):
    loader = importlib.machinery.SourceFileLoader(name, str(path))
    spec = importlib.util.spec_from_loader(name, loader)
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    loader.exec_module(module)
    return module


runtime = load("execution_contract_runtime", NEOHASKELL_TEST / "test_runtime.py")
doctest = load("execution_contract_doctest", NEOHASKELL_TEST / "run-doctest")
match = load("execution_contract_match", TDD / "test-match")
evidence = load("execution_contract_evidence", REGRESSION_EVIDENCE / "evidence.py")
validator = load("execution_contract_validator", VALIDATE / "check.py")


class CorrectnessRegressionTests(unittest.TestCase):
    def setUp(self) -> None:
        temporary = tempfile.TemporaryDirectory(dir=FIXTURES)
        self.addCleanup(temporary.cleanup)
        self.root = Path(temporary.name)

    def test_timeout_receipt_cannot_satisfy_an_expected_assertion_failure(self) -> None:
        root = self.root / "runtime-evidence"
        script = root / ".agents/skills/tdd/scripts/test-match"
        wrapper = root / ".agents/skills/neohaskell-best-practices/scripts/with-toolchain"
        root_helper = wrapper.with_name("repo-root")
        script.parent.mkdir(parents=True)
        wrapper.parent.mkdir(parents=True)
        (root / "cabal.project").write_text("packages: core/*.cabal\n")
        root_helper.write_text("#!/bin/sh\nprintf '%s\\n' " + repr(str(root)) + "\n")
        root_helper.chmod(0o755)
        script.write_bytes((TDD / "test-match").read_bytes())
        script.chmod(0o755)
        (wrapper.parent / "bounded_process.py").write_bytes((BEST_PRACTICES / "bounded_process.py").read_bytes())
        wrapper.write_text(
            "#!" + sys.executable + "\n"
            "import sys,time\n"
            "print('Failures:\\n  1) component, rejects stale revision\\n"
            "      EXPECTED_REVISION\\n  To rerun use: --match revision\\n\\n"
            "1 example, 1 failure', flush=True)\n"
            "time.sleep(5)\n"
        )
        wrapper.chmod(0o755)
        result = evidence.run(
            [str(script), "--timeout", "1", "rejects stale revision", "fixture-suite"],
            root,
            self.root / "runtime.log",
            4,
            {**os.environ, "NEOHASKELL_ROOT": str(root)},
        )
        self.assertEqual(result.returncode, 1)
        self.assertEqual(result.process_outcome, "exited")
        self.assertEqual(result.runner_outcome, "timed-out")
        self.assertTrue(result.requires_runner_receipt)
        self.assertIsNotNone(result.runner_receipt_sha256)
        self.assertIn("1 example, 1 failure", result.text)
        with self.assertRaisesRegex(ValueError, "not runtime assertion evidence"):
            evidence.classify_runtime(
                result.returncode,
                result.text,
                "fail",
                "rejects stale revision",
                "EXPECTED_REVISION",
                runner_outcome=result.runner_outcome,
                process_outcome=result.process_outcome,
                requires_runner_receipt=result.requires_runner_receipt,
            )

    def test_outer_timeout_stops_the_nested_owned_process_group(self) -> None:
        directory = self.root
        heartbeat = directory / "heartbeat"
        child_pid = directory / "child-pid"
        group_id = directory / "group-id"
        child_code = (
            "import os,signal,time; from pathlib import Path; "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); os.close(1); os.close(2); "
            "Path(" + repr(str(child_pid)) + ").write_text(str(os.getpid())); "
            "Path(" + repr(str(group_id)) + ").write_text(str(os.getpgrp())); "
            "p=Path(" + repr(str(heartbeat)) + "); "
            "i=0\nwhile True: p.write_text(str(i)); i+=1; time.sleep(0.02)"
        )
        nested_runner = directory / "nested_runner.py"
        nested_runner.write_text(
            "import os,sys; from pathlib import Path; sys.dont_write_bytecode=True; sys.path.insert(0,"
            + repr(str(NEOHASKELL_TEST))
            + "); from test_runtime import run_bounded; "
            "run_bounded([sys.executable,\"-c\","
            + repr(child_code)
            + "],cwd=Path("
            + repr(str(directory))
            + "),env=os.environ.copy(),timeout=15)\n"
        )
        result = None
        group = None
        try:
            result = runtime.run_bounded(
                [sys.executable, str(nested_runner)],
                cwd=directory,
                env=os.environ.copy(),
                timeout=0.8,
            )
            group = int(group_id.read_text())
            child = int(child_pid.read_text())
            before = heartbeat.read_text()
            time.sleep(0.15)
            self.assertEqual(heartbeat.read_text(), before)
            self.assert_process_gone(child)
            self.assertTrue(result.timed_out)
        finally:
            if group is not None:
                try:
                    os.killpg(group, signal.SIGKILL)
                except ProcessLookupError:
                    pass

    def test_owner_kills_descendant_after_direct_child_exits_and_output_closes(self) -> None:
        heartbeat = self.root / "orphan-heartbeat"
        child_pid = self.root / "orphan-pid"
        group_id = self.root / "orphan-group"
        ready = self.root / "orphan-ready"
        grandchild = (
            "import os,signal,time; from pathlib import Path; "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); os.close(1); os.close(2); "
            f"Path({str(child_pid)!r}).write_text(str(os.getpid())); "
            f"Path({str(group_id)!r}).write_text(str(os.getpgrp())); "
            f"ready=Path({str(ready)!r}); beat=Path({str(heartbeat)!r}); ready.write_text('ready'); "
            "index=0\nwhile True: beat.write_text(str(index)); index+=1; time.sleep(0.02)"
        )
        launcher = (
            "import subprocess,sys,time; from pathlib import Path; "
            f"subprocess.Popen([sys.executable, '-c', {grandchild!r}], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL); "
            f"ready=Path({str(ready)!r}); deadline=time.monotonic()+2\n"
            "while not ready.exists() and time.monotonic() < deadline: time.sleep(0.01)\n"
            "assert ready.exists()\nprint('launcher exited')"
        )
        group = None
        try:
            result = runtime.run_bounded(
                [sys.executable, "-c", launcher],
                cwd=self.root,
                env=os.environ.copy(),
                timeout=4,
            )
            group = int(group_id.read_text())
            child = int(child_pid.read_text())
            self.assertEqual(result.returncode, 0)
            self.assertIn("launcher exited", result.output.decode())
            before = heartbeat.read_text()
            time.sleep(0.15)
            self.assertEqual(heartbeat.read_text(), before)
            self.assert_process_gone(child)
        finally:
            if group is not None:
                try:
                    os.killpg(group, signal.SIGKILL)
                except ProcessLookupError:
                    pass

    def test_signal_cancellation_reaps_nested_runner_and_stops_heartbeat(self) -> None:
        heartbeat = self.root / "cancel-heartbeat"
        child_pid = self.root / "cancel-child-pid"
        group_id = self.root / "cancel-group"
        runner_pid = self.root / "cancel-runner-pid"
        child_code = (
            "import os,signal,time; from pathlib import Path; "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); os.close(1); os.close(2); "
            f"Path({str(child_pid)!r}).write_text(str(os.getpid())); "
            f"Path({str(group_id)!r}).write_text(str(os.getpgrp())); "
            f"beat=Path({str(heartbeat)!r}); index=0\n"
            "while True: beat.write_text(str(index)); index+=1; time.sleep(0.02)"
        )
        nested_runner = self.root / "cancel_nested_runner.py"
        nested_runner.write_text(
            "import os,sys; from pathlib import Path; "
            f"Path({str(runner_pid)!r}).write_text(str(os.getpid())); "
            "sys.dont_write_bytecode=True; sys.path.insert(0,"
            + repr(str(NEOHASKELL_TEST))
            + "); from test_runtime import run_bounded; "
            "run_bounded([sys.executable,\"-c\","
            + repr(child_code)
            + "],cwd=Path("
            + repr(str(self.root))
            + "),env=os.environ.copy(),timeout=20)\n"
        )
        outer_runner = self.root / "cancel_outer_runner.py"
        outer_runner.write_text(
            "import os,sys; from pathlib import Path; sys.dont_write_bytecode=True; sys.path.insert(0,"
            + repr(str(NEOHASKELL_TEST))
            + "); from test_runtime import run_bounded; "
            "run_bounded([sys.executable,"
            + repr(str(nested_runner))
            + "],cwd=Path("
            + repr(str(self.root))
            + "),env=os.environ.copy(),timeout=20)\n"
        )
        outer = subprocess.Popen(
            [sys.executable, str(outer_runner)],
            cwd=self.root,
            env=os.environ.copy(),
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            start_new_session=True,
        )
        group = None
        try:
            deadline = time.monotonic() + 5
            while not heartbeat.exists() and outer.poll() is None and time.monotonic() < deadline:
                time.sleep(0.02)
            self.assertTrue(heartbeat.exists(), "nested child did not start before cancellation")
            group = int(group_id.read_text())
            child = int(child_pid.read_text())
            os.kill(outer.pid, signal.SIGTERM)
            self.assertEqual(outer.wait(timeout=5), -signal.SIGTERM)
            runner = int(runner_pid.read_text())
            self.assert_process_gone(runner)
            self.assert_process_gone(child)
            before = heartbeat.read_text()
            time.sleep(0.15)
            self.assertEqual(heartbeat.read_text(), before)
        finally:
            if outer.poll() is None:
                try:
                    os.killpg(outer.pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
                outer.wait()
            if group is not None:
                try:
                    os.killpg(group, signal.SIGKILL)
                except ProcessLookupError:
                    pass

    def test_signal_during_launch_waits_for_child_group_cleanup(self) -> None:
        import bounded_process

        heartbeat = self.root / "launch-heartbeat"
        ready = self.root / "launch-ready"
        pid_file = self.root / "launch-child-pid"
        code = (
            "import os,signal,time; from pathlib import Path; "
            "signal.signal(signal.SIGTERM, signal.SIG_IGN); os.close(1); os.close(2); "
            f"Path({str(pid_file)!r}).write_text(str(os.getpid())); "
            f"Path({str(ready)!r}).write_text('ready'); beat=Path({str(heartbeat)!r}); index=0\n"
            "while True: beat.write_text(str(index)); index+=1; time.sleep(0.02)"
        )
        original_popen = bounded_process.subprocess.Popen
        launched = {}

        def launch(*args, **kwargs):
            os.kill(os.getpid(), signal.SIGTERM)
            process = original_popen(*args, **kwargs)
            launched["process"] = process
            deadline = time.monotonic() + 3
            while not ready.exists() and time.monotonic() < deadline:
                time.sleep(0.01)
            return process

        previous_handler = signal.getsignal(signal.SIGTERM)
        observed_signals = []
        signal.signal(signal.SIGTERM, lambda signum, frame: observed_signals.append(signum))
        try:
            with patch.object(bounded_process.subprocess, "Popen", side_effect=launch):
                with self.assertRaises(bounded_process.ProcessCancelled):
                    bounded_process.run_bounded(
                        [sys.executable, "-c", code],
                        cwd=self.root,
                        env=os.environ.copy(),
                        timeout=5,
                    )
        finally:
            signal.signal(signal.SIGTERM, previous_handler)
        self.assertTrue(ready.exists())
        self.assertEqual(observed_signals, [signal.SIGTERM])
        self.assertIsNotNone(launched["process"].returncode)
        before = heartbeat.read_text()
        time.sleep(0.15)
        self.assertEqual(heartbeat.read_text(), before)

    def assert_process_gone(self, pid: int) -> None:
        deadline = time.monotonic() + 2
        while time.monotonic() < deadline:
            try:
                os.kill(pid, 0)
            except ProcessLookupError:
                return
            time.sleep(0.02)
        self.fail(f"owned fixture process {pid} remained after bounded cleanup")

    def test_strict_doctest_requires_one_positive_executed_summary(self) -> None:
        package = self.root / "pkg"
        binary = self.root / "bin"
        package.mkdir()
        binary.mkdir()
        (self.root / "cabal.project").write_text("packages: pkg/*.cabal\n")
        (package / "example.cabal").write_text(
            "name: example\nlibrary\n  hs-source-dirs: .\n  default-language: Haskell2010\n"
        )
        (package / "Example.hs").write_text('module Example where\ntoken = ">>>"\n')
        self.write_executable(binary / "ghc", 'print("/fixture/lib")')
        self.write_executable(
            binary / "doctest",
            'import os; print(os.environ["DOCTEST_SUMMARY"])',
        )
        environment = {**os.environ, "PATH": str(binary)}
        cases = [
            ("Examples: 0  Tried: 0  Errors: 0  Failures: 0", 1, "INCOMPLETE"),
            ("tool ran without a summary", 1, "FAIL"),
            (
                "Examples: 1  Tried: 1  Errors: 0  Failures: 0\n"
                "Examples: 1  Tried: 1  Errors: 0  Failures: 0",
                1,
                "FAIL",
            ),
            ("Examples: 1  Tried: 0  Errors: 0  Failures: 0", 1, "INCOMPLETE"),
            ("Examples: 2  Tried: 2  Errors: 0  Failures: 0", 0, "PASS"),
        ]
        for summary, expected_code, expected_status in cases:
            with self.subTest(summary=summary), patch.dict(os.environ, environment):
                os.environ["DOCTEST_SUMMARY"] = summary
                captured = io.StringIO()
                with contextlib.redirect_stdout(captured):
                    code = doctest.run_inside(self.root, True, 3)
                self.assertEqual(code, expected_code, captured.getvalue())
                self.assertIn(f"doctest: {expected_status}", captured.getvalue())

    def test_yaml_nonstring_plain_scalars_are_rejected_and_quoted_literals_pass(self) -> None:
        for raw in (
            "null",
            "NULL",
            "~",
            "true",
            "FALSE",
            "yes",
            "OFF",
            "0",
            "-12",
            "1.5",
            "1e3",
            "0x10",
            "0o7",
            "0b1",
            ".inf",
            ".NaN",
            "2025-03-08",
            "2025-03-08T11:22:33Z",
        ):
            with self.subTest(raw=raw):
                self.write_skill("description: " + raw)
                self.assertTrue(validator.validate(self.root)["errors"])
        for raw in ('"null"', "'true'", '"123"'):
            with self.subTest(raw=raw):
                self.write_skill("description: " + raw)
                self.assertEqual(validator.validate(self.root)["errors"], [])

    def write_executable(self, path: Path, body: str) -> None:
        path.write_text("#!" + sys.executable + "\n" + body + "\n")
        path.chmod(0o755)

    def write_skill(self, description: str) -> None:
        path = self.root / ".agents/skills/example/SKILL.md"
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text("---\nname: example\n" + description + "\n---\n")


if __name__ == "__main__":
    unittest.main(verbosity=2)
