from __future__ import annotations

import json
import os
import shutil
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path
from unittest.mock import patch

sys.dont_write_bytecode = True
REPOSITORY = next(path for path in Path(__file__).resolve().parents if (path / ".git").exists())


SCRIPTS = Path(__file__).resolve().parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))

from test_runtime import cabal_packages, cabal_test_targets, dependency_source_dirs, run_bounded, run_testbed


class TestRuntimeTests(unittest.TestCase):
    def test_nonfinite_timeouts_are_rejected_before_toolchain_entry(self) -> None:
        for script in ("run-doctest", "run-full-suite", "run-testbed"):
            for value in ("nan", "inf"):
                with self.subTest(script=script, value=value):
                    result = subprocess.run([sys.executable, str(SCRIPTS / script), "--timeout", value], capture_output=True, text=True, timeout=5)
                    self.assertEqual(result.returncode, 2)
                    self.assertIn("timeout must", result.stderr)

    def test_cabal_project_discovers_the_declared_local_packages(self) -> None:
        packages = cabal_packages(REPOSITORY)
        names = {package.name for package in packages}
        self.assertEqual(names, {"nhcore", "nhintegrations", "nhtestbed"})
        self.assertTrue(all(package.cabal_file.is_file() for package in packages))

    def test_cabal_test_targets_match_current_component_declarations(self) -> None:
        targets = set(cabal_test_targets(REPOSITORY))
        self.assertIn(("nhcore", "nhcore-test-core"), targets)
        self.assertIn(("nhcore", "nhcore-test-service"), targets)
        self.assertIn(("nhcore", "nhcore-test-integration"), targets)
        self.assertIn(("nhintegrations", "nhintegrations-test"), targets)

    def test_package_source_roots_include_local_cabal_dependencies(self) -> None:
        packages = cabal_packages(REPOSITORY)
        integrations = next(package for package in packages if package.name == "nhintegrations")
        core = next(package for package in packages if package.name == "nhcore")
        self.assertIn(core.source_dirs[0], dependency_source_dirs(integrations, packages))
        self.assertIn("GHC2021", core.language or "")
        self.assertIn("NoImplicitPrelude", core.extensions)

    def test_bounded_process_captures_output(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "print('finished')"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=3,
        )
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.output.decode().strip(), "finished")
        self.assertFalse(result.timed_out)

    def test_bounded_process_kills_timed_out_child(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "import time; time.sleep(10)"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=0.1,
        )
        self.assertTrue(result.timed_out)
        self.assertIsNotNone(result.returncode)

    def test_bounded_process_stops_when_output_exceeds_the_limit(self) -> None:
        result = run_bounded(
            [sys.executable, "-c", "import sys; sys.stdout.write('x' * 4096)"],
            cwd=REPOSITORY,
            env=os.environ.copy(),
            timeout=3,
            max_output_bytes=128,
        )
        self.assertTrue(result.output_limited)
        self.assertLessEqual(len(result.output), 128)

    def test_missing_testbed_tools_are_reported_as_a_skip(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            with patch.dict(os.environ, {"PATH": ""}):
                status, reason, result = run_testbed(Path(directory), os.environ.copy(), 1)
        self.assertEqual(status, "SKIP")
        self.assertIn("missing prerequisite", reason)
        self.assertIsNone(result)

    def test_missing_curl_is_reported_as_a_testbed_skip(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tools = Path(directory)
            for name in ("hurl", "pg_isready", "bash"):
                executable = tools / name
                executable.write_text("#!/bin/sh\nexit 0\n")
                executable.chmod(0o755)
            env = os.environ.copy()
            env["PATH"] = str(tools)
            with patch.dict(os.environ, {"PATH": str(tools)}):
                status, reason, result = run_testbed(Path(directory), env, 1)
        self.assertEqual(status, "SKIP")
        self.assertIn("curl", reason)
        self.assertIsNone(result)

    def test_testbed_skip_is_nonfatal_by_default_and_fails_in_strict_mode(self) -> None:
        with tempfile.TemporaryDirectory() as directory:
            tools = Path(directory)
            bash = shutil.which("bash")
            dirname = shutil.which("dirname")
            self.assertIsNotNone(bash)
            self.assertIsNotNone(dirname)
            (tools / "bash").symlink_to(bash)
            (tools / "dirname").symlink_to(dirname)
            env = os.environ.copy()
            env["PATH"] = str(tools)
            env["NEOHASKELL_ROOT"] = str(REPOSITORY)
            script = SCRIPTS / "run-testbed"
            normal = subprocess.run(
                [sys.executable, str(script), "--_inside-toolchain"],
                cwd=REPOSITORY,
                env=env,
                text=True,
                capture_output=True,
                check=False,
                timeout=5,
            )
            strict = subprocess.run(
                [sys.executable, str(script), "--_inside-toolchain", "--strict"],
                cwd=REPOSITORY,
                env=env,
                text=True,
                capture_output=True,
                check=False,
                timeout=5,
            )
        self.assertEqual(normal.returncode, 0, normal.stderr)
        self.assertIn("testbed: SKIP", normal.stdout)
        self.assertEqual(strict.returncode, 1, strict.stderr)
        self.assertIn("testbed: INCOMPLETE", strict.stdout)


class FullSuiteTests(unittest.TestCase):
    def run_full_suite(self, statuses=None, cabal_exit=0, strict=True, testbed_exit=0):
        with tempfile.TemporaryDirectory(prefix="nh-full-suite-") as directory:
            root = Path(directory)
            shutil.copy2(REPOSITORY / "cabal.project", root / "cabal.project")
            for package in cabal_packages(REPOSITORY):
                destination = root / package.cabal_file.relative_to(REPOSITORY)
                destination.parent.mkdir(parents=True, exist_ok=True)
                shutil.copy2(package.cabal_file, destination)
            (root / "nix").mkdir()
            (root / "nix/hix.nix").touch()
            binaries = root / "bin"
            binaries.mkdir()
            for name in ("bash", "dirname"):
                (binaries / name).symlink_to(shutil.which(name))
            log = root / "commands.jsonl"
            fixture = binaries / "fixture"
            fixture.write_text(
                "#!" + sys.executable + "\n"
                "import json, os, pathlib, sys\n"
                "name = pathlib.Path(sys.argv[0]).name\n"
                "with open(os.environ['FIXTURE_LOG'], 'a') as output:\n"
                "    output.write(json.dumps([name, *sys.argv[1:]]) + '\\n')\n"
                "if name == 'cabal':\n"
                "    print('owned per-example output')\n"
                "    for suite, status in json.loads(os.environ['SUITE_RESULTS']).items():\n"
                "        print(f'Test suite {suite}: {status}')\n"
                "    sys.exit(int(os.environ['CABAL_EXIT']))\n"
                "if name == 'hurl':\n"
                "    sys.exit(int(os.environ['TESTBED_EXIT']))\n"
            )
            fixture.chmod(0o755)
            for name in ("cabal", "pg_isready", "hurl", "curl"):
                (binaries / name).symlink_to(fixture.name)
            runner = root / "testbed/scripts/run-tests.sh"
            runner.parent.mkdir(parents=True, exist_ok=True)
            runner.write_text("#!/bin/sh\nexec hurl owned-testbed.hurl\n")
            environment = dict(os.environ)
            results = {target: "PASS" for _, target in cabal_test_targets(REPOSITORY)}
            results.update(statuses or {})
            environment.update(
                PATH=str(binaries), NEOHASKELL_ROOT=str(root), FIXTURE_LOG=str(log),
                SUITE_RESULTS=json.dumps({key: value for key, value in results.items() if value}),
                CABAL_EXIT=str(cabal_exit), TESTBED_EXIT=str(testbed_exit),
            )
            command = [sys.executable, str(SCRIPTS / "run-full-suite"), "--_inside-toolchain"]
            if strict:
                command.append("--strict")
            result = subprocess.run(command, cwd=root, env=environment, text=True,
                                    capture_output=True, check=False, timeout=10)
            calls = [json.loads(line) for line in log.read_text().splitlines()]
            self.assertEqual([call for call in calls if call[0] == "cabal"], [
                ["cabal", "test", "all", "--project-file=cabal.project", "--test-show-details=direct"]
            ])
            self.assertIn(["hurl", "owned-testbed.hurl"], calls)
            self.assertIn("owned per-example output", result.stdout)
            return result

    def test_whole_integrations_suite_is_executed(self):
        result = self.run_full_suite()
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        for package, target in cabal_test_targets(REPOSITORY):
            self.assertEqual(result.stdout.count(f"full-suite: PASS Cabal target {package}:{target}\n"), 1)
        self.assertIn("full-suite: PASS testbed acceptance", result.stdout)

    def test_integrations_failure_is_not_skipped(self):
        for suite in ("nhintegrations-test", "nhcore-test-core", "nhcore-test-service"):
            for exit_code in (0, 37):
                with self.subTest(suite=suite, exit_code=exit_code):
                    result = self.run_full_suite({suite: "FAIL"}, cabal_exit=exit_code)
                    self.assertEqual(result.returncode, 1, result.stdout + result.stderr)
                    self.assertIn("full-suite: FAIL:", result.stdout)

    def test_missing_wrong_or_skipped_integrations_result_is_incomplete(self):
        for status in (None, "SKIP"):
            for strict in (False, True):
                with self.subTest(status=status, strict=strict):
                    result = self.run_full_suite(
                        {"nhintegrations-test": status, "wrong-integrations-test": "PASS"}, strict=strict
                    )
                    self.assertEqual(result.returncode, 1 if strict else 0, result.stdout + result.stderr)
                    self.assertIn("full-suite: INCOMPLETE", result.stdout)
                    self.assertIn("SKIP Cabal target nhintegrations:nhintegrations-test", result.stdout)

    def test_cabal_and_testbed_failures_remain_fatal(self):
        for cabal_exit, testbed_exit in ((37, 0), (0, 37)):
            with self.subTest(cabal_exit=cabal_exit, testbed_exit=testbed_exit):
                result = self.run_full_suite(cabal_exit=cabal_exit, testbed_exit=testbed_exit)
                self.assertEqual(result.returncode, 1, result.stdout + result.stderr)


if __name__ == "__main__":
    unittest.main(verbosity=2)
